DROP TABLE IF EXISTS #raw_events;

-- create target event cohort
SELECT
  a.subject_id AS subject_id,
  a.cohort_definition_id AS target_id,
  b.cohort_definition_id AS event_id,
  a.cohort_start_date AS target_start,
  a.cohort_end_date AS target_end,
  b.cohort_start_date AS event_start,
  dateadd(d, 1, b.cohort_end_date) AS event_end
INTO #raw_events
FROM (
  SELECT * FROM @work_database_schema.@cohort_table
  WHERE cohort_definition_id IN (@target_cohort_id)
) a
JOIN (
  SELECT * FROM @work_database_schema.@cohort_table
  WHERE cohort_definition_id IN (@event_cohort_id)
) b
ON (a.cohort_start_date < b.cohort_end_date OR a.cohort_start_date = b.cohort_start_date)
  AND b.cohort_start_date <= a.cohort_end_date
  AND a.subject_id = b.subject_id
ORDER BY subject_id, event_start
;

/*
* Find closely located dates, which need to be collapsed, based on combo_window
*/

DROP TABLE IF EXISTS #collapse_events;

WITH person_dates AS (
SELECT subject_id, event_start AS cohort_date FROM #raw_events
UNION
SELECT subject_id, event_end AS cohort_date FROM #raw_events
),
marked_dates AS (
SELECT ROW_NUMBER() OVER (ORDER BY subject_id ASC, cohort_date ASC) ordinal,
    subject_id,
    cohort_date,
    CASE WHEN (datediff(d,LAG(cohort_date) OVER (ORDER BY subject_id ASC, cohort_date ASC), cohort_date) <= @gap_days AND subject_id = LAG(subject_id) OVER (ORDER BY subject_id ASC, cohort_date ASC)) THEN 1 ELSE 0 END to_be_collapsed
  FROM person_dates
),
grouped_dates AS (
  SELECT ordinal, subject_id, cohort_date, to_be_collapsed, ordinal - SUM(to_be_collapsed) OVER ( PARTITION BY subject_id ORDER BY cohort_date ASC ROWS UNBOUNDED PRECEDING) group_idx
  FROM marked_dates
),
replacements AS (
  SELECT orig.subject_id, orig.cohort_date, FIRST_VALUE(cohort_date) OVER (PARTITION BY group_idx ORDER BY ordinal ASC ROWS UNBOUNDED PRECEDING) as replacement_date
  FROM grouped_dates orig
)
SELECT subject_id, cohort_date, replacement_date
INTO #date_replacements
FROM replacements
WHERE cohort_date <> replacement_date;


/*
* Collapse dates
*/

DROP TABLE IF EXISTS #event_cohort_eras;

SELECT
  e.subject_id,
  e.event_id,
  e.cohort_start_date,
  case
  /*
  The collapsed dates (or the raw event cohort dates) may have intervals where start == end, so these should be expanded to cover a minimum of 1 day
  */
    when e.cohort_start_date = e.cohort_end_date then CAST(dateadd(d,1,e.cohort_end_date) AS DATETIME) /* cast is required for BigQuery */
    else e.cohort_end_date
  end cohort_end_date
INTO #collapse_events
FROM (
  SELECT
    event.event_id,
    event.subject_id,
    COALESCE(start_dr.replacement_date, event.event_start) cohort_start_date,
    COALESCE(end_dr.replacement_date, event.event_end) cohort_end_date
  FROM #raw_events event
  LEFT JOIN #date_replacements start_dr ON start_dr.subject_id = event.subject_id AND start_dr.cohort_date = event.event_start
  LEFT JOIN #date_replacements end_dr ON end_dr.subject_id = event.subject_id AND end_dr.cohort_date = event.event_end
) e
;


-- we need to era-fy the collapsed dates because collapsing leads to overlapping.

with cteEndDates (SUBJECT_ID, EVENT_ID, END_DATE) as -- the magic: identify the end of eras, paritioned by the GAP_GROUP and the person_id
(
	select SUBJECT_ID, EVENT_ID, EVENT_DATE as END_DATE -- unpad the end date
	FROM
	(
		select SUBJECT_ID, EVENT_ID, EVENT_DATE, EVENT_TYPE,
		MAX(START_ORDINAL) OVER (PARTITION BY SUBJECT_ID, EVENT_ID ORDER BY EVENT_DATE, EVENT_TYPE ROWS UNBOUNDED PRECEDING) as START_ORDINAL, -- this pulls the current START down from the prior rows so that the NULLs from the END DATES will contain a value we can compare with
		ROW_NUMBER() OVER (PARTITION BY SUBJECT_ID, EVENT_ID ORDER BY EVENT_DATE, EVENT_TYPE) AS OVERALL_ORD -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
		from
		(
			-- select the start dates, assigning a row number to each
			Select SUBJECT_ID, EVENT_ID, COHORT_START_DATE AS EVENT_DATE, 1 as EVENT_TYPE, ROW_NUMBER() OVER (PARTITION BY SUBJECT_ID, EVENT_ID ORDER BY COHORT_START_DATE) as START_ORDINAL
			from #collapse_events

			UNION ALL

			-- pad the end dates by 30 to allow a grace period for overlapping ranges.
			select SUBJECT_ID, EVENT_ID, COHORT_END_DATE, -1 as EVENT_TYPE, NULL
			FROM #collapse_events
		) RAWDATA
	) E
	WHERE (2 * E.START_ORDINAL) - E.OVERALL_ORD = 0
)
,cteEpisodeEnds (SUBJECT_ID, EVENT_ID, COHORT_START_DATE, ERA_END_DATE) as
(
	select
		re.SUBJECT_ID,
		re.EVENT_ID,
		re.COHORT_START_DATE,
		MIN(ed.END_DATE) as ERA_END_DATE
	FROM #collapse_events re
	JOIN cteEndDates ed on re.SUBJECT_ID = ed.SUBJECT_ID and re.EVENT_ID = ed.EVENT_ID and ed.END_DATE >= re.COHORT_START_DATE
	GROUP BY
		re.SUBJECT_ID,
		re.EVENT_ID,
		re.COHORT_START_DATE
)
,cteFinalEras(SUBJECT_ID, EVENT_ID, COHORT_START_DATE, COHORT_END_DATE) as
(
  select SUBJECT_ID, EVENT_ID, min(COHORT_START_DATE) as COHORT_START_DATE, ERA_END_DATE as COHORT_END_DATE
	from cteEpisodeEnds e
	group by SUBJECT_ID, EVENT_ID, ERA_END_DATE
)
select SUBJECT_ID, EVENT_ID, COHORT_START_DATE, COHORT_END_DATE
INTO #event_cohort_eras
from cteFinalEras;

/*
Split partially overlapping events into a set of events which either do not overlap or fully overlap (for later GROUP BY start_date, end_date)

e.g.
  |A------|
      |B-----|
into

  |A--|A--|
      |B--|B--|

or
  |A--------------|
      |B-----|
into
  |A--|A-----|A---|
      |B-----|
*/

DROP TABLE IF EXISTS #combo_events;


WITH
cohort_dates AS (
	SELECT DISTINCT subject_id, cohort_date
	FROM (
		  SELECT subject_id, cohort_start_date cohort_date FROM #event_cohort_eras
		  UNION
		  SELECT subject_id,cohort_end_date cohort_date FROM #event_cohort_eras
		  ) all_dates
),
time_periods AS (
	SELECT subject_id, cohort_date, LEAD(cohort_date,1) over (PARTITION BY subject_id ORDER BY cohort_date ASC) next_cohort_date
	FROM cohort_dates
	GROUP BY subject_id, cohort_date

),
events AS (
	SELECT tp.subject_id, event_id, cohort_date cohort_start_date, next_cohort_date cohort_end_date
	FROM time_periods tp
	LEFT JOIN #event_cohort_eras e ON e.subject_id = tp.subject_id
	WHERE (e.cohort_start_date <= tp.cohort_date AND e.cohort_end_date >= tp.next_cohort_date)
)
SELECT cast(SUM(e.event_id) as bigint) as combo_id,  subject_id , cohort_start_date, cohort_end_date
into #combo_events
FROM events e
GROUP BY subject_id, cohort_start_date, cohort_end_date;


/* Join back to target table and order */
DROP TABLE IF EXISTS @trt_history_table;

WITH T1 AS (
SELECT
  e.subject_id,
  t.cohort_definition_id,
  e.combo_id AS event_id,
  t.cohort_start_date AS target_start,
  t.cohort_end_date AS target_end,
  CAST(e.cohort_start_date AS date) AS event_start,
  CAST(e.cohort_end_date AS date) AS event_end,
  '@query_method' AS query_method
FROM #combo_events e
JOIN (
  SELECT * FROM @work_database_schema.@cohort_table
  WHERE cohort_definition_id IN (@target_cohort_id)
) t
ON t.cohort_start_date <= e.cohort_start_date AND e.cohort_start_date <= t.cohort_end_date AND t.subject_id = e.subject_id
)
SELECT *,
ROW_NUMBER() OVER(PARTITION BY subject_id ORDER BY subject_id, event_start, event_end) AS row_num
INTO @trt_history_table
FROM T1
;

/* Clean up */
TRUNCATE TABLE #collapse_events;
DROP TABLE #collapse_events;

TRUNCATE TABLE #event_cohort_eras;
DROP TABLE #event_cohort_eras;


TRUNCATE TABLE #date_replacements;
DROP TABLE #date_replacements;

TRUNCATE TABLE #raw_events;
DROP TABLE #raw_events;

TRUNCATE TABLE #combo_events;
DROP TABLE #combo_events;
