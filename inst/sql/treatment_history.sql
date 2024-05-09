/*
Step 1: Join target and event cohorts
*/
DROP TABLE IF EXISTS #raw_events;

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
ON a.cohort_start_date <= b.cohort_start_date AND b.cohort_start_date <= a.cohort_end_date AND a.subject_id = b.subject_id
ORDER BY subject_id
;

/*
Step 2: collapse eras that are less than @gap_days
*/

DROP TABLE IF EXISTS #collapse_events;

WITH marked_dates AS(
  SELECT
  ROW_NUMBER() OVER (PARTITION BY subject_id ORDER BY event_start, event_end) AS ordinal,
  subject_id, event_id, event_start, event_end,
  CASE WHEN(DATEDIFF(day, LAG(event_end) OVER (PARTITION BY subject_id, event_id ORDER BY event_start, event_end), event_start) <= @gap_days) THEN 1 ELSE 0 END to_collapse
  FROM #raw_events
),
grouped_dates AS (
SELECT ordinal, subject_id, event_id, event_start, event_end, to_collapse, ordinal - SUM(to_collapse) OVER ( PARTITION BY subject_id ORDER BY event_start, event_end ROWS UNBOUNDED PRECEDING) group_idx
FROM marked_dates
),
replacements AS (
SELECT ordinal, subject_id, event_id, event_start, event_end, to_collapse, group_idx, FIRST_VALUE(event_start) OVER (PARTITION BY subject_id, group_idx ORDER BY ordinal ASC ROWS UNBOUNDED PRECEDING) as replacement_date
FROM grouped_dates
),
update_dates AS(
  SELECT subject_id, event_id, event_start,
    CASE WHEN to_collapse = 1 THEN replacement_date ELSE event_start END AS new_event_start, event_end,
    ROW_NUMBER() OVER (PARTITION BY subject_id, group_idx ORDER BY to_collapse DESC) AS d
  FROM replacements
)
SELECT subject_id, event_id, new_event_start AS cohort_start_date, event_end AS cohort_end_date
  INTO #collapse_events
  FROM update_dates
WHERE d = 1
;
/* Check
SELECT * FROM #collapse_events WHERE subject_id = 92706486
*/

DROP TABLE IF EXISTS #event_cohort_eras;

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

/* Check
SELECT * FROM #event_cohort_eras WHERE subject_id = 92706486
*/

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
  CAST(e.cohort_end_date AS date) AS event_end
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

TRUNCATE TABLE #raw_events;
DROP TABLE #raw_events;

TRUNCATE TABLE #combo_events;
DROP TABLE #combo_events;
