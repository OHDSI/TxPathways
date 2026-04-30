/*
TxPathways: Build Treatment History
------------------------------------
Parameters (via SqlRender::render):
  work_database_schema           Schema containing the cohort table
  cohort_table                   Cohort table name (unqualified)
  tx_history_table               Fully-qualified output table
  target_cohort_id               Cohort definition ID for the target cohort
  event_cohort_ids               Comma-separated list of event cohort IDs
  start_anchor                     Which date to offset for follow-up start: 'cohort_start_date' or 'cohort_end_date' (default 'cohort_start_date')
  start_days                       Days offset from target date for follow-up start (default 0)
  end_anchor                       Which date to offset for follow-up end: 'cohort_start_date' or 'cohort_end_date' (default 'cohort_end_date')
  end_days                         Days offset from target date for follow-up end (default 0)
  era_collapse_size              Days; gap <= this merges same-drug eras (default 30)
  min_era_duration               Days; collapsed eras shorter than this are dropped (default 0)
  combination_window             Days; min overlap to classify as combination (default 30)
  min_post_combination_duration  Days; min duration to retain a combination segment (default 30)
  max_path_length                Max treatment lines per person to retain (default 5)
  filter_treatments              Which lines to keep: 'All', 'First', 'Changes' (default 'All')
                                    All     - keep every treatment era
                                    First   - keep only the first occurrence of each drug per person
                                    Changes - deduplicate consecutive same-drug lines
Output columns:
  person_id, n_target, target_start_date, target_end_date,
  combo_label, event_start_date, event_end_date, duration_era, event_seq

*/

DROP TABLE IF EXISTS @work_database_schema.@tx_history_table;

WITH
/*
  target_cohort
  Enumerate each person's target cohort entries (index dates).
  n_target distinguishes multiple index dates per person.
  windowStart / windowEnd offset the effective follow-up window
  relative to cohort_start_date and cohort_end_date respectively.
*/
target_cohort AS (
    SELECT
        subject_id                                  AS person_id,
        cohort_start_date                           AS target_start_date,
        cohort_end_date                             AS target_end_date,
        DATEADD(day, @start_days, @start_anchor) AS window_start_date,
        DATEADD(day, @end_days,   @end_anchor)   AS window_end_date,
        ROW_NUMBER() OVER (
            PARTITION BY subject_id
            ORDER BY cohort_start_date
        )                                           AS n_target
    FROM @work_database_schema.@cohort_table
    WHERE cohort_definition_id = @target_cohort_id
),

/*
  STEP 0: Raw events
  Join event cohorts to the target cohort. Events must start within
  the effective follow-up window [window_start_date, window_end_date].
  Add 1 day to event_end per OMOP era convention.
  Event end is also clipped to window_end_date so no era extends
  beyond the person's follow-up.
*/
raw_events AS (
    SELECT
        t.person_id,
        t.n_target,
        e.cohort_definition_id                          AS event_cohort_id,
        e.cohort_start_date                             AS event_start_date,
        CASE
            WHEN DATEADD(day, 1, e.cohort_end_date) > t.window_end_date
            THEN t.window_end_date
            ELSE DATEADD(day, 1, e.cohort_end_date)
        END                                             AS event_end_date
    FROM target_cohort t
    JOIN @work_database_schema.@cohort_table e
        ON  t.person_id             = e.subject_id
        AND e.cohort_definition_id IN (@event_cohort_ids)
        AND e.cohort_start_date    >= t.window_start_date
        AND e.cohort_start_date    <= t.window_end_date
),

/*
  STEP 1: Era collapse
  Merge same-drug intervals separated by <= @era_collapse_size days
  into a single era. Algorithm: LAG to measure gap, cumulative SUM to
  assign an island group, then MIN/MAX to collapse.
*/
era_ordered AS (
    SELECT
        person_id, n_target, event_cohort_id,
        event_start_date, event_end_date,
        DATEDIFF(day,
            LAG(event_end_date) OVER (
                PARTITION BY person_id, n_target, event_cohort_id
                ORDER BY event_start_date
            ),
            event_start_date
        ) AS gap_from_prev
    FROM raw_events
),
era_flagged AS (
    SELECT
        person_id, n_target, event_cohort_id,
        event_start_date, event_end_date,
        CASE
            WHEN gap_from_prev IS NULL OR gap_from_prev > @era_collapse_size
            THEN 1 ELSE 0
        END AS is_new_era
    FROM era_ordered
),
era_groups AS (
    SELECT
        person_id, n_target, event_cohort_id,
        event_start_date, event_end_date,
        SUM(is_new_era) OVER (
            PARTITION BY person_id, n_target, event_cohort_id
            ORDER BY event_start_date
            ROWS UNBOUNDED PRECEDING
        ) AS era_group
    FROM era_flagged
),
collapsed_eras_all AS (
    SELECT
        person_id, n_target, event_cohort_id,
        MIN(event_start_date) AS event_start_date,
        MAX(event_end_date)   AS event_end_date
    FROM era_groups
    GROUP BY person_id, n_target, event_cohort_id, era_group
),

/*
  minEraDuration filter
  Remove collapsed eras whose duration is shorter than @min_era_duration.
  A value of 0 (default) keeps all eras.
*/
collapsed_eras AS (
    SELECT person_id, n_target, event_cohort_id, event_start_date, event_end_date
    FROM collapsed_eras_all
    WHERE DATEDIFF(day, event_start_date, event_end_date) >= @min_era_duration
),

/*
  STEP 2: Breakpoints
  Every unique start and end date per person x n_target becomes a
  cut point. UNION (not UNION ALL) deduplicates coincident dates.
*/
breakpoints AS (
    SELECT person_id, n_target, event_start_date AS dt
    FROM collapsed_eras
    UNION
    SELECT person_id, n_target, event_end_date AS dt
    FROM collapsed_eras
),

/*
  STEP 3: Atomic segments
  Form non-overlapping slices between consecutive breakpoints via
  LEAD. The trailing open segment (seg_end IS NULL) is removed in a
  subquery instead of QUALIFY, for cross-database compatibility.
*/
segments_raw AS (
    SELECT
        person_id, n_target,
        dt AS seg_start,
        LEAD(dt) OVER (
            PARTITION BY person_id, n_target
            ORDER BY dt
        ) AS seg_end
    FROM breakpoints
),
segments AS (
    SELECT person_id, n_target, seg_start, seg_end
    FROM segments_raw
    WHERE seg_end IS NOT NULL
),

/*
  STEP 4: Active drugs per segment
  Join back to collapsed_eras to find every drug whose era fully
  contains each atomic segment (seg_start >= era_start AND
  seg_end <= era_end).
*/
active_in_segment AS (
    SELECT
        s.person_id, s.n_target,
        s.seg_start, s.seg_end,
        DATEDIFF(day, s.seg_start, s.seg_end) AS seg_duration,
        e.event_cohort_id
    FROM segments s
    JOIN collapsed_eras e
        ON  s.person_id  = e.person_id
        AND s.n_target   = e.n_target
        AND s.seg_start  >= e.event_start_date
        AND s.seg_end    <= e.event_end_date
),

/*
  STEP 4a: Combo labeling and combination filters
  Label each segment with a '+'-delimited sorted string of cohort IDs
  (e.g. "1067" or "1067+1068"). Apply two filters:
    - combination_window: overlap must be >= this many days to qualify
      as a true combination (not a short transitional overlap).
    - min_post_combination_duration: combination segment must be at
      least this long to be retained after assembly.
  Single-drug segments always pass both filters.
*/
labeled_segments AS (
    SELECT
        person_id, n_target, seg_start, seg_end, seg_duration,
        COUNT(*)  AS drug_count,
        @combo_agg_sql AS combo_label
    FROM active_in_segment
    GROUP BY person_id, n_target, seg_start, seg_end, seg_duration
    HAVING
        COUNT(*) = 1
        OR (COUNT(*) > 1 AND seg_duration >= @combination_window)
),
filtered_segments AS (
    SELECT person_id, n_target, seg_start, seg_end, drug_count, combo_label
    FROM labeled_segments
    WHERE drug_count = 1
       OR seg_duration >= @min_post_combination_duration
),

/*
  STEP 5: Island collapse
  Collapse adjacent segments that share the same combo_label into a
  single treatment era. Two segments are contiguous when the current
  seg_start equals the prior seg_end (no gap, no overlap).
*/
island_lag AS (
    SELECT
        person_id, n_target, seg_start, seg_end, drug_count, combo_label,
        LAG(combo_label) OVER (
            PARTITION BY person_id, n_target
            ORDER BY seg_start
        ) AS prev_label,
        LAG(seg_end) OVER (
            PARTITION BY person_id, n_target
            ORDER BY seg_start
        ) AS prev_seg_end
    FROM filtered_segments
),
island_flagged AS (
    SELECT
        person_id, n_target, seg_start, seg_end, drug_count, combo_label,
        CASE
            WHEN combo_label = prev_label AND seg_start = prev_seg_end
            THEN 0 ELSE 1
        END AS is_new_island
    FROM island_lag
),
island_groups AS (
    SELECT
        person_id, n_target, seg_start, seg_end, drug_count, combo_label,
        SUM(is_new_island) OVER (
            PARTITION BY person_id, n_target
            ORDER BY seg_start
            ROWS UNBOUNDED PRECEDING
        ) AS island_id
    FROM island_flagged
),
final_events_raw AS (
    SELECT
        person_id, n_target, combo_label, island_id,
        MIN(seg_start) AS event_start_date,
        MAX(seg_end)   AS event_end_date
    FROM island_groups
    GROUP BY person_id, n_target, combo_label, island_id
),
/*
  duration_era is derived after collapsing so DATEDIFF references
  scalar columns rather than aggregate expressions, which is safer
  across all supported dialects.
*/
final_events AS (
    SELECT
        person_id, n_target, combo_label,
        event_start_date, event_end_date,
        DATEDIFF(day, event_start_date, event_end_date) AS duration_era
    FROM final_events_raw
),

/*
  STEP 6: filterTreatments
  Apply the filterTreatments rule before sequence numbering.
    All     - keep every era as-is.
    Changes - remove consecutive repeat of the same combo_label per
              person x n_target (e.g. DrugA -> DrugA -> DrugB becomes
              DrugA -> DrugB). Implemented by flagging rows where the
              combo_label differs from the prior row.
    First   - keep only the first occurrence of each combo_label per
              person x n_target, regardless of position.
*/
filtered_events AS (
    SELECT
        person_id, n_target, combo_label,
        event_start_date, event_end_date, duration_era,
        LAG(combo_label) OVER (
            PARTITION BY person_id, n_target
            ORDER BY event_start_date
        ) AS prev_combo_label,
        ROW_NUMBER() OVER (
            PARTITION BY person_id, n_target, combo_label
            ORDER BY event_start_date
        ) AS occurrence_num
    FROM final_events
),

/*
  STEP 7: Sequence assignment
  Number treatment lines per person x n_target in chronological order.
  Join target dates for the final output. Cap at @max_path_length.
*/
sequenced AS (
    SELECT
        fe.person_id,
        fe.n_target,
        tc.target_start_date,
        tc.target_end_date,
        fe.combo_label,
        fe.event_start_date,
        fe.event_end_date,
        fe.duration_era,
        ROW_NUMBER() OVER (
            PARTITION BY fe.person_id, fe.n_target
            ORDER BY fe.event_start_date
        ) AS event_seq
    FROM filtered_events fe
    JOIN target_cohort tc
        ON  fe.person_id = tc.person_id
        AND fe.n_target  = tc.n_target
    WHERE
        '@filter_treatments' = 'All'
        OR ('@filter_treatments' = 'Changes' AND (
                prev_combo_label IS NULL
                OR combo_label <> prev_combo_label
           )
        )
        OR ('@filter_treatments' = 'First' AND occurrence_num = 1)
)
SELECT
    person_id,
    n_target,
    target_start_date,
    target_end_date,
    combo_label,
    event_start_date,
    event_end_date,
    duration_era,
    event_seq
INTO @work_database_schema.@tx_history_table
FROM sequenced
WHERE event_seq <= @max_path_length
;
