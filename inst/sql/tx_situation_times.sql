/*
 * Treatment Situation Times
 * Parameters: work_database_schema, tx_history_table, min_cell_count, max_line
 *
 * Returns time-based summary statistics (days) for three metrics:
 *   1. Time to Therapy Line      - days from target_start_date to each event line start
 *                                  (filtered to event_seq <= @max_line)
 *   2. Time Between Treatments   - days between event_end_date and next event_start_date
 *   3. Time After Last Treatment - days from last event_end_date to target_end_date
 *
 * All values are in days. No censoring adjustment applied.
 */
WITH ordered AS (
    SELECT
        person_id,
        n_target,
        event_seq,
        combo_label,
        event_start_date,
        event_end_date,
        target_start_date,
        target_end_date,
        LEAD(event_start_date) OVER (
            PARTITION BY person_id, n_target ORDER BY event_seq
        ) AS next_event_start,
        MAX(event_seq)    OVER (PARTITION BY person_id, n_target) AS max_seq,
        MAX(event_end_date) OVER (PARTITION BY person_id, n_target) AS last_event_end
    FROM @work_database_schema.@tx_history_table
),
time_metrics AS (
    SELECT
        person_id,
        n_target,
        event_seq,
        combo_label,
        DATEDIFF(day, target_start_date, event_start_date)                            AS days_to_line,
        CASE
            WHEN next_event_start IS NOT NULL
            THEN DATEDIFF(day, event_end_date, next_event_start)
            ELSE NULL
        END                                                                            AS gap_to_next_days,
        CASE
            WHEN event_seq = max_seq
            THEN DATEDIFF(day, event_end_date, target_end_date)
            ELSE NULL
        END                                                                            AS days_after_last_event
    FROM ordered
)

/* 1. Time to therapy line (filtered by max_line) */
SELECT
    'Time to Therapy Line'                                                 AS metric,
    event_seq,
    combo_label,
    COUNT(DISTINCT person_id)                                              AS subject_count,
    AVG(CAST(days_to_line AS FLOAT))                                       AS mean,
    STDDEV(CAST(days_to_line AS FLOAT))                                    AS sd,
    MIN(days_to_line)                                                      AS min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY days_to_line)             AS p25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY days_to_line)             AS median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY days_to_line)             AS p75,
    MAX(days_to_line)                                                      AS max
FROM time_metrics
WHERE event_seq <= @max_line
GROUP BY event_seq, combo_label
HAVING COUNT(DISTINCT person_id) >= @min_cell_count

UNION ALL

/* 2. Time between consecutive treatments (gap) */
SELECT
    'Time Between Treatments'                                              AS metric,
    event_seq,
    combo_label,
    COUNT(DISTINCT person_id)                                              AS subject_count,
    AVG(CAST(gap_to_next_days AS FLOAT))                                   AS mean,
    STDDEV(CAST(gap_to_next_days AS FLOAT))                                AS sd,
    MIN(gap_to_next_days)                                                  AS min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY gap_to_next_days)         AS p25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY gap_to_next_days)         AS median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY gap_to_next_days)         AS p75,
    MAX(gap_to_next_days)                                                  AS max
FROM time_metrics
WHERE gap_to_next_days IS NOT NULL
GROUP BY event_seq, combo_label
HAVING COUNT(DISTINCT person_id) >= @min_cell_count

UNION ALL

/* 3. Time from last treatment event to end of observation */
SELECT
    'Time After Last Treatment to End of Observation'                      AS metric,
    -1                                                                     AS event_seq,
    'Overall'                                                              AS combo_label,
    COUNT(DISTINCT person_id)                                              AS subject_count,
    AVG(CAST(days_after_last_event AS FLOAT))                              AS mean,
    STDDEV(CAST(days_after_last_event AS FLOAT))                           AS sd,
    MIN(days_after_last_event)                                             AS min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY days_after_last_event)    AS p25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY days_after_last_event)    AS median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY days_after_last_event)    AS p75,
    MAX(days_after_last_event)                                             AS max
FROM time_metrics
WHERE days_after_last_event IS NOT NULL
HAVING COUNT(DISTINCT person_id) >= @min_cell_count

ORDER BY metric, event_seq, combo_label
