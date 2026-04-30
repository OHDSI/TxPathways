/*
 * Treatment Adherence - Transition Classification
 * Parameters: @work_database_schema, @tx_history_table, @min_cell_count
 *
 * Classifies each treatment event transition per person as:
 *   Interrupted  - same combo_label appears again after a gap
 *   Switched     - next event has a different combo_label
 *   Discontinued - no subsequent event (last recorded event)
 *
 * Note: Discontinuation is not censoring-adjusted. All values are lower bounds.
 *       Gap classification into user-defined intervals is applied in R after retrieval.
 */
WITH lead_events AS (
    SELECT
        person_id,
        n_target,
        event_seq,
        combo_label,
        event_end_date,
        LEAD(combo_label) OVER (
            PARTITION BY person_id, n_target ORDER BY event_seq
        ) AS next_combo_label,
        LEAD(event_start_date) OVER (
            PARTITION BY person_id, n_target ORDER BY event_seq
        ) AS next_event_start
    FROM @work_database_schema.@tx_history_table
),
transitions AS (
    SELECT
        person_id,
        n_target,
        event_seq,
        combo_label,
        next_combo_label,
        DATEDIFF(day, event_end_date, next_event_start) AS gap_days,
        CASE
            WHEN next_combo_label IS NULL            THEN 'Discontinued'
            WHEN next_combo_label = combo_label      THEN 'Interrupted'
            ELSE                                          'Switched'
        END AS transition_type
    FROM lead_events
)
SELECT
    combo_label,
    transition_type,
    event_seq,
    COUNT(DISTINCT person_id)                                        AS subject_count,
    AVG(CAST(gap_days AS FLOAT))                                     AS mean_gap_days,
    MIN(gap_days)                                                    AS min_gap_days,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY gap_days)           AS p25_gap_days,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY gap_days)           AS median_gap_days,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY gap_days)           AS p75_gap_days,
    MAX(gap_days)                                                    AS max_gap_days
FROM transitions
GROUP BY combo_label, transition_type, event_seq
HAVING COUNT(DISTINCT person_id) >= @min_cell_count
ORDER BY event_seq, combo_label, transition_type
