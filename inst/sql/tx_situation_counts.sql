/*
 * Treatment Situation Counts
 * Parameters: work_database_schema, tx_history_table, min_cell_count
 *
 * Returns a summary count table of treatment situations per (person, target index):
 *   - Total Subjects
 *   - Mono-treatment Only         (no combo, switch, or interruption)
 *   - Any Combination Treatment   (at least one combo_label containing '+')
 *   - Switched Drugs              (at least one consecutive different drug)
 *   - Interrupted Treatment       (at least one consecutive same drug after a gap)
 *   - Discontinued                (last recorded event, no switch or interruption observed)
 *
 * Note: 'Discontinued' here means last treatment with no further events recorded.
 *       It does not distinguish from end of observation. Not censoring-adjusted.
 */
WITH transitions AS (
    SELECT
        person_id,
        n_target,
        combo_label,
        event_seq,
        LEAD(combo_label) OVER (
            PARTITION BY person_id, n_target ORDER BY event_seq
        ) AS next_combo_label
    FROM @work_database_schema.@tx_history_table
),
person_flags AS (
    SELECT
        person_id,
        n_target,
        MAX(CASE WHEN combo_label LIKE '%%+%%'                                          THEN 1 ELSE 0 END) AS has_combination,
        MAX(CASE WHEN next_combo_label IS NOT NULL AND next_combo_label != combo_label  THEN 1 ELSE 0 END) AS has_switch,
        MAX(CASE WHEN next_combo_label IS NOT NULL AND next_combo_label = combo_label   THEN 1 ELSE 0 END) AS has_interrupt,
        MAX(CASE WHEN next_combo_label IS NULL                                          THEN 1 ELSE 0 END) AS is_last_event
    FROM transitions
    GROUP BY person_id, n_target
),
total AS (
    SELECT COUNT(*) AS n FROM person_flags
)
SELECT *
FROM (
    SELECT 'Total Subjects'                            AS situation,
        n                                              AS subject_count,
        100.0                                          AS pct
    FROM total

    UNION ALL

    SELECT 'Mono-treatment Only',
        SUM(CASE WHEN has_combination = 0 AND has_switch = 0 AND has_interrupt = 0 THEN 1 ELSE 0 END),
        ROUND(100.0 * SUM(CASE WHEN has_combination = 0 AND has_switch = 0 AND has_interrupt = 0 THEN 1 ELSE 0 END) / MAX(n), 2)
    FROM person_flags CROSS JOIN total

    UNION ALL

    SELECT 'Any Combination Treatment',
        SUM(has_combination),
        ROUND(100.0 * SUM(has_combination) / MAX(n), 2)
    FROM person_flags CROSS JOIN total

    UNION ALL

    SELECT 'Switched Drugs',
        SUM(has_switch),
        ROUND(100.0 * SUM(has_switch) / MAX(n), 2)
    FROM person_flags CROSS JOIN total

    UNION ALL

    SELECT 'Interrupted Treatment (same drug after gap)',
        SUM(has_interrupt),
        ROUND(100.0 * SUM(has_interrupt) / MAX(n), 2)
    FROM person_flags CROSS JOIN total

    UNION ALL

    SELECT 'Discontinued (no further treatment recorded)',
        SUM(CASE WHEN is_last_event = 1 AND has_switch = 0 AND has_interrupt = 0 THEN 1 ELSE 0 END),
        ROUND(100.0 * SUM(CASE WHEN is_last_event = 1 AND has_switch = 0 AND has_interrupt = 0 THEN 1 ELSE 0 END) / MAX(n), 2)
    FROM person_flags CROSS JOIN total

) situation_counts
WHERE situation = 'Total Subjects' OR subject_count >= @min_cell_count
ORDER BY subject_count DESC
