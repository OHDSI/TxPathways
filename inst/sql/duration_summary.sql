/*
 * Treatment Duration Summary
 * Parameters: @work_database_schema, @tx_history_table, @min_cell_count
 * Groupings: Overall | By Line (event_seq) | By Event (combo_label) | By Line and Event
 * Output: grouping_type, combo_label, event_seq, subject_count, mean, sd, min, p10, p25, median, p75, p90, max
 * Note: event_seq = -1 indicates 'not applicable' (aggregated across all lines)
 */
SELECT *
FROM (

    /* Overall */
    SELECT
        'Overall'                                                          AS grouping_type,
        'Overall'                                                          AS combo_label,
        -1                                                                 AS event_seq,
        COUNT(DISTINCT person_id)                                          AS subject_count,
        AVG(CAST(duration_era AS FLOAT))                                   AS mean,
        STDDEV(CAST(duration_era AS FLOAT))                                AS sd,
        MIN(duration_era)                                                  AS min,
        PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY duration_era)         AS p10,
        PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration_era)         AS p25,
        PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY duration_era)         AS median,
        PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY duration_era)         AS p75,
        PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY duration_era)         AS p90,
        MAX(duration_era)                                                  AS max
    FROM @work_database_schema.@tx_history_table

    UNION ALL

    /* By line of therapy */
    SELECT
        'By Line'                                                          AS grouping_type,
        'Overall'                                                          AS combo_label,
        event_seq,
        COUNT(DISTINCT person_id)                                          AS subject_count,
        AVG(CAST(duration_era AS FLOAT))                                   AS mean,
        STDDEV(CAST(duration_era AS FLOAT))                                AS sd,
        MIN(duration_era)                                                  AS min,
        PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY duration_era)         AS p10,
        PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration_era)         AS p25,
        PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY duration_era)         AS median,
        PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY duration_era)         AS p75,
        PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY duration_era)         AS p90,
        MAX(duration_era)                                                  AS max
    FROM @work_database_schema.@tx_history_table
    GROUP BY event_seq

    UNION ALL

    /* By event (combo_label) */
    SELECT
        'By Event'                                                         AS grouping_type,
        combo_label,
        -1                                                                 AS event_seq,
        COUNT(DISTINCT person_id)                                          AS subject_count,
        AVG(CAST(duration_era AS FLOAT))                                   AS mean,
        STDDEV(CAST(duration_era AS FLOAT))                                AS sd,
        MIN(duration_era)                                                  AS min,
        PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY duration_era)         AS p10,
        PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration_era)         AS p25,
        PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY duration_era)         AS median,
        PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY duration_era)         AS p75,
        PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY duration_era)         AS p90,
        MAX(duration_era)                                                  AS max
    FROM @work_database_schema.@tx_history_table
    GROUP BY combo_label

    UNION ALL

    /* By line and event */
    SELECT
        'By Line And Event'                                                AS grouping_type,
        combo_label,
        event_seq,
        COUNT(DISTINCT person_id)                                          AS subject_count,
        AVG(CAST(duration_era AS FLOAT))                                   AS mean,
        STDDEV(CAST(duration_era AS FLOAT))                                AS sd,
        MIN(duration_era)                                                  AS min,
        PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY duration_era)         AS p10,
        PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration_era)         AS p25,
        PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY duration_era)         AS median,
        PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY duration_era)         AS p75,
        PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY duration_era)         AS p90,
        MAX(duration_era)                                                  AS max
    FROM @work_database_schema.@tx_history_table
    GROUP BY combo_label, event_seq

) duration_summary
WHERE subject_count >= @min_cell_count
ORDER BY grouping_type, event_seq, combo_label
