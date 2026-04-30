/*
 * Treatment Pathway Frequency
 * Parameters: @work_database_schema, @tx_history_table, @path_agg_sql, @min_frequency, @max_path_length
 * Returns: path (e.g. "DrugA | DrugB | DrugA"), freq
 */
WITH filtered AS (
    SELECT person_id, n_target, event_seq, combo_label
    FROM @work_database_schema.@tx_history_table
    WHERE event_seq <= @max_path_length
),
paths AS (
    SELECT
        n_target,
        person_id,
        @path_agg_sql
    FROM filtered
    GROUP BY n_target, person_id
),
freq AS (
    SELECT path, COUNT(*) AS freq
    FROM paths
    GROUP BY path
)
SELECT path, freq
FROM freq
WHERE freq >= @min_frequency
ORDER BY freq DESC, path
