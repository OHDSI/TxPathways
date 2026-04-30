/*
 * Exit Reason Counts
 * Parameters: @work_database_schema, @tx_history_table, @cohort_table, @exit_cohort_ids, @min_cell_count
 *
 * Joins the last observed treatment event per person to exit cohorts.
 * If an exit cohort event falls within the target observation window, it is
 * classified as the exit reason. Otherwise the exit reason is NULL
 * (interpreted as "End of Observation" in R).
 *
 * Note: cohort IDs are resolved to human-readable labels in R using txCohorts.
 */
WITH last_events AS (
    SELECT
        person_id,
        n_target,
        target_start_date,
        target_end_date
    FROM (
        SELECT
            person_id,
            n_target,
            target_start_date,
            target_end_date,
            ROW_NUMBER() OVER (
                PARTITION BY person_id, n_target ORDER BY event_seq DESC
            ) AS rn
        FROM @work_database_schema.@tx_history_table
    ) ranked
    WHERE rn = 1
),
exit_events AS (
    SELECT
        le.person_id,
        le.n_target,
        e.cohort_definition_id AS exit_cohort_id
    FROM last_events le
    JOIN @work_database_schema.@cohort_table e
        ON  le.person_id             = e.subject_id
        AND e.cohort_definition_id  IN (@exit_cohort_ids)
        AND e.cohort_start_date     >= le.target_start_date
        AND e.cohort_start_date     <= le.target_end_date
),
person_exit AS (
    SELECT
        person_id,
        n_target,
        MIN(exit_cohort_id) AS exit_cohort_id
    FROM exit_events
    GROUP BY person_id, n_target
),
total AS (
    SELECT COUNT(*) AS n FROM last_events
)
SELECT
    pe.exit_cohort_id,
    COUNT(DISTINCT le.person_id)                                           AS subject_count,
    ROUND(100.0 * COUNT(DISTINCT le.person_id) / MAX(t.n), 2)             AS pct
FROM last_events le
LEFT JOIN person_exit pe
    ON le.person_id  = pe.person_id
    AND le.n_target  = pe.n_target
CROSS JOIN total t
GROUP BY pe.exit_cohort_id
HAVING COUNT(DISTINCT le.person_id) >= @min_cell_count
ORDER BY subject_count DESC
