#' Returns dialect-appropriate string aggregation SQL for combo_label
#'
#' Used internally by `TxHistory$assemble_treatment_history_sql()`.
#'
#' @param dbms Character string; the DBMS dialect name.
#' @return A SQL string fragment for use in a `@combo_agg_sql` parameter.
#' @noRd
combo_agg_sql <- function(dbms) {
    if (dbms %in% c("sql server", "pdw", "synapse")) {
        "STRING_AGG(CAST(event_cohort_id AS VARCHAR(255)), '+') WITHIN GROUP (ORDER BY event_cohort_id)"
    } else if (dbms == "postgresql") {
        "STRING_AGG(CAST(event_cohort_id AS VARCHAR), '+' ORDER BY event_cohort_id)"
    } else if (dbms %in% c("redshift", "snowflake", "oracle")) {
        "LISTAGG(CAST(event_cohort_id AS VARCHAR(255)), '+') WITHIN GROUP (ORDER BY event_cohort_id)"
    } else if (dbms == "bigquery") {
        "STRING_AGG(CAST(event_cohort_id AS STRING), '+' ORDER BY event_cohort_id)"
    } else if (dbms == "spark") {
        "CONCAT_WS('+', SORT_ARRAY(COLLECT_LIST(CAST(event_cohort_id AS STRING))))"
    } else {
        cli::cli_abort("Unsupported dbms {.val {dbms}} for combo label aggregation.")
    }
}



#' Returns dialect-appropriate string aggregation SQL for pathway paths
#'
#' Used internally by `TxAnalysis$run_pathways()`.
#'
#' @param dbms Character string; the DBMS dialect name.
#' @return A SQL string fragment for building a `path` column.
#' @noRd
path_agg_sql <- function(dbms) {
    if (dbms %in% c("sql server", "pdw", "synapse")) {
        "STRING_AGG(CAST(combo_label AS VARCHAR(255)), ' | ') WITHIN GROUP (ORDER BY event_seq) AS path"
    } else if (dbms == "postgresql") {
        "STRING_AGG(CAST(combo_label AS VARCHAR), ' | ' ORDER BY event_seq) AS path"
    } else if (dbms %in% c("redshift", "snowflake", "oracle")) {
        "LISTAGG(CAST(combo_label AS VARCHAR(255)), ' | ') WITHIN GROUP (ORDER BY event_seq) AS path"
    } else if (dbms == "bigquery") {
        "STRING_AGG(CAST(combo_label AS STRING), ' | ' ORDER BY event_seq) AS path"
    } else if (dbms == "spark") {
        "CONCAT_WS(' | ', COLLECT_LIST(CAST(combo_label AS STRING))) AS path"
    } else {
        cli::cli_abort("Unsupported dbms {.val {dbms}} for path label aggregation.")
    }
}
