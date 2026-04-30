# Private helper: returns the dialect-appropriate string aggregation SQL
# for building the combo_label column. Called from assemble_treatment_history_sql().
combo_agg_sql <- function(dbms) {
    if (dbms %in% c("sql server", "pdw", "synapse")) {
        agg_sql <- "STRING_AGG(CAST(event_cohort_id AS VARCHAR(255)), '+') WITHIN GROUP (ORDER BY event_cohort_id)"
    }

    if (dbms == "postgresql") {
        agg_sql <- "STRING_AGG(CAST(event_cohort_id AS VARCHAR), '+' ORDER BY event_cohort_id)"
    }

    if (dbms %in% c("redshift", "snowflake", "oracle")) {
        agg_sql <- "LISTAGG(CAST(event_cohort_id AS VARCHAR(255)), '+') WITHIN GROUP (ORDER BY event_cohort_id)"
    }
    # lacks testing
    if (dbms == "bigquery") {
        agg_sql <- "STRING_AGG(CAST(event_cohort_id AS STRING), '+' ORDER BY event_cohort_id)"
    }
    # lacks testing
    if (dbms == "spark") {
        agg_sql <- "CONCAT_WS('+', SORT_ARRAY(COLLECT_LIST(CAST(event_cohort_id AS STRING))))" 
    }
    return(agg_sql)
    cli::cli_abort("Unsupported dbms {.val {dbms}} for combo label aggregation.")
}



# Private helper: returns the dialect-appropriate string aggregation SQL
# for building the path column from combo_label values. Called from run_pathways().
path_agg_sql <- function(dbms) {
    if (dbms %in% c("sql server", "pdw", "synapse")) {
        agg_sql <- "STRING_AGG(CAST(combo_label AS VARCHAR(255)), ' | ') WITHIN GROUP (ORDER BY event_seq) AS path"
    }

    if (dbms == "postgresql") {
        agg_sql <- "STRING_AGG(CAST(combo_label AS VARCHAR), ' | ' ORDER BY event_seq) AS path"
    }

    if (dbms %in% c("redshift", "snowflake", "oracle")) {
        agg_sql <- "LISTAGG(CAST(combo_label AS VARCHAR(255)), ' | ') WITHIN GROUP (ORDER BY event_seq) AS path"
    }
    # lacks testing
    if (dbms == "bigquery") {
        agg_sql <- "STRING_AGG(CAST(combo_label AS STRING), ' | ' ORDER BY event_seq) AS path"
    }
    # lacks testing
    if (dbms == "spark") {
        agg_sql <- "CONCAT_WS(' | ', COLLECT_LIST(CAST(combo_label AS STRING))) AS path"
    }
    return(agg_sql)
    cli::cli_abort("Unsupported dbms {.val {dbms}} for path label aggregation.")
}
