
#' Make the treatment history table as a temp in the database
#' @param connection the connection to an omop database
#' @param workDatabaseSchema the database schema where results and cohorts can read and written
#' @param cohortTable the table where the cohort table is located
#' @param targetCohortId the id of the cohort in the cohort table
#' @param eventCohortIds the ids of the event cohort in the cohort table
#' @param gapDays the gap between eras to collapse on, defaults to 32
#' @param tableName the name of the table to save the treatment history table
#' @export
createTxHistoryTable <- function(
    connection,
    workDatabaseSchema,
    cohortTable,
    targetCohortId,
    eventCohortIds,
    gapDays = 32,
    tableName
) {

  # get sql file
  sqlFile <- fs::path_package(package = "TxPathways", "sql/treatment_history.sql")
  #sqlFile <- here::here("ideas/DrugUtilization/inst/sql/treatment_history.sql")

  tableName <- paste0(workDatabaseSchema, '.', tableName)

  # read render and translate sql -> prep for execution
  sql <- read_render_translate(
    sqlFile = sqlFile,
    dbms = connection@dbms,
    tempEmulationSchema = workDatabaseSchema,
    work_database_schema = workDatabaseSchema,
    cohort_table = cohortTable,
    target_cohort_id = targetCohortId,
    event_cohort_id = eventCohortIds,
    gap_days = gapDays,
    trt_history_table = tableName
  )

  cli::cat_bullet(
    glue::glue("Building Treatment History table: {crayon::red(tableName)}"),
    bullet = "info",
    bullet_col = "blue"
  )

  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql
  )

  invisible(sql)
}


dropTxHistoryTable <- function(connection, tableName = '#trt_history') {

  cli::cat_bullet(
    glue::glue("Droping Treatment History Table: {crayon::red(tableName)}"),
    bullet = "info",
    bullet_col = "blue"
  )

  DatabaseConnector::executeSql(
    connection = connection,
    sql = trunc_drop_tbl(table = tableName)
  )
}

retrieveTxHistoryTable <- function(
    connection,
    tableName
) {

  cli::cat_bullet(
    glue::glue("Collecting Treatment History Table: {crayon::red(tableName)}"),
    bullet = "info",
    bullet_col = "blue"
  )

  tbl <- DatabaseConnector::querySql(
    connection = connection,
    sql = pull_tbl(table = tableName)
  ) |>
  tibble::as_tibble() |>
    dplyr::rename_with(.fn = tolower)

  return(tbl)
}

#' Function that retrieves and formats tx history table
#' @param connection the connection to an omop database
#' @param targetCohortKey a tibble with the target cohort id and name to use as a merge key for formatting
#' @param eventCohortKey a tibble with the event cohort id and name to use as a merge key for formatting
#' @param tableName the name of the table to save the treatment history table
#' @param dropTable toggle to drop table, defaults to true
#' @return a tibble with the treatment history information
#' @export
getTxHistoryTable <- function(
    connection,
    targetCohortKey,
    eventCohortKey,
    tableName,
    workDatabaseSchema,
    dropTable = TRUE
) {

  tableName <- paste0(workDatabaseSchema, '.', tableName)
  # get table from db
  tbl <- retrieveTxHistoryTable(connection = connection, tableName = tableName)

  # get all combinations of event cohorts
  eventCohortKey_new <- format_event_combos(eventCohortKey = eventCohortKey)

  # merge table with format keys
  tbl2 <- tbl |>
    dplyr::rename(
      target_cohort_id = cohort_definition_id,
      event_cohort_id = event_id,
      event_seq = row_num
    ) |>
    dplyr::left_join(
      targetCohortKey, by = c("target_cohort_id")
    ) |>
    dplyr::left_join(
      eventCohortKey_new, by = c("event_cohort_id")
    ) |>
    dplyr::select(
      subject_id, event_seq, target_cohort_id, target_cohort_name, target_start, target_end,
      event_cohort_id, event_cohort_name, event_start, event_end
    )

  if (dropTable) {
    #dropTxHistoryTable(connection)
    DatabaseConnector::renderTranslateExecuteSql(connection, "DROP TABLE @table_name;", table_name = tableName)
  }

  if (nrow(tbl2) == 0) {
    warning("Your treatment history table is empty!")
  }

  return(tbl2)
}
