# Primary Classes ----------------
TxHistory <- R6::R6Class(
    classname = "TxHistory",
    public = list(
        initialize = function(txCohorts,
                              followUpWindow,
                              eraCollapseSettings,
                              combinationTreatmentSettings,
                              pathwayOptions,
                              executionSettings,
                              txHistoryTableName = "tx_history") {
            # review txCohorts dataframe then add to class
            checkmate::assertDataFrame(txCohorts, min.rows = 1)
            checkmate::assertNames(names(txCohorts), permutation.of = c("cohortId", "cohortLabel", "cohortType"))
            checkmate::assertSubset(unique(txCohorts$cohortType), choices = c("target", "event", "exit"))
            checkmate::assertTRUE(sum(txCohorts$cohortType == "target") == 1)
            private$.txCohorts <- txCohorts

            checkmate::assertClass(followUpWindow, classes = "FollowUpWindow")
            private$.followUpWindow <- followUpWindow
            checkmate::assertClass(eraCollapseSettings, classes = "EraCollapseSettings")
            private$.eraCollapseSettings <- eraCollapseSettings
            checkmate::assertClass(combinationTreatmentSettings, classes = "CombinationTreatmentSettings")
            private$.combinationTreatmentSettings <- combinationTreatmentSettings
            checkmate::assertClass(pathwayOptions, classes = "PathwayOptions")
            private$.pathwayOptions <- pathwayOptions
            checkmate::assertClass(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
            private$.executionSettings <- executionSettings
            checkmate::assertString(txHistoryTableName)
            private$.txHistoryTable <- txHistoryTableName
        },

        buildTreatmentHistory = function() {
            # get connection from execution settings and ensure it gets closed when function exits
            settings <- private$.executionSettings
            connection <- settings$getConnection()
            if (is.null(connection)) {
                settings$connect()
                connection <- settings$getConnection()
            }
            withr::defer(settings$disconnect())

            # print analysis settings to console
            self$printAnalysis()

            # assemble sql
            cli::cli_progress_step("Assembling treatment history SQL")
            sql <- private$assemble_treatment_history_sql() 

            # execute sql to build treatment history table
            cli::cli_progress_step(
                "Building treatment history table {.val {private$.txHistoryTable}}..."
            )
            tryCatch(
                {
                    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
                    cli::cli_alert_success(
                        "Treatment history table successfully built: {.val {private$.txHistoryTable}}."
                    )
                    private$.builtTimestamp <- Sys.time()
                    private$fetch_th_summary(connection)
                },
                error = function(e) {
                    cli::cli_abort(
                        c(
                            "Failed to build treatment history table {.val {private$.txHistoryTable}}.",
                            "x" = "{conditionMessage(e)}"
                        )
                    )
                }
            )

        },

        printAnalysis = function() {
            cli::cli_rule(left = "Treatment History Settings")

            cli::cli_h3("Tx Cohorts")
            cli::cli_verbatim(paste(capture.output(print(private$.txCohorts)), collapse = "\n"))

            cli::cli_h3("Follow-up Window")
            cli::cli_dl(c(
                "Start Anchor" = private$.followUpWindow$startAnchor,
                "Start Days"   = as.character(private$.followUpWindow$startDays),
                "End Anchor"   = private$.followUpWindow$endAnchor,
                "End Days"     = as.character(private$.followUpWindow$endDays)
            ))

            cli::cli_h3("Era Collapse Settings")
            cli::cli_dl(c(
                "Era Collapse Size" = as.character(private$.eraCollapseSettings$eraCollapseSize),
                "Min Era Duration"  = as.character(private$.eraCollapseSettings$minEraDuration)
            ))

            cli::cli_h3("Combination Treatment Settings")
            cli::cli_dl(c(
                "Combination Window"             = as.character(private$.combinationTreatmentSettings$combinationWindow),
                "Min Post-Combination Duration"  = as.character(private$.combinationTreatmentSettings$minPostCombinationDuration)
            ))

            cli::cli_h3("Pathway Options")
            cli::cli_dl(c(
                "Max Pathway Length" = as.character(private$.pathwayOptions$maxPathwayLength),
                "Filter Treatments"  = private$.pathwayOptions$filterTreatments
            ))

            cli::cli_rule()
        },

        saveQuery = function(savePath = here::here()) {
            
            file <- fs::path(savePath, "treatment_history_query.sql")
            sql <- private$assemble_treatment_history_sql()
            readr::write_lines(sql, file)
            cli::cli_alert_success("SQL query saved to {.file {file}}")
        },

        getSummary = function() {
            if (is.null(private$.lastSummary)) {
                cli::cli_alert_info("No summary available. Call {.code build()} first.")
                return(NULL)
            }
            return(private$.lastSummary)
        },

        hasBeenBuilt = function() {
            settings <- private$.executionSettings
            settings$connect()
            conn <- settings$getConnection()

            if (is.null(conn)) {
                return(FALSE)
            }

            withr::defer(settings$disconnect())

            query <- SqlRender::render(
                "SELECT COUNT(*) AS n FROM @work_database_schema.@tx_history_table",
                work_database_schema = private$.executionSettings$workDatabaseSchema,
                tx_history_table = private$.txHistoryTable
            ) |>
            SqlRender::translate(
                targetDialect = private$.executionSettings$getDbms(),
                tempEmulationSchema = private$.executionSettings$tempEmulationSchema
            )

            tryCatch(
                {
                    DatabaseConnector::querySql(conn, query)
                    return(TRUE)
                },
                error = function(e) {
                    return(FALSE)
                }
            )
        }
    ),
    private = list(
        .txCohorts = NULL,
        .followUpWindow = NULL,
        .eraCollapseSettings = NULL,
        .combinationTreatmentSettings = NULL,
        .pathwayOptions = NULL,
        .executionSettings = NULL,
        .txHistoryTable = "tx_history",
        .lastSummary = NULL,
        .builtTimestamp = NULL,

        # function to assemble SQL for building treatment history table based on class settings
        assemble_treatment_history_sql = function() {

            target_cohort_id <- private$.txCohorts |>
                dplyr::filter(cohortType == "target") |>
                dplyr::pull(cohortId)

            event_cohort_ids <- private$.txCohorts |>
                dplyr::filter(cohortType == "event") |>
                dplyr::pull(cohortId)

            dbms <- private$.executionSettings$getDbms()
            
            sql_template <- fs::path_package(package = "TxPathways", "sql/build_treatment_history.sql") |>
                readr::read_file()
             # render SQL for building treatment history table based on class settings
             sql <- sql_template |>
             SqlRender::render(
                 work_database_schema = private$.executionSettings$workDatabaseSchema,
                 cohort_table = private$.executionSettings$cohortTable,
                 tx_history_table = private$.txHistoryTable,
                 target_cohort_id = target_cohort_id,
                 event_cohort_ids = paste(event_cohort_ids, collapse = ","),
                 start_anchor = private$.followUpWindow$startAnchor,
                 start_days = private$.followUpWindow$startDays,
                 end_anchor = private$.followUpWindow$endAnchor,
                 end_days = private$.followUpWindow$endDays,
                 era_collapse_size = private$.eraCollapseSettings$eraCollapseSize,
                 min_era_duration = private$.eraCollapseSettings$minEraDuration,
                 combination_window = private$.combinationTreatmentSettings$combinationWindow,
                 min_post_combination_duration = private$.combinationTreatmentSettings$minPostCombinationDuration,
                 max_path_length = private$.pathwayOptions$maxPathwayLength,
                 filter_treatments = private$.pathwayOptions$filterTreatments,
                 combo_agg_sql = combo_agg_sql(dbms)
              ) |>
              SqlRender::translate(
                targetDialect = dbms,
                tempEmulationSchema = private$.executionSettings$tempEmulationSchema
              ) |>  # Convert CRLF to LF
              stringr::str_replace_all("\r", "\n")
             return(sql)
            
        },

        fetch_th_summary = function(connection) {
            cli::cli_progress_step("Fetching build summary statistics")

            # Query: total rows, unique persons, persons with events, avg events per person
            summary_query <- "SELECT
                    COUNT(*) AS total_rows,
                    COUNT(DISTINCT person_id) AS unique_persons,
                    COUNT(DISTINCT CASE WHEN event_seq >= 1 THEN person_id END) AS persons_with_events,
                    ROUND(CAST(COUNT(*) AS FLOAT) / NULLIF(COUNT(DISTINCT person_id), 0), 2) AS avg_events_per_person,
                    COUNT(DISTINCT combo_label) AS unique_lines,
                    SUM(CASE WHEN combo_label LIKE '%%+%%' THEN 1 ELSE 0 END) AS combo_events,
                    MAX(event_seq) AS max_pathway_length
                FROM @work_database_schema.@tx_history_table" |>
                SqlRender::render(
                    work_database_schema = private$.executionSettings$workDatabaseSchema,
                    tx_history_table = private$.txHistoryTable
                ) |>
                SqlRender::translate(
                    targetDialect = private$.executionSettings$getDbms(),
                    tempEmulationSchema = private$.executionSettings$tempEmulationSchema
                )

            summary_result <- DatabaseConnector::querySql(connection, summary_query)

            # Query: pathway length distribution
            pathway_query <- "WITH person_max_seq AS (
                    SELECT person_id, MAX(event_seq) AS max_seq
                    FROM @work_database_schema.@tx_history_table
                    GROUP BY person_id
                )
                SELECT
                    max_seq AS pathway_length,
                    COUNT(*) AS person_count
                FROM person_max_seq
                GROUP BY max_seq
                ORDER BY max_seq" |>
                SqlRender::render(
                    work_database_schema = private$.executionSettings$workDatabaseSchema,
                    tx_history_table = private$.txHistoryTable
                ) |>
                SqlRender::translate(
                    targetDialect = private$.executionSettings$getDbms(),
                    tempEmulationSchema = private$.executionSettings$tempEmulationSchema
                )

            pathway_result <- DatabaseConnector::querySql(connection, pathway_query)

            # Format and display
            cli::cli_h3("Build Summary")
            cli::cli_dl(c(
                "Total Rows" = format(summary_result$TOTAL_ROWS[1], big.mark = ","),
                "Unique Persons" = format(summary_result$UNIQUE_PERSONS[1], big.mark = ","),
                "Persons with Events" = format(summary_result$PERSONS_WITH_EVENTS[1], big.mark = ","),
                "Avg Events per Person" = as.character(summary_result$AVG_EVENTS_PER_PERSON[1]),
                "Unique Tx Lines" = as.character(summary_result$UNIQUE_LINES[1]),
                "Combination Events" = format(summary_result$COMBO_EVENTS[1], big.mark = ","),
                "Max Pathway Length" = as.character(summary_result$MAX_PATHWAY_LENGTH[1])
            ))

            cli::cli_h3("Pathway Length Distribution")
            cli::cli_verbatim(paste(capture.output(print(pathway_result)), collapse = "\n"))

            # Cache the results
            private$.lastSummary <- list(
                summary = summary_result,
                pathways = pathway_result
            )
        }
    ),
    active = list(
        txCohorts = function(value) {
            if (missing(value)) {
                return(private$.txCohorts)
            }
            checkmate::assertDataFrame(value, min.rows = 1)
            checkmate::assertNames(names(value), permutation.of = c("cohortId", "cohortLabel", "cohortType"))
            checkmate::assertSubset(unique(value$cohortType), choices = c("target", "event", "exit"))
            checkmate::assertTrue(sum(value$cohortType == "target") == 1)
            private$.txCohorts <- value
        },
        followUpWindow = function(value) {
            if (missing(value)) {
                return(private$.followUpWindow)
            }
            checkmate::assertClass(value, classes = "FollowUpWindow")
            private$.followUpWindow <- value
        },
        eraCollapseSettings = function(value) {
            if (missing(value)) {
                return(private$.eraCollapseSettings)
            }
            checkmate::assertClass(value, classes = "EraCollapseSettings")
            private$.eraCollapseSettings <- value
        },
        combinationTreatmentSettings = function(value) {
            if (missing(value)) {
                return(private$.combinationTreatmentSettings)
            }
            checkmate::assertClass(value, classes = "CombinationTreatmentSettings")
            private$.combinationTreatmentSettings <- value
        },
        pathwayOptions = function(value) {
            if (missing(value)) {
                return(private$.pathwayOptions)
            }
            checkmate::assertClass(value, classes = "PathwayOptions")
            private$.pathwayOptions <- value
        },
        txHistoryTableName = function(value) {
            if (missing(value)) {
                return(private$.txHistoryTable)
            }
            checkmate::assertString(value)
            private$.txHistoryTable <- value
        },
        executionSettings = function(value) {
            if (missing(value)) {
                return(private$.executionSettings)
            }
            checkmate::assertClass(value, classes = "ExecutionSettings")
            private$.executionSettings <- value
        }
    )
)

# TxHistory Support Classes ---------------

CombinationTreatmentSettings <- R6::R6Class(
    classname = "CombinationTreatmentSettings",
    public = list(
        initialize = function(minPostCombinationDuration = 30, combinationWindow = 30) {
            checkmate::assertIntegerish(minPostCombinationDuration, len = 1, lower = 0)
            private$.minPostCombinationDuration <- minPostCombinationDuration
            checkmate::assertIntegerish(combinationWindow, len = 1, lower = 0)
            private$.combinationWindow <- combinationWindow
        }
    ),
    private = list(
        .minPostCombinationDuration = NULL,
        .combinationWindow = NULL
    ),
    active = list(
        minPostCombinationDuration = function(value) {
            if (missing(value)) {
                return(private$.minPostCombinationDuration)
            }
            checkmate::assertIntegerish(value, len = 1, lower = 0)
            private$.minPostCombinationDuration <- value
        },
        combinationWindow = function(value) {
            if (missing(value)) {
                return(private$.combinationWindow)
            }
            checkmate::assertIntegerish(value, len = 1, lower = 0)
            private$.combinationWindow <- value
        }
    )
)

EraCollapseSettings <- R6::R6Class(
    classname = "EraCollapseSettings",
    public = list(
        initialize = function(eraCollapseSize= 30, minEraDuration = 0) {
            checkmate::assertIntegerish(eraCollapseSize, len = 1, lower = 0)
            private$.eraCollapseSize <- eraCollapseSize
            checkmate::assertIntegerish(minEraDuration, len = 1, lower = 0)
            private$.minEraDuration <- minEraDuration
        }
    ),
    private = list(
        .eraCollapseSize = NULL,
        .minEraDuration = NULL
    ),
    active = list(
        eraCollapseSize = function(value) {
            if (missing(value)) {
                return(private$.eraCollapseSize)
            }
            checkmate::assertIntegerish(value, len = 1, lower = 0)
            private$.eraCollapseSize <- value
        },
        minEraDuration = function(value) {
            if (missing(value)) {
                return(private$.minEraDuration)
            }
            checkmate::assertIntegerish(value, len = 1, lower = 0)
            private$.minEraDuration <- value
        }
    )
)

PathwayOptions <- R6::R6Class(
    classname = "PathwayOptions",
    public = list(
        initialize = function(maxPathwayLength = 5,
                              filterTreatments = "First") {
            checkmate::assertIntegerish(maxPathwayLength, len = 1, lower = 1)
            checkmate::assertChoice(filterTreatments, c("First", "All", "Changes"))                                
            private$.maxPathwayLength <- maxPathwayLength
            private$.filterTreatments <- filterTreatments
        }
    ),
    private = list(
        .maxPathwayLength = NULL,
        .filterTreatments = NULL
    ),
    active = list(
        maxPathwayLength = function(value) {
            if (missing(value)) {
                return(private$.maxPathwayLength)
            }
            checkmate::assertIntegerish(value, len = 1, lower = 1)
            private$.maxPathwayLength <- value
        },
        filterTreatments = function(value) {
            if (missing(value)) {
                return(private$.filterTreatments)
            }
            checkmate::assertChoice(value, c("First", "All", "Changes"))
            private$.filterTreatments <- value
        }
    )
)

FollowUpWindow <- R6::R6Class(
  classname = "FollowUpWindow",
  private = list(
    .startAnchor = NULL,
    .startDays = NULL,
    .endAnchor = NULL,
    .endDays = NULL
  ),
  public = list(
    initialize = function(
      startAnchor = "cohort_start_date",
      startDays = 0,
      endAnchor = "cohort_end_date",
      endDays = 0
    ) {
      # check inputs are valid
      checkmate::assert_choice(x = startAnchor, choices = c("cohort_start_date", "cohort_end_date"))
      checkmate::assert_choice(x = endAnchor, choices = c("cohort_start_date", "cohort_end_date"))
      checkmate::assert_integerish(x = startDays, len = 1)
      checkmate::assert_integerish(x = endDays, len = 1)

      # assign to private fields
      private$.startAnchor <- startAnchor
      private$.endAnchor <- endAnchor
      private$.startDays <- startDays
      private$.endDays <- endDays

    }

  ),
  active = list(
    startAnchor = function(value) {
      if (missing(value)) {
        return(private$.startAnchor)
      }
      checkmate::assert_choice(x = value, choices = c("start_date", "end_date"))
      private$.startAnchor <- value
    },
    endAnchor = function(value) {
      if (missing(value)) {
        return(private$.endAnchor)
      }
      checkmate::assert_choice(x = value, choices = c("start_date", "end_date"))
      private$.endAnchor <- value
    },
    startDays = function(value) {
      if (missing(value)) {
        return(private$.startDays)
      }
      checkmate::assert_integerish(x = value, len = 1)
      private$.startDays <- value
    },
    endDays = function(value) {
      if (missing(value)) {
        return(private$.endDays)
      }
      checkmate::assert_integerish(x = value, len = 1)
      private$.endDays <- value
    }
  )
)