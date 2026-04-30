# TxAnalysis Settings Classes ---------------

DurationSettings <- R6::R6Class(
    classname = "DurationSettings",
    public = list(
        initialize = function() {
            invisible(self)
        }
    )
)

PathwaySettings <- R6::R6Class(
    classname = "PathwaySettings",
    public = list(
        initialize = function(minFrequency = 1L, maxPathLength = 5L) {
            checkmate::assertIntegerish(minFrequency, len = 1, lower = 1)
            checkmate::assertIntegerish(maxPathLength, len = 1, lower = 1)
            private$.minFrequency  <- as.integer(minFrequency)
            private$.maxPathLength <- as.integer(maxPathLength)
        }
    ),
    private = list(
        .minFrequency  = NULL,
        .maxPathLength = NULL
    ),
    active = list(
        minFrequency = function(value) {
            if (missing(value)) return(private$.minFrequency)
            checkmate::assertIntegerish(value, len = 1, lower = 1)
            private$.minFrequency <- as.integer(value)
        },
        maxPathLength = function(value) {
            if (missing(value)) return(private$.maxPathLength)
            checkmate::assertIntegerish(value, len = 1, lower = 1)
            private$.maxPathLength <- as.integer(value)
        }
    )
)

AdherenceSettings <- R6::R6Class(
    classname = "AdherenceSettings",
    public = list(
        initialize = function(
            interruptionGaps  = c(30L, 60L, 9999L),
            adherenceAnchor   = "event_end"
        ) {
            checkmate::assertIntegerish(interruptionGaps, min.len = 1, lower = 1)
            checkmate::assertChoice(adherenceAnchor, c("event_end", "line_end"))
            private$.interruptionGaps <- as.integer(interruptionGaps)
            private$.adherenceAnchor  <- adherenceAnchor
        }
    ),
    private = list(
        .interruptionGaps = NULL,
        .adherenceAnchor  = NULL
    ),
    active = list(
        interruptionGaps = function(value) {
            if (missing(value)) return(private$.interruptionGaps)
            checkmate::assertIntegerish(value, min.len = 1, lower = 1)
            private$.interruptionGaps <- as.integer(value)
        },
        adherenceAnchor = function(value) {
            if (missing(value)) return(private$.adherenceAnchor)
            checkmate::assertChoice(value, c("event_end", "line_end"))
            private$.adherenceAnchor <- value
        }
    )
)

SituationCountSettings <- R6::R6Class(
    classname = "SituationCountSettings",
    public = list(
        initialize = function() {
            invisible(self)
        }
    )
)

SituationTimeSettings <- R6::R6Class(
    classname = "SituationTimeSettings",
    public = list(
        initialize = function(maxLine = 5L) {
            checkmate::assertIntegerish(maxLine, len = 1, lower = 1)
            private$.maxLine <- as.integer(maxLine)
        }
    ),
    private = list(
        .maxLine = NULL
    ),
    active = list(
        maxLine = function(value) {
            if (missing(value)) return(private$.maxLine)
            checkmate::assertIntegerish(value, len = 1, lower = 1)
            private$.maxLine <- as.integer(value)
        }
    )
)


# TxAnalysis Primary Class ---------------

TxAnalysis <- R6::R6Class(
    classname = "TxAnalysis",
    public = list(

        initialize = function(
            txHistory,
            analysisName,
            treatmentDuration         = NULL,
            treatmentPathways          = NULL,
            treatmentAdherence         = NULL,
            treatmentSituationsCounts  = NULL,
            treatmentSituationsTimes   = NULL,
            stratify                   = NULL,
            minCellCount               = 5L,
            customSql                  = NULL
        ) {
            checkmate::assertClass(txHistory, classes = "TxHistory")
            private$.txHistory <- txHistory

            checkmate::assertString(analysisName)
            private$.analysisName <- analysisName

            if (!is.null(treatmentDuration)) checkmate::assertClass(treatmentDuration, "DurationSettings")
            private$.treatmentDuration <- treatmentDuration

            if (!is.null(treatmentPathways)) checkmate::assertClass(treatmentPathways, "PathwaySettings")
            private$.treatmentPathways <- treatmentPathways

            if (!is.null(treatmentAdherence)) checkmate::assertClass(treatmentAdherence, "AdherenceSettings")
            private$.treatmentAdherence <- treatmentAdherence

            if (!is.null(treatmentSituationsCounts)) checkmate::assertClass(treatmentSituationsCounts, "SituationCountSettings")
            private$.treatmentSituationsCounts <- treatmentSituationsCounts

            if (!is.null(treatmentSituationsTimes)) checkmate::assertClass(treatmentSituationsTimes, "SituationTimeSettings")
            private$.treatmentSituationsTimes <- treatmentSituationsTimes

            if (!is.null(stratify)) {
                checkmate::assertSubset(stratify, choices = c("age", "gender", "indexYear"))
                # NOTE: stratification by age and gender joins to cdm_database_schema.person.
                # indexYear is derived from target_start_date (no join required).
                # Full stratified SQL injection is a planned feature - strata columns will be
                # added to each module query via a person_strata CTE in a future version.
                cli::cli_warn(
                    c(
                        "!" = "Stratification is configured but not yet fully implemented.",
                        "i" = "Results will be unstratified. Stratification support is planned for a future release."
                    )
                )
            }
            private$.stratify <- stratify

            checkmate::assertIntegerish(minCellCount, len = 1, lower = 0)
            private$.minCellCount <- as.integer(minCellCount)

            if (!is.null(customSql)) {
                checkmate::assertList(customSql, names = "named")
                for (nm in names(customSql)) {
                    checkmate::assertFileExists(customSql[[nm]], extension = "sql",
                        .var.name = paste0("customSql[['", nm, "']]"))
                }
            }
            private$.customSql <- customSql
            private$.results   <- list()
        },

        retrieve = function(force = FALSE) {
            private$check_built()

            if (length(private$.results) > 0 && !force) {
                cli::cli_warn(
                    "Results already retrieved. Use {.code retrieve(force = TRUE)} to re-run and overwrite."
                )
                return(invisible(self))
            }

            cli::cli_rule(left = "TxAnalysis: {private$.analysisName}")

            if (!is.null(private$.treatmentDuration)) {
                cli::cli_progress_step("Running: treatment duration")
                private$.results$duration <- private$run_duration()
            }

            if (!is.null(private$.treatmentPathways)) {
                cli::cli_progress_step("Running: treatment pathways")
                private$.results$pathways <- private$run_pathways()
            }

            if (!is.null(private$.treatmentAdherence)) {
                cli::cli_progress_step("Running: treatment adherence")
                private$.results$adherence <- private$run_adherence()
            }

            if (!is.null(private$.treatmentSituationsCounts)) {
                cli::cli_progress_step("Running: treatment situation counts")
                private$.results$situation_counts <- private$run_situation_counts()
            }

            if (!is.null(private$.treatmentSituationsTimes)) {
                cli::cli_progress_step("Running: treatment situation times")
                private$.results$situation_times <- private$run_situation_times()
            }

            exit_cohorts <- dplyr::filter(private$.txHistory$txCohorts, cohortType == "exit")

            if (nrow(exit_cohorts) > 0) {
                cli::cli_progress_step("Running: exit reasons")
                private$.results$exit_reasons <- private$run_exit_reasons()
            }

            if (!is.null(private$.customSql)) {
                for (nm in names(private$.customSql)) {
                    cli::cli_progress_step("Running: custom query {.val {nm}}")
                    private$.results[[paste0("custom_", nm)]] <- private$run_custom_sql(nm)
                }
            }

            cli::cli_rule()
            cli::cli_alert_success(
                "Retrieval complete. Use {.code $save()} to export or {.code $getX()} methods to access results."
            )
            invisible(self)
        },

        getDuration = function(force = FALSE) {
            private$check_built()

            if (is.null(private$.treatmentDuration)) {
                cli::cli_alert_info("Duration module not configured. Pass {.code treatmentDuration = createDurationSettings()} to {.code createTxAnalysis()}.")
                return(NULL)
            }

            if (!is.null(private$.results$duration) && !force) {
                return(private$.results$duration)
            }

            private$.results$duration <- private$run_duration()
            return(private$.results$duration)
        },

        getPathways = function(force = FALSE) {
            private$check_built()

            if (is.null(private$.treatmentPathways)) {
                cli::cli_alert_info("Pathways module not configured. Pass {.code treatmentPathways = createPathwaySettings()} to {.code createTxAnalysis()}.")
                return(NULL)
            }

            if (!is.null(private$.results$pathways) && !force) {
                return(private$.results$pathways)
            }

            private$.results$pathways <- private$run_pathways()
            return(private$.results$pathways)
        },

        getAdherence = function(force = FALSE) {
            private$check_built()

            if (is.null(private$.treatmentAdherence)) {
                cli::cli_alert_info("Adherence module not configured. Pass {.code treatmentAdherence = createAdherenceSettings()} to {.code createTxAnalysis()}.")
                return(NULL)
            }

            if (!is.null(private$.results$adherence) && !force) {
                return(private$.results$adherence)
            }

            private$.results$adherence <- private$run_adherence()
            return(private$.results$adherence)
        },

        getSituationCounts = function(force = FALSE) {
            private$check_built()

            if (is.null(private$.treatmentSituationsCounts)) {
                cli::cli_alert_info("Situation counts module not configured. Pass {.code treatmentSituationsCounts = createSituationCountSettings()} to {.code createTxAnalysis()}.")
                return(NULL)
            }

            if (!is.null(private$.results$situation_counts) && !force) {
                return(private$.results$situation_counts)
            }

            private$.results$situation_counts <- private$run_situation_counts()
            return(private$.results$situation_counts)
        },

        getSituationTimes = function(force = FALSE) {
            private$check_built()

            if (is.null(private$.treatmentSituationsTimes)) {
                cli::cli_alert_info("Situation times module not configured. Pass {.code treatmentSituationsTimes = createSituationTimeSettings()} to {.code createTxAnalysis()}.")
                return(NULL)
            }

            if (!is.null(private$.results$situation_times) && !force) {
                return(private$.results$situation_times)
            }

            private$.results$situation_times <- private$run_situation_times()
            return(private$.results$situation_times)
        },

        getExitReasons = function(force = FALSE) {
            private$check_built()

            exit_cohorts <- dplyr::filter(private$.txHistory$txCohorts, cohortType == "exit")

            if (nrow(exit_cohorts) == 0) {
                cli::cli_alert_info("No exit cohorts defined. Add rows with {.code cohortType = 'exit'} to txCohorts to enable this module.")
                return(NULL)
            }

            if (!is.null(private$.results$exit_reasons) && !force) {
                return(private$.results$exit_reasons)
            }

            private$.results$exit_reasons <- private$run_exit_reasons()
            return(private$.results$exit_reasons)
        },

        getCustomSql = function(name, force = FALSE) {
            private$check_built()

            if (is.null(private$.customSql) || !name %in% names(private$.customSql)) {
                cli::cli_abort("Custom SQL query {.val {name}} not found in configured custom queries.")
            }

            cache_key <- paste0("custom_", name)

            if (!is.null(private$.results[[cache_key]]) && !force) {
                return(private$.results[[cache_key]])
            }

            private$.results[[cache_key]] <- private$run_custom_sql(name)
            return(private$.results[[cache_key]])
        },

        save = function(outputPath = here::here()) {
            if (length(private$.results) == 0) {
                cli::cli_abort("No results to save. Call {.code $retrieve()} first.")
            }

            ts         <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
            output_dir <- fs::path(outputPath, paste0("tx_pathways_", ts))
            fs::dir_create(output_dir)

            for (nm in names(private$.results)) {
                if (!is.null(private$.results[[nm]])) {
                    file <- fs::path(output_dir, paste0(nm, ".csv"))
                    readr::write_csv(private$.results[[nm]], file)
                    cli::cli_alert_success("Saved {.file {file}}")
                }
            }

            settings_df <- private$build_settings_summary()
            readr::write_csv(settings_df, fs::path(output_dir, "settings_summary.csv"))
            cli::cli_alert_success("Saved {.file {fs::path(output_dir, 'settings_summary.csv')}}")

            cli::cli_alert_success("All results saved to {.file {output_dir}}")
            invisible(output_dir)
        },

        sankey = function() {
            if (!requireNamespace("networkD3", quietly = TRUE)) {
                cli::cli_abort("Package {.pkg networkD3} is required. Install with {.code install.packages('networkD3')}.")
            }
            if (!requireNamespace("tidyr", quietly = TRUE)) {
                cli::cli_abort("Package {.pkg tidyr} is required. Install with {.code install.packages('tidyr')}.")
            }

            pathways <- if (!is.null(private$.results$pathways)) private$.results$pathways else self$getPathways()

            if (is.null(pathways)) {
                cli::cli_abort("Pathways data not available. Configure {.code treatmentPathways} and call {.code $retrieve()} first.")
            }

            max_len    <- max(lengths(strsplit(pathways$PATH, " \\| ")))
            split_cols <- paste0("combo_name", seq_len(max_len))

            pathway_wide <- do.call(rbind, lapply(seq_len(nrow(pathways)), function(i) {
                parts              <- strsplit(pathways$PATH[i], " \\| ")[[1]]
                length(parts)      <- max_len
                as.data.frame(t(c(parts, pathways$FREQ[i])), stringsAsFactors = FALSE)
            }))
            names(pathway_wide)              <- c(split_cols, "n")
            pathway_wide$n                   <- as.integer(pathway_wide$n)

            links <- pathway_wide |>
                dplyr::mutate(row = dplyr::row_number()) |>
                tidyr::pivot_longer(
                    cols      = c(-row, -n),
                    names_to  = "column",
                    values_to = "source"
                ) |>
                dplyr::mutate(column = match(column, names(pathway_wide))) |>
                tidyr::drop_na(source) |>
                dplyr::mutate(source = paste0(source, "__", column)) |>
                dplyr::group_by(row) |>
                dplyr::mutate(target = dplyr::lead(source, order_by = column)) |>
                tidyr::drop_na(target, source) |>
                dplyr::group_by(source, target) |>
                dplyr::summarise(value = sum(n), .groups = "drop") |>
                dplyr::arrange(dplyr::desc(value))

            nodes        <- data.frame(name = unique(c(links$source, links$target)), stringsAsFactors = FALSE)
            links$source <- match(links$source, nodes$name) - 1L
            links$target <- match(links$target, nodes$name) - 1L
            nodes$name   <- sub("__[0-9]+$", "", nodes$name)

            networkD3::sankeyNetwork(
                Links      = links,
                Nodes      = nodes,
                Source     = "source",
                Target     = "target",
                Value      = "value",
                NodeID     = "name",
                fontSize   = 11,
                sinksRight = FALSE
            )
        },

        sunburst = function() {
            if (!requireNamespace("plotly", quietly = TRUE)) {
                cli::cli_abort("Package {.pkg plotly} is required. Install with {.code install.packages('plotly')}.")
            }

            pathways <- if (!is.null(private$.results$pathways)) private$.results$pathways else self$getPathways()

            if (is.null(pathways)) {
                cli::cli_abort("Pathways data not available. Configure {.code treatmentPathways} and call {.code $retrieve()} first.")
            }

            split_paths <- strsplit(pathways$PATH, " \\| ")

            rows <- lapply(seq_along(split_paths), function(i) {
                parts <- split_paths[[i]]
                freq  <- pathways$FREQ[i]
                do.call(rbind, lapply(seq_along(parts), function(j) {
                    data.frame(
                        ids     = paste(parts[1:j], collapse = " | "),
                        labels  = parts[j],
                        parents = if (j == 1L) "" else paste(parts[1:(j - 1L)], collapse = " | "),
                        values  = if (j == length(parts)) freq else 0L,
                        stringsAsFactors = FALSE
                    )
                }))
            })

            sunburst_df <- dplyr::bind_rows(rows) |>
                dplyr::group_by(ids, labels, parents) |>
                dplyr::summarise(values = sum(values), .groups = "drop")

            plotly::plot_ly(
                data         = sunburst_df,
                type         = "sunburst",
                ids          = ~ids,
                labels       = ~labels,
                parents      = ~parents,
                values       = ~values,
                branchvalues = "total"
            )
        },

        print = function(...) {
            cli::cli_rule(left = "TxAnalysis: {private$.analysisName}")

            module_map <- list(
                "Treatment Duration"    = list(key = "duration",        setting = private$.treatmentDuration),
                "Treatment Pathways"    = list(key = "pathways",        setting = private$.treatmentPathways),
                "Treatment Adherence"   = list(key = "adherence",       setting = private$.treatmentAdherence),
                "Situation Counts"      = list(key = "situation_counts", setting = private$.treatmentSituationsCounts),
                "Situation Times"       = list(key = "situation_times",  setting = private$.treatmentSituationsTimes)
            )

            has_exit <- any(private$.txHistory$txCohorts$cohortType == "exit")

            cli::cli_h3("Modules")

            for (nm in names(module_map)) {
                mod    <- module_map[[nm]]
                status <- if (!is.null(private$.results[[mod$key]])) " [retrieved]" else ""

                if (!is.null(mod$setting)) {
                    cli::cli_bullets(c("v" = paste0(nm, status)))
                } else {
                    cli::cli_bullets(c("x" = paste0(nm, " [not configured]")))
                }
            }

            if (has_exit) {
                status <- if (!is.null(private$.results$exit_reasons)) " [retrieved]" else ""
                cli::cli_bullets(c("v" = paste0("Exit Reasons", status)))
            } else {
                cli::cli_bullets(c("x" = "Exit Reasons [no exit cohorts in txCohorts]"))
            }

            if (!is.null(private$.customSql)) {
                for (nm in names(private$.customSql)) {
                    cache_key <- paste0("custom_", nm)
                    status    <- if (!is.null(private$.results[[cache_key]])) " [retrieved]" else ""
                    cli::cli_bullets(c("v" = paste0("Custom: ", nm, status)))
                }
            }

            cli::cli_h3("Settings")
            cli::cli_dl(c(
                "Min Cell Count" = as.character(private$.minCellCount),
                "Stratify"       = if (is.null(private$.stratify)) "None" else paste(private$.stratify, collapse = ", ")
            ))

            cli::cli_rule()
            invisible(self)
        }
    ),

    private = list(
        .txHistory                = NULL,
        .analysisName             = NULL,
        .treatmentDuration        = NULL,
        .treatmentPathways        = NULL,
        .treatmentAdherence       = NULL,
        .treatmentSituationsCounts = NULL,
        .treatmentSituationsTimes = NULL,
        .stratify                 = NULL,
        .minCellCount             = 5L,
        .customSql                = NULL,
        .results                  = list(),

        check_built = function() {
            if (!private$.txHistory$hasBeenBuilt()) {
                cli::cli_abort(
                    c(
                        "Treatment history table has not been built.",
                        "i" = "Call {.code txHistory$buildTreatmentHistory()} before running analysis."
                    )
                )
            }
        },

        render_translate = function(sql_file, ...) {
            es       <- private$.txHistory$executionSettings
            sql_path <- fs::path_package(package = "TxPathways", paste0("sql/", sql_file))
            sql      <- readr::read_file(sql_path)

            sql |>
                SqlRender::render(
                    work_database_schema = es$workDatabaseSchema,
                    tx_history_table     = private$.txHistory$txHistoryTableName,
                    min_cell_count       = private$.minCellCount,
                    ...
                ) |>
                SqlRender::translate(
                    targetDialect        = es$getDbms(),
                    tempEmulationSchema  = es$tempEmulationSchema
                )
        },

        resolve_labels = function(df) {
            col <- intersect(c("COMBO_LABEL", "combo_label"), names(df))

            if (length(col) == 0L) {
                return(df)
            }

            col     <- col[1L]
            cohorts <- private$.txHistory$txCohorts
            lut     <- stats::setNames(cohorts$cohortLabel, as.character(cohorts$cohortId))

            df[[col]] <- vapply(df[[col]], function(label) {
                if (is.na(label) || label == "Overall") return(label)
                parts    <- strsplit(as.character(label), "\\+")[[1]]
                resolved <- vapply(parts, function(p) if (p %in% names(lut)) lut[[p]] else p, character(1))
                paste(resolved, collapse = "+")
            }, character(1))

            df
        },

        resolve_path_labels = function(df) {
            col <- intersect(c("PATH", "path"), names(df))

            if (length(col) == 0L) {
                return(df)
            }

            col     <- col[1L]
            cohorts <- private$.txHistory$txCohorts
            lut     <- stats::setNames(cohorts$cohortLabel, as.character(cohorts$cohortId))

            df[[col]] <- vapply(df[[col]], function(path) {
                events   <- strsplit(path, " \\| ")[[1]]
                resolved <- vapply(events, function(event) {
                    ids      <- strsplit(event, "\\+")[[1]]
                    mapped   <- vapply(ids, function(id) if (id %in% names(lut)) lut[[id]] else id, character(1))
                    paste(mapped, collapse = "+")
                }, character(1))
                paste(resolved, collapse = " | ")
            }, character(1))

            df
        },

        run_duration = function() {
            es <- private$.txHistory$executionSettings
            es$connect()
            connection <- es$getConnection()
            withr::defer(es$disconnect())

            sql    <- private$render_translate("duration_summary.sql")
            result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
            result <- private$resolve_labels(result)
            tibble::as_tibble(result)
        },

        run_pathways = function() {
            es <- private$.txHistory$executionSettings
            es$connect()
            connection <- es$getConnection()
            withr::defer(es$disconnect())

            sql <- private$render_translate(
                "pathway_summary.sql",
                path_agg_sql    = path_agg_sql(es$getDbms()),
                min_frequency   = private$.treatmentPathways$minFrequency,
                max_path_length = private$.treatmentPathways$maxPathLength
            )
            result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
            result <- private$resolve_path_labels(result)
            tibble::as_tibble(result)
        },

        run_adherence = function() {
            es <- private$.txHistory$executionSettings
            es$connect()
            connection <- es$getConnection()
            withr::defer(es$disconnect())

            sql    <- private$render_translate("tx_adherence.sql")
            result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
            result <- private$resolve_labels(result)

            # Classify gap_days into user-defined interruption buckets (in R)
            breaks <- c(-Inf, private$.treatmentAdherence$interruptionGaps)
            labels <- paste0("<=", private$.treatmentAdherence$interruptionGaps, "d")
            gap_col <- intersect(c("MEDIAN_GAP_DAYS", "median_gap_days"), names(result))

            if (length(gap_col) > 0L) {
                result$gap_category <- cut(
                    result[[gap_col[1L]]],
                    breaks = breaks,
                    labels = labels,
                    right  = TRUE
                )
            }

            tibble::as_tibble(result)
        },

        run_situation_counts = function() {
            es <- private$.txHistory$executionSettings
            es$connect()
            connection <- es$getConnection()
            withr::defer(es$disconnect())

            sql    <- private$render_translate("tx_situation_counts.sql")
            result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
            tibble::as_tibble(result)
        },

        run_situation_times = function() {
            es <- private$.txHistory$executionSettings
            es$connect()
            connection <- es$getConnection()
            withr::defer(es$disconnect())

            sql <- private$render_translate(
                "tx_situation_times.sql",
                max_line = private$.treatmentSituationsTimes$maxLine
            )
            result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
            result <- private$resolve_labels(result)
            tibble::as_tibble(result)
        },

        run_exit_reasons = function() {
            es           <- private$.txHistory$executionSettings
            exit_cohorts <- dplyr::filter(private$.txHistory$txCohorts, cohortType == "exit")
            exit_lut     <- stats::setNames(exit_cohorts$cohortLabel, as.character(exit_cohorts$cohortId))

            es$connect()
            connection <- es$getConnection()
            withr::defer(es$disconnect())

            sql <- private$render_translate(
                "tx_exit_reasons.sql",
                cohort_table     = es$cohortTable,
                exit_cohort_ids  = paste(exit_cohorts$cohortId, collapse = ",")
            )
            result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)

            # Resolve exit cohort ID to label; NULL -> "End of Observation"
            id_col <- intersect(c("EXIT_COHORT_ID", "exit_cohort_id"), names(result))

            if (length(id_col) > 0L) {
                col <- id_col[1L]
                result$exit_reason <- ifelse(
                    is.na(result[[col]]),
                    "End of Observation",
                    vapply(as.character(result[[col]]), function(id) {
                        if (id %in% names(exit_lut)) exit_lut[[id]] else id
                    }, character(1))
                )
                result[[col]] <- NULL
            }

            tibble::as_tibble(result)
        },

        run_custom_sql = function(name) {
            # TODO: full validation in next version (token checks, dry-run, dialect testing)
            cli::cli_alert_info("Custom SQL {.val {name}}: executing with minimal validation.")

            sql_path <- private$.customSql[[name]]
            es       <- private$.txHistory$executionSettings

            sql <- readr::read_file(sql_path) |>
                SqlRender::render(
                    work_database_schema = es$workDatabaseSchema,
                    tx_history_table     = private$.txHistory$txHistoryTableName
                ) |>
                SqlRender::translate(
                    targetDialect       = es$getDbms(),
                    tempEmulationSchema = es$tempEmulationSchema
                )

            es$connect()
            connection <- es$getConnection()
            withr::defer(es$disconnect())

            tryCatch(
                {
                    result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
                    tibble::as_tibble(result)
                },
                error = function(e) {
                    cli::cli_abort(
                        c(
                            "Custom SQL query {.val {name}} failed.",
                            "x" = "{conditionMessage(e)}",
                            "i" = "Ensure the SQL file uses {.code @work_database_schema} and {.code @tx_history_table} as parameters."
                        )
                    )
                }
            )
        },

        build_settings_summary = function() {
            rows <- list()

            add_row <- function(section, key, value) {
                rows[[length(rows) + 1L]] <<- data.frame(
                    section = section,
                    key     = key,
                    value   = as.character(value),
                    stringsAsFactors = FALSE
                )
            }

            add_row("analysis", "analysisName",  private$.analysisName)
            add_row("analysis", "minCellCount",   private$.minCellCount)
            add_row("analysis", "stratify",       paste(if (is.null(private$.stratify)) "none" else private$.stratify, collapse = ", "))
            add_row("analysis", "retrievedAt",    format(Sys.time()))

            th <- private$.txHistory
            add_row("txHistory", "txHistoryTable",            th$txHistoryTableName)
            add_row("txHistory", "startAnchor",               th$followUpWindow$startAnchor)
            add_row("txHistory", "startDays",                 th$followUpWindow$startDays)
            add_row("txHistory", "endAnchor",                 th$followUpWindow$endAnchor)
            add_row("txHistory", "endDays",                   th$followUpWindow$endDays)
            add_row("txHistory", "eraCollapseSize",           th$eraCollapseSettings$eraCollapseSize)
            add_row("txHistory", "minEraDuration",            th$eraCollapseSettings$minEraDuration)
            add_row("txHistory", "combinationWindow",         th$combinationTreatmentSettings$combinationWindow)
            add_row("txHistory", "minPostCombinationDuration", th$combinationTreatmentSettings$minPostCombinationDuration)
            add_row("txHistory", "maxPathwayLength",          th$pathwayOptions$maxPathwayLength)
            add_row("txHistory", "filterTreatments",          th$pathwayOptions$filterTreatments)

            if (!is.null(private$.treatmentPathways)) {
                add_row("pathwaySettings", "minFrequency",  private$.treatmentPathways$minFrequency)
                add_row("pathwaySettings", "maxPathLength", private$.treatmentPathways$maxPathLength)
            }

            if (!is.null(private$.treatmentAdherence)) {
                add_row("adherenceSettings", "interruptionGaps", paste(private$.treatmentAdherence$interruptionGaps, collapse = ","))
                add_row("adherenceSettings", "adherenceAnchor",  private$.treatmentAdherence$adherenceAnchor)
            }

            if (!is.null(private$.treatmentSituationsTimes)) {
                add_row("situationTimeSettings", "maxLine", private$.treatmentSituationsTimes$maxLine)
            }

            dplyr::bind_rows(rows)
        }
    )
)
