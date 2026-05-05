
# builders.R — Factory Functions (Public API)
# ----------------------------------------------------------------------

# TxHistory Builders ----------------------------------------------------------

#' @title Create a PathwayOptions object
#'
#' @param maxPathwayLength Integer. Maximum treatment lines per person (default 5).
#' @param filterTreatments Character. Which lines to retain: `"First"`, `"All"`, or
#'   `"Changes"` (default `"First"`).
#' @returns A [PathwayOptions] R6 object.
#' @export
createPathwayOptions <- function(
    maxPathwayLength  = 5, 
    filterTreatments = c("First", "All", "Changes")
) {
    po <- PathwayOptions$new(maxPathwayLength = maxPathwayLength, filterTreatments = filterTreatments)
    return(po)
}

#' @title Create an EraCollapseSettings object
#'
#' @param eraCollapseSize Integer. Maximum gap (days) between same-drug eras
#'   that should be merged into a single era (default 30).
#' @param minEraDuration Integer. Collapsed eras shorter than this (days) are
#'   dropped (default 0).
#' @returns An [EraCollapseSettings] R6 object.
#' @export
createEraCollapseSettings <- function(
    eraCollapseSize = 30, 
    minEraDuration = 0
) {
    ecs <- EraCollapseSettings$new(minEraDuration = minEraDuration)
    return(ecs)
}

#' @title Create a CombinationTreatmentSettings object
#'
#' @param minPostCombinationDuration Integer. Minimum days a combination segment
#'   must persist to be retained (default 30).
#' @param combinationWindow Integer. Minimum overlap (days) for two events to
#'   be classified as a combination (default 30).
#' @returns A [CombinationTreatmentSettings] R6 object.
#' @export
createCombinationTreatmentSettings <- function(
    minPostCombinationDuration = 30,
    combinationWindow = 30
) {
    ctcs <- CombinationTreatmentSettings$new(
        minPostCombinationDuration = minPostCombinationDuration,
        combinationWindow = combinationWindow
    )
    return(ctcs)
}

#' @title Create a FollowUpWindow object
#'
#' @param startAnchor Character. Anchor for the follow-up window start:
#'   `"cohort_start_date"` or `"cohort_end_date"` (default `"cohort_start_date"`).
#' @param startDays Integer. Days offset from the start anchor (default 0).
#' @param endAnchor Character. Anchor for the follow-up window end:
#'   `"cohort_start_date"` or `"cohort_end_date"` (default `"cohort_end_date"`).
#' @param endDays Integer. Days offset from the end anchor (default 0).
#' @returns A [FollowUpWindow] R6 object.
#' @export
createFollowUpWindow <- function(
    startAnchor = c("cohort_start_date", "cohort_end_date"), 
    startDays = 0, 
    endAnchor = c("cohort_start_date", "cohort_end_date"), 
    endDays = 0
) {
    fuw <- FollowUpWindow$new(
        startAnchor = startAnchor, 
        startDays = startDays, 
        endAnchor = endAnchor, 
        endDays = endDays
    )
    return(fuw)
}

#' @title Create a txCohorts data frame
#'
#' @param cohortIds Integer vector. Cohort definition IDs.
#' @param cohortLabels Character vector. Human-readable cohort names.
#' @param cohortTypes Character vector. Cohort types: `"target"`, `"event"`,
#'   or `"exit"`. Exactly one row must have `"target"`.
#' @returns A data frame with columns `cohortId`, `cohortLabel`, `cohortType`.
#' @export
createTxCohorts <- function(cohortIds, cohortLabels, cohortTypes) {
    txCohorts <- data.frame(
        cohortId = cohortIds,
        cohortLabel = cohortLabels,
        cohortType = cohortTypes
    )
    return(txCohorts)
}


#' @title Create treatment history settings
#'
#' @description
#' Convenience builder that wires together all settings needed to instantiate
#' a `TxHistory` R6 object. Any optional settings left `NULL` will be created
#' with sensible defaults.
#'
#' @param txCohorts A data frame from [createTxCohorts()].
#' @param executionSettings An `ExecutionSettings` object (from the Picard
#'   package) providing database connection details.
#' @param txHistoryTable Character. Name for the output treatment history table
#'   (default `"tx_history"`).
#' @param followUpWindow A [FollowUpWindow] object, or `NULL` for default.
#' @param eraCollapseSettings An [EraCollapseSettings] object, or `NULL` for default.
#' @param combinationTreatmentSettings A [CombinationTreatmentSettings] object,
#'   or `NULL` for default.
#' @param pathwayOptions A [PathwayOptions] object, or `NULL` for default.
#' @returns A [TxHistory] R6 object, ready for `$buildTreatmentHistory()`.
#' @export
createTreatmentHistorySettings <- function(
    txCohorts,
    executionSettings,
    txHistoryTable = "tx_history",
    followUpWindow = NULL,
    eraCollapseSettings = NULL,
    combinationTreatmentSettings = NULL,
    pathwayOptions = NULL
) {
    # if followUpWindow, eraCollapseSettings, combinationTreatmentSettings, or pathwayOptions are NULL, create with defaults
    if (is.null(followUpWindow)) {
        followUpWindow <- createFollowUpWindow()
    }

    if (is.null(eraCollapseSettings)) {
        eraCollapseSettings <- createEraCollapseSettings()
    }

    if (is.null(combinationTreatmentSettings)) {
        combinationTreatmentSettings <- createCombinationTreatmentSettings()
    }

    if (is.null(pathwayOptions)) {
        pathwayOptions <- createPathwayOptions()
    }

    # initialize TxHistory object
    ths <- TxHistory$new(
        txCohorts = txCohorts,
        followUpWindow = followUpWindow,
        eraCollapseSettings = eraCollapseSettings,
        combinationTreatmentSettings = combinationTreatmentSettings,
        pathwayOptions = pathwayOptions,
        executionSettings = executionSettings,
        txHistoryTable = txHistoryTable
    )
    return(ths)
}


# TxAnalysis Builders ---------------------------------------------------------

#' @title Create a TxAnalysis object
#'
#' @param txHistory A [TxHistory] object. Must have `buildTreatmentHistory()` called first.
#' @param analysisName Character. A label for this analysis run.
#' @param treatmentDuration A [DurationSettings] object, or `NULL` to skip.
#' @param treatmentPathways A [PathwaySettings] object, or `NULL` to skip.
#' @param treatmentAdherence An [AdherenceSettings] object, or `NULL` to skip.
#' @param treatmentSituationsCounts A [SituationCountSettings] object, or `NULL` to skip.
#' @param treatmentSituationsTimes A [SituationTimeSettings] object, or `NULL` to skip.
#' @param stratify Character vector. Strata: `"age"`, `"gender"`, `"indexYear"`.
#'   (Note: stratification is configured but not yet fully implemented.)
#' @param minCellCount Integer. Minimum cell count for result suppression (default 5).
#' @param customSql Named list. Each element is a path to a `.sql` file to execute
#'   against the `tx_history` table.
#' @returns A [TxAnalysis] R6 object.
#' @export
createTxAnalysis <- function(
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
    TxAnalysis$new(
        txHistory                 = txHistory,
        analysisName              = analysisName,
        treatmentDuration         = treatmentDuration,
        treatmentPathways          = treatmentPathways,
        treatmentAdherence         = treatmentAdherence,
        treatmentSituationsCounts  = treatmentSituationsCounts,
        treatmentSituationsTimes   = treatmentSituationsTimes,
        stratify                   = stratify,
        minCellCount               = minCellCount,
        customSql                  = customSql
    )
}

#' @title Create DurationSettings
#'
#' @returns A [DurationSettings] R6 object.
#' @export
createDurationSettings <- function() {
    DurationSettings$new()
}

#' @title Create PathwaySettings
#'
#' @param minFrequency Integer. Minimum frequency to include a pathway in results (default 1).
#' @param maxPathLength Integer. Maximum number of treatment lines per pathway string (default 5).
#' @returns A [PathwaySettings] R6 object.
#' @export
createPathwaySettings <- function(minFrequency = 1L, maxPathLength = 5L) {
    PathwaySettings$new(minFrequency = minFrequency, maxPathLength = maxPathLength)
}

#' @title Create AdherenceSettings
#'
#' @param interruptionGaps Integer vector. Gap thresholds (days) for categorising
#'   interruptions (default `c(30L, 60L, 9999L)`).
#' @param adherenceAnchor Character. How to anchor the gap measurement: `"event_end"`
#'   or `"line_end"` (default `"event_end"`).
#' @returns An [AdherenceSettings] R6 object.
#' @export
createAdherenceSettings <- function(
    interruptionGaps = c(30L, 60L, 9999L),
    adherenceAnchor  = "event_end"
) {
    AdherenceSettings$new(
        interruptionGaps = interruptionGaps,
        adherenceAnchor  = adherenceAnchor
    )
}

#' @title Create SituationCountSettings
#'
#' @returns A [SituationCountSettings] R6 object.
#' @export
createSituationCountSettings <- function() {
    SituationCountSettings$new()
}

#' @title Create SituationTimeSettings
#'
#' @param maxLine Integer. Maximum therapy line to include in "Time to Therapy Line"
#'   output (default 5).
#' @returns A [SituationTimeSettings] R6 object.
#' @export
createSituationTimeSettings <- function(maxLine = 5L) {
    SituationTimeSettings$new(maxLine = maxLine)
}