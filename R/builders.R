

createPathwayOptions <- function(
    maxPathwayLength  = 5, 
    filterTreatments = c("First", "All", "Changes")
) {
    po <- PathwayOptions$new(maxPathwayLength = maxPathwayLength, filterTreatments = filterTreatments)
    return(po)
}

createEraCollapseSettings <- function(
    eraCollapseSize = 30, 
    minEraDuration = 0
) {
    ecs <- EraCollapseSettings$new(minEraDuration = minEraDuration)
    return(ecs)
}

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

createTxCohorts <- function(cohortIds, cohortLabels, cohortTypes) {
    txCohorts <- data.frame(
        cohortId = cohortIds,
        cohortLabel = cohortLabels,
        cohortType = cohortTypes
    )
    return(txCohorts)
}


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


# TxAnalysis Builders ---------------

#' Create a TxAnalysis object
#' @param txHistory A TxHistory object (must have buildTreatmentHistory() called first before retrieve())
#' @param analysisName A string label for this analysis
#' @param treatmentDuration A DurationSettings object, or NULL to skip
#' @param treatmentPathways A PathwaySettings object, or NULL to skip
#' @param treatmentAdherence An AdherenceSettings object, or NULL to skip
#' @param treatmentSituationsCounts A SituationCountSettings object, or NULL to skip
#' @param treatmentSituationsTimes A SituationTimeSettings object, or NULL to skip
#' @param stratify Character vector of strata: "age", "gender", "indexYear"
#' @param minCellCount Integer; suppress output with counts below this threshold
#' @param customSql Named list of SQL file paths to execute against the tx_history table
#' @return A TxAnalysis R6 object
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

#' Create DurationSettings for treatment duration analysis
#' @return A DurationSettings R6 object
#' @export
createDurationSettings <- function() {
    DurationSettings$new()
}

#' Create PathwaySettings for treatment pathway frequency analysis
#' @param minFrequency Integer; minimum frequency to include a pathway in results
#' @param maxPathLength Integer; maximum number of treatment lines per pathway string
#' @return A PathwaySettings R6 object
#' @export
createPathwaySettings <- function(minFrequency = 1L, maxPathLength = 5L) {
    PathwaySettings$new(minFrequency = minFrequency, maxPathLength = maxPathLength)
}

#' Create AdherenceSettings for treatment adherence analysis
#' @param interruptionGaps Integer vector of gap thresholds (days) for categorising interruptions
#' @param adherenceAnchor How to anchor the gap measurement: "event_end" or "line_end"
#'   "event_end"  - gap measured from event_end_date to next event_start_date
#'   "line_end"   - if event continues into a combination, use the combination end as the line end
#' @return An AdherenceSettings R6 object
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

#' Create SituationCountSettings for treatment situation count analysis
#' @return A SituationCountSettings R6 object
#' @export
createSituationCountSettings <- function() {
    SituationCountSettings$new()
}

#' Create SituationTimeSettings for treatment situation time analysis
#' @param maxLine Integer; maximum therapy line to include in "Time to Therapy Line" output
#' @return A SituationTimeSettings R6 object
#' @export
createSituationTimeSettings <- function(maxLine = 5L) {
    SituationTimeSettings$new(maxLine = maxLine)
}