# howMany2 ----------------

## Utilities -----------------------

get_denominator <- function(th) {
  nn <- th |>
    dplyr::distinct(subject_id) |>
    nrow()
  return(nn)
}

summarize_scenarios_count <- function(th) {

  # summarize by the seq gap and event order
  byEventBySeq <- th |>
    dplyr::group_by(seqGap, eventOrder) |>
    dplyr::count() |>
    dplyr::ungroup()

  # summarize by just the seq gap (any drug)
  bySeq <- th |>
    dplyr::group_by(seqGap) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      eventOrder = "any event",
      .before = 2
    )

  # summarize by any event order
  byEvent <- th |>
    dplyr::group_by(eventOrder) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      seqGap = "any sequence",
      .before = 1
    )

  tb <- dplyr::bind_rows(
    byEvent,
    bySeq,
    byEventBySeq
  )

  return(tb)

}

## How Many functions -------------------------

#' How many patients had one treatment era
#' @description
#' This function counts how many people only had one treatment era.
#' Does not count persons who have multiple eras of the same treatment.
#'
#' @param th the treatment history table
#' @return a tibble summarizing the number of persons and percentage with a single treatment line
#' @export
howManyOneTx <- function(th) {

  #create denominator
  nn <- get_denominator(th)

  #find those who only have single treatment
  single_era_ids <- single_era(th)

  # find persons with single era in th
  t1 <- th |>
    dplyr::filter(
      subject_id %in% single_era_ids
    )

  # find which lines had one tx
  byLine <- t1 |>
    dplyr::group_by(event_cohort_name) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(
      name = event_cohort_name
    )

  # of any line who had one tx
  anyLine <- t1 |>
    dplyr::count() |>
    dplyr::mutate(
      name = "any",
      .before = 1
    )

  # build summary table
  tb <- dplyr::bind_rows(
    anyLine,
    byLine
  ) |>
    dplyr::mutate(
      group = "Single Treatment",
      .before = 1
    ) |>
    dplyr::mutate(
      pct = (n / nn) * 100
    )

  return(tb)
}

#' How many patients have multiple treatments
#' @param th the treatment history table
#' @return a tibble summarizing the number of persons and percentage with a treatment sequence and event order
#' @export
howManyMultiTx <- function(th) {

  #create denominator
  nn <- get_denominator(th)

  # find persons with mult eras and lag them
  t1 <- lag_events(th)


  #summarize
  tb <- summarize_scenarios_count(t1) |>
    dplyr::mutate(
      group = "Next Treatment",
      .before = 1
    ) |>
    dplyr::mutate(
      pct = (n / nn) * 100
    )

  return(tb)
}

#' How many patients interrupt treatment
#' @param th the treatment history table
#' @param breaks interruption breaks
#' @return a tibble summarizing the number of persons and percentage with a treatment sequence and event order by interruption
#' @export
howManyInterruptTx <- function(th, breaks = c(30, 60, 9999)) {
  if (th$query_method[1] == 'atlas') {
    stop("ATLAS Pathways method is not suitable for this analysis as end dates are collapsed.")
  }
  #create denominator
  nn <- get_denominator(th)

  # find persons with mult eras and lag them
  t1 <- lag_events(th)


  # get those who repeat and sort into interruption groups
  t2 <- t1 |>
    dplyr::filter(
      event_cohort_name == prev_drug
    ) |>
    dplyr::mutate(
      interruption = cut(duration, breaks = breaks)
    )

  # summarize by event and seq order and interruption
  byEventBySeq <- t2 |>
    dplyr::group_by(seqGap, eventOrder, interruption) |>
    dplyr::count() |>
    dplyr::ungroup()

  # summarize by any seq order
  bySeq <- t2 |>
    dplyr::group_by(seqGap, interruption) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      eventOrder = "any event",
      .before = 2
    )
  # summarize by any event order
  byEvent <- t2 |>
    dplyr::group_by(eventOrder, interruption) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      seqGap = "any sequence",
      .before = 1
    )

  # return summary table
  tb <- dplyr::bind_rows(
    byEvent,
    bySeq,
    byEventBySeq
  ) |>
    dplyr::mutate(
      group = "Treatment Interruption",
      .before = 1
    ) |>
    dplyr::mutate(
      pct = (n /nn) * 100
    )

  return(tb)
}

#' How many patients have combination treatments
#' @param th the treatment history table
#' @return a tibble summarizing the number of persons and percentage with a treatment combinations
#' @export
howManyComboTx <- function(th) {

  #create denominator
  nn <- get_denominator(th)

  t1 <- combo_events(th)

  #summarize
  tb <- t1 |>
    dplyr::group_by(eventOrder) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      group = "Treatment Combinations",
      .before = 1
    ) |>
    dplyr::mutate(
      pct = (n / nn) * 100,
      name = dplyr::case_when(
        grepl("NA", eventOrder) ~ gsub("NA -> ", "", eventOrder),
        TRUE ~ eventOrder
      )
    ) |>
    dplyr::select(group, name, n, pct)
  return(tb)

}

#' How many patients have discontinuation
#' @param th the treatment history table
#' @param days the maximum number of days before stopping to consider an event a discontinuation
#' @return a tibble summarizing the number of persons and percentage with a discontinuation at 365d
#' @export
howManyDiscontinue <- function(th, days = 365) {
  if (th$query_method[1] == 'atlas') {
    stop("ATLAS Pathways method is not suitable for this analysis as end dates are collapsed.")
  }
  #create denominator
  nn <- get_denominator(th)

  # find persons with mult eras and lag them
  t1 <- find_discontinuation(th) |>
    dplyr::mutate(
      diff = as.integer(event_end - event_start),
      discontinue = dplyr::if_else(diff <= days, 1, 0)
    ) |>
    dplyr::filter(discontinue == 1)

  # find which lines had one tx
  byLine <- t1 |>
    dplyr::group_by(event_cohort_name) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(
      name = event_cohort_name
    )

  # of any line who had one tx
  anyLine <- t1 |>
    dplyr::count() |>
    dplyr::mutate(
      name = "any",
      .before = 1
    )

  # build summary table
  tb <- dplyr::bind_rows(
    anyLine,
    byLine
  ) |>
    dplyr::mutate(
      group = "Treatment Discontinuation",
      .before = 1
    ) |>
    dplyr::mutate(
      pct = (n / nn) * 100
    )

  return(tb)

}

#' How many lines of treatment do patients have
#' @param th the treatment history table
#' @export
howManyTxLines <- function(th) {
  # get number of unique persons
  nn <- th |>
    dplyr::distinct(subject_id) |>
    nrow()

  # count persons by number of treatment lines
  byLineCount <- th |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(linesPerSubject = dplyr::n_distinct(event_seq)) |>
    dplyr::ungroup() |>
    dplyr::group_by(linesPerSubject) |>
    dplyr::summarise(n = dplyr::n_distinct(subject_id)) |>
    dplyr::mutate(
      pct = (n / nn) * 100
    )

  byLineCount <- tibble::as_tibble(byLineCount)

  return(byLineCount)
}

#' How many had each treatment in a given line
#' @param th the treatment history table
#' @param txLine the line of treatment (integer >= 1)
#' @export
howManyWithEachTxInLine <- function(th, txLine) {
  # get number of unique persons with treatment in the specified line
  nn <- th |>
    dplyr::filter(event_seq == txLine) |>
    dplyr::distinct(subject_id) |>
    nrow()

  # count persons with each tx in the specified line of treatment
  line <- th |>
    dplyr::filter(event_seq == txLine) |>
    dplyr::group_by(event_cohort_name) |>
    dplyr::summarise(n = dplyr::n_distinct(subject_id)) |>
    dplyr::mutate(
      pct = (n / nn) * 100
    )

  return(line)
}
