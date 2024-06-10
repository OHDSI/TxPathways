# HowLong ------------------
# Functions to summarize time to certain txpat events

# th <- fs::path_package("TxPathways", "data/treatmentHistory.csv") |>
#   readr::read_csv(show_col_types = FALSE)

summarize_time <- function(dt) {
  tb <- dt |>
    dplyr::summarize(
      n = dplyr::n_distinct(subject_id),
      mean = mean(duration),
      sd = sd(duration),
      minValue = min(duration),
      p25Value = quantile(duration, probs = c(0.25)),
      medianValue = median(duration),
      p75Value = quantile(duration, probs = c(0.75)),
      maxValue = max(duration)
    )
  return(tb)
}

summarize_scenarios_time <- function(th) {

  byEventBySeq <- th |>
    dplyr::group_by(seqGap, eventOrder) |>
    summarize_time() |>
    dplyr::ungroup()


  byEvent <- th |>
    dplyr::group_by(eventOrder) |>
    summarize_time() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      seqGap = "any",
      .before = 1
    )
  bySeq <- th |>
    dplyr::group_by(seqGap) |>
    summarize_time() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      eventOrder = "any",
      .before = 2
    )

  tb <- dplyr::bind_rows(
    byEvent,
    bySeq,
    byEventBySeq
  )

  tb <- dplyr::bind_rows(
    bySeq,
    byEvent,
    byEventBySeq
  )

  return(tb)

}


#' How long until first treatment
#' @param th the treatment history table
#' @return a tibble summarizing the duration until first treatment using min, p25
#' p50, p75 and max values
#' @export
howLongFirstTx <- function(th) {

  # determine the duration of time until first treatment
  t1 <- th |>
    dplyr::filter(event_seq == 1) |>
    dplyr::mutate(
      duration = event_start - target_start,
      duration = as.integer(duration)
    ) |>
    dplyr::select(
      subject_id, event_seq, target_start, event_cohort_name, event_start, duration
    )

  # summarize for all
  allLines <- t1 |>
    summarize_time() |>
    dplyr::mutate(
      name = "any",
      .before = 1
    )

  # summarize by line
  byLine <- t1 |>
    dplyr::group_by(event_cohort_name) |>
    summarize_time() |>
    dplyr::rename(
      name = event_cohort_name
    ) |>
      dplyr::select(name, n:maxValue)


  tb <- dplyr::bind_rows(allLines, byLine) |>
    dplyr::mutate(
      group = "Time Until 1st Tx"
    )
  return(tb)

}


#' How long until start of combo treatment
#' @param th the treatment history table
#' @return a tibble summarizing the duration until start of combo treatment using min, p25
#' p50, p75 and max values
#' @export
howLongCombination <- function(th) {

 t1 <- combo_events(th) |>
   dplyr::mutate(
     duration = dplyr::case_when(
       is.na(duration) ~ as.integer(event_end - event_start),
       TRUE ~ duration
     )
   )

 tb <- t1 |>
   dplyr::group_by(eventOrder) |>
   summarize_time() |>
   dplyr::mutate(
     group = "Time Until Combination",
     .before = 1
   ) |>
   dplyr::mutate(
     name = dplyr::case_when(
       grepl("NA", eventOrder) ~ gsub("NA -> ", "", eventOrder),
       TRUE ~ eventOrder
     ),
     .before = 2
   ) |>
   dplyr::select(-eventOrder)

  return(tb)
}

#' How long only treatment
#' @param th the treatment history table
#' @return a tibble summarizing the duration until next treatment using min, p25
#' p50, p75 and max values
#' @export
howLongOnlyTx <- function(th) {
  if (th$query_method[1] == 'atlas') {
    stop("ATLAS Pathways method is not suitable for this analysis as end dates are collapsed.")
  }
  t1 <- th |>
    dplyr::filter(
      subject_id %in% single_era(th)
    ) |>
    # get duration, seq gap and event order between current and prior row
    dplyr::mutate(
      duration = as.integer(event_end - event_start),
      seqGap = glue::glue("{event_seq}"),
      eventOrder = glue::glue("{event_cohort_name}")
    )
  tb <- summarize_scenarios_time(t1) |>
    dplyr::filter(
      seqGap == 1
    ) |>
    dplyr::mutate(
      group = "Time on Single Tx",
      .before = 1
    )

  return(tb)

}

#' How long until next treatment
#' @param th the treatment history table
#' @return a tibble summarizing the duration until next treatment using min, p25
#' p50, p75 and max values
#' @export
howLongNextTx <- function(th) {

  t1 <- lag_events(th)

  tb <- summarize_scenarios_time(t1) |>
    dplyr::filter(
      !grepl("\\+", eventOrder)
    ) |>
    dplyr::mutate(
      group = "Time Until Next Tx",
      .before = 1
    )

  return(tb)

}


#' How long is a specific treatment line
#' @param th the treatment history table
#' @param line the treatment line to consider, defaults to 1
#' @return a tibble summarizing the duration of eras using min, p25
#' p50, p75 and max values
#' @export
howLongTxLine <- function(th, line = 1) {
  if (th$query_method[1] == 'atlas') {
    stop("ATLAS Pathways method is not suitable for this analysis as end dates are collapsed.")
  }
  t1 <- th |>
    dplyr::filter(
      event_seq == !!line
    ) |>
    dplyr::mutate(
      duration = event_end - event_start,
      duration = as.integer(duration)
    ) |>
    dplyr::select(
      subject_id, event_seq, event_cohort_name, event_start, event_end, duration
    )

  # summarize all lines
  allLines <- t1 |>
    summarize_time() |>
    dplyr::mutate(
      name = glue::glue("Duration of eras: any"),
      .before = 1
    )
  byLine <- t1 |>
    dplyr::group_by(event_cohort_name) |>
    summarize_time() |>
    dplyr::mutate(
      name = glue::glue("Duration of eras: {event_cohort_name}")
    ) |>
    dplyr::select(name, n:maxValue)

  tb <- dplyr::bind_rows(allLines, byLine)
  return(tb)

}

#' How long is any drug era
#' @param th the treatment history table
#' @return a tibble summarizing the duration of eras using min, p25
#' p50, p75 and max values
#' @export
howLongAreDrugEras <- function(th) {
  if (th$query_method[1] == 'atlas') {
    stop("ATLAS Pathways method is not suitable for this analysis as end dates are collapsed.")
  }
  t1 <- th |>
    dplyr::mutate(
      duration = event_end - event_start,
      duration = as.integer(duration)
    ) |>
    dplyr::select(
      subject_id, event_seq, event_cohort_name, event_start, event_end, duration
    )

  # summarize all lines
  allLines <- t1 |>
    summarize_time() |>
    dplyr::mutate(
      name = glue::glue("Duration of eras: any"),
      .before = 1
    )
  byLine <- t1 |>
    dplyr::group_by(event_cohort_name) |>
    summarize_time() |>
    dplyr::mutate(
      name = glue::glue("Duration of eras: {event_cohort_name}")
    ) |>
    dplyr::select(name, n:maxValue)

  tb <- dplyr::bind_rows(allLines, byLine)
  return(tb)
}

#' How long for patients until discontinuation
#' @param th the treatment history table
#' @param days the maximum number of days before stopping to consider an event a discontinuation
#' @return a tibble summarizing the time to discontinuation using min, p25
#' p50, p75 and max values
#' @export
howLongDiscontinuation <- function(th, days = 365) {
  if (th$query_method[1] == 'atlas') {
    stop("ATLAS Pathways method is not suitable for this analysis as end dates are collapsed.")
  }
  #create denominator
  nn <- get_denominator(th)

  # find persons with mult eras and lag them
  t1 <- find_discontinuation(th) |>
    dplyr::mutate(
      duration = as.integer(event_end - event_start),
      discontinue = dplyr::if_else(duration <= days, 1, 0)
    ) |>
    dplyr::filter(discontinue == 1)

  # find which lines had one tx
  byLine <- t1 |>
    dplyr::group_by(event_cohort_name) |>
    summarize_time() |>
    dplyr::ungroup() |>
    dplyr::rename(
      name = event_cohort_name
    )

  # of any line who had one tx
  anyLine <- t1 |>
    summarize_time() |>
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
    )

  return(tb)

}
