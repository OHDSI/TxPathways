# Truncate analysis ---------------
#
# th <- fs::path_package("TxPathways", "data/treatmentHistory.csv") |>
#   readr::read_csv(show_col_types = FALSE)

#' Truncate th to certain number of events
#' @param th the treatment history table
#' @param eventSeqLimit the cutoff of eligible event sequences in the treatment
#' history table. Defaults to 2
#' @return the treatment history table filtered by the event seq limit
#' @export
truncateByEvent <- function(th, eventSeqLimit = 2) {

  #get the event sequence to truncate
  event_seq_limit <- 1:eventSeqLimit

  dt <- th |>
    dplyr::filter(
      event_seq %in% event_seq_limit
    )

  return(dt)

}


#' Truncate th to certain number of events
#' @param th the treatment history table
#' @param dayLimit the cutoff of days considered in the treatment history table
#' Defaults to 365 (approx 1 year)
#' @return the treatment history table filtered by the day limit
#' @import tidyr
#' @export
truncateByTime <- function(th, dayLimit = 365) {


  #get the first sequence per person
  event_limit_tbl <- th |>
    dplyr::filter(
      event_seq == 1
    ) |>
    dplyr::mutate(
      event_limit = event_start + dayLimit
    ) |>
    dplyr::select(
      subject_id, event_seq, event_start, event_start, event_limit
    )

  # join event limit back to original th table
  kk <- th |>
    dplyr::left_join(
      event_limit_tbl, by = c("subject_id", "event_seq", "event_start") # join to original th to filter by limit
    ) |>
    dplyr::group_by(subject_id) |>
    tidyr::fill(
      event_limit, .direction = "down" # fill in event limit of other event seq with previous value
    ) |>
    dplyr::ungroup() |>
    # first check if next start date is past event limit and drop
    dplyr::mutate(
      diff1 = as.integer(event_limit - event_start),
      drop = dplyr::if_else(diff1 < 0, 1, 0)
    ) |>
    dplyr::filter(
      drop == 0
    ) |>
    dplyr::mutate(
      # now check if end date is past event limit and adjust to event limit
      diff2 = as.integer(event_limit - event_end),
      event_end = dplyr::if_else(diff2 < 0, event_limit, event_end)
    ) |>
    dplyr::select(
      -c(diff1, drop, diff2, event_limit)
    )

  return(kk)

}
