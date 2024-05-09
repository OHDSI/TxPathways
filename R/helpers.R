# Helper functions -------------

# TODO think about what is a discontinuation
find_discontinuation <- function(th) {
  t1 <- th |>
    dplyr::group_by(
      subject_id
    ) |>
    # determine what is the event prior to the next row
    dplyr::mutate(
      last_end = dplyr::lag(event_end, order_by = event_seq),
      prev_drug = dplyr::lag(event_cohort_name, order_by = event_seq)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      duration = as.integer(event_start - last_end),
      discontinue = dplyr::case_when(
        duration >= 60 ~ 1, # indicate row where discontinuation occurs
        TRUE ~ NA_integer_ # all other rows NA
      )
    ) |>
    dplyr::group_by(
      subject_id
    ) |>
    # fill NA values below discontinue date to also drop since this is a new era
    tidyr::fill(discontinue, .direction = "down") |>
    dplyr::ungroup() |>
    # replace the remaining NAs with 0 as these are the values to keep
    tidyr::replace_na(list(discontinue = 0))|>
    dplyr::filter(
      # keep values where discontinuation does not occur
      discontinue == 0
    ) |>
    dplyr::select(-c(last_end, prev_drug, discontinue, duration))


  t2 <- t1 |>
    dplyr::group_by(subject_id) |>
    dplyr::mutate(
      next_drug = dplyr::lead(event_cohort_name, order_by = event_seq)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      drop = dplyr::case_when(
        event_cohort_name != next_drug ~ 1,
        is.na(next_drug) ~ 0,
        TRUE ~ 0
      )
    ) |>
    dplyr::filter(
      drop == 0
    ) |>
    dplyr::select(
      -c(drop, next_drug)
    )

  t3 <- t2 |>
    dplyr::group_by(subject_id) |>
    dplyr::mutate(
      final_dat = dplyr::last(event_end, order_by = event_seq)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      event_seq == 1
    ) |>
    dplyr::mutate(
      event_end = final_dat
    ) |>
    dplyr::select(
      -c(final_dat)
    )
  return(t3)
}

lag_events <- function(th) {

  # get persons who only have multiple eras
  multi_era_ids <- multi_era(th)

  # find persons with mult eras
  t1 <- th |>
    dplyr::filter(
      subject_id %in% multi_era_ids
    ) |>
    dplyr::group_by(
      subject_id
    ) |>
    # determine what is the event prior to the next row
    dplyr::mutate(
      last_end = dplyr::lag(event_end, order_by = event_seq),
      prev_event = dplyr::lag(event_seq),
      prev_drug = dplyr::lag(event_cohort_name, order_by = event_seq)
    ) |>
    dplyr::ungroup() |>
    # get duration, seq gap and event order between current and prior row
    dplyr::mutate(
      duration = as.integer(event_start - last_end),
      seqGap = glue::glue("{prev_event} - {event_seq}"),
      eventOrder = glue::glue("{prev_drug} | {event_cohort_name}")
    ) |>
    dplyr::filter(
      !is.na(last_end) # remove the last row
    )
  return(t1)

}


combo_events <- function(th) {

  # get persons who only have multiple eras
  combo_ids <- whoHasCombinationTreatments(th)


  t1 <- th |>
    dplyr::filter(
      subject_id %in% combo_ids
    ) |>
    dplyr::group_by(
      subject_id
    ) |>
    dplyr::mutate(
      last_start = dplyr::lag(event_start, order_by = event_seq),
      last_drug = dplyr::lag(event_cohort_name, order_by = event_seq)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      duration = as.integer(event_start - last_start),
      eventOrder = glue::glue("{last_drug} -> {event_cohort_name}")
    ) |>
    dplyr::filter(
      grepl("\\+", event_cohort_name)
    )

  return(t1)
}
