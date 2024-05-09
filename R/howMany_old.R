# HowMany ------------------
# Function to summarize counts and percentages for certain txpat events


howManyHadSingleTx <- function(th) {

  # get number of unique persons
  nn <- th |>
    dplyr::distinct(subject_id) |>
    nrow()

  #find those who only have single treatment
  ids <- single_era(th)

  # find number of persons
  num_persons <- length(ids)
  #create summary table
  tb <- tibble::tibble(
    name = glue::glue("One Treatment: All"),
    n = num_persons,
    pct = (num_persons / nn) * 100
  )

  # Now find by treatment line
  byLine <- th |>
    dplyr::filter(subject_id %in% ids) |>
    dplyr::group_by(event_cohort_name) |>
    dplyr::summarise(n = dplyr::n_distinct(subject_id)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct = (n / nn) * 100
    ) |>
    dplyr::mutate(
      name = glue::glue("One Treatment: {event_cohort_name}")
    ) |>
    dplyr::select(name, n, pct)

  # bind rows
  tb <- tb |>
    dplyr::bind_rows(byLine)

  return(tb)

}


howManyHadOneTxManyTimes <- function(th) {

  # get number of unique persons
  nn <- th |>
    dplyr::distinct(subject_id) |>
    nrow()

  #find those who only have single treatment
  ids <- whoHasSingleTreatment(th)

  # first find overall with single treatment
  # find number of persons
  num_persons <- length(ids)
  #create summary table
  tb <- tibble::tibble(
    name = glue::glue("Single Treatment: All"),
    n = num_persons,
    pct = (num_persons / nn) * 100
  )

  # Now find by treatment line
  byLine <- th |>
    dplyr::filter(subject_id %in% ids) |>
    dplyr::group_by(event_cohort_name) |>
    dplyr::summarise(n = dplyr::n_distinct(subject_id)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct = (n / nn) * 100
    ) |>
    dplyr::mutate(
      name = glue::glue("Single Treatment: {event_cohort_name}")
    ) |>
    dplyr::select(name, n, pct)

  # bind rows
  tb <- tb |>
    dplyr::bind_rows(byLine)

  return(tb)

}


howManyHaveMultipleTx <- function(th) {


  # get number of unique persons
  nn <- th |>
    dplyr::distinct(subject_id) |>
    nrow()

  #find those who only have multi treatment
  ids <- whoHasMultipleTreatments(th)

  # first count with any number of drugs
  # find number of persons
  num_persons <- length(ids)
  #create summary table
  tb <- tibble::tibble(
    name = "Num Drugs: Any",
    n = num_persons,
    pct = (num_persons / nn) * 100
  )

  # Now find by number of lines
  byLine <- th |>
    dplyr::filter(subject_id %in% ids) |>
    dplyr::distinct(subject_id, event_cohort_name) |>
    dplyr::count(subject_id, name = "name") |>
    dplyr::count(name) |>
    dplyr::mutate(
      pct = (n / nn) * 100,
      name = glue::glue("Num Drugs: {name}")
    )

  # bind rows
  tb <- tb |>
    dplyr::bind_rows(byLine)

  return(tb)
}


howManyHaveComboTx <- function(th) {

  # get number of unique persons
  nn <- th |>
    dplyr::distinct(subject_id) |>
    nrow()

  #find those who only have combo treatment
  ids <- whoHasCombinationTreatments(th)

  # find number of persons
  num_persons <- length(ids)
  #create summary table
  tb <- tibble::tibble(
    name = "Combinations: Any ",
    n = num_persons,
    pct = (num_persons / nn) * 100
  )

  # Now find by number of lines
  byLine <- th |>
    dplyr::filter(
      subject_id %in% ids,
      grepl("\\+", event_cohort_name)) |>
    dplyr::distinct(subject_id, event_cohort_name) |>
    dplyr::group_by(event_cohort_name) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct = (n / nn) * 100
    ) |>
    dplyr::mutate(
      name = glue::glue("Comboniations: {event_cohort_name}")
    ) |>
    dplyr::select(name, n, pct)

  # bind rows
  tb <- tb |>
    dplyr::bind_rows(byLine)
  return(tb)
}


howManySwitchedTxFromAtoB <- function(th, depth = 2) {


  # get persons who only have multiple eras
  multi_tx_ids <- whoHasMultipleTreatments(th)
  nn <- length(multi_tx_ids)

  tb <- th |>
    dplyr::filter(subject_id %in% multi_tx_ids) |>
    dplyr::distinct(subject_id, event_cohort_name) |>
    dplyr::group_by(subject_id) |>
    dplyr::mutate(
      event_seq = dplyr::row_number()
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      event_seq <= depth
    ) |>
    tidyr::pivot_wider(
      id_cols = subject_id,
      names_from = event_seq,
      names_prefix = "event_cohort_name",
      values_from = event_cohort_name
    ) |>
    dplyr::count(
      dplyr::across(tidyselect::starts_with("event_cohort_name"))
    ) |>
    tidyr::unite(
      col = name,
      tidyselect::starts_with("event_cohort_name"),
      sep = " | "
    ) |>
    dplyr::mutate(
      pct = (n / nn) * 100,
      name = glue::glue("Switch: {name}")
    ) |>
    dplyr::arrange(dplyr::desc(n))

  return(tb)
}


howManyHadTxInterruption <- function(th, seqGap = '1-2', interruption = '30-60') {

  # Deal with string split

  seqGapA <- stringr::str_split_1(seqGap, pattern = '-')[1] |> as.integer()
  seqGapB <- stringr::str_split_1(seqGap, pattern = '-')[2] |> as.integer()
  interruptionA <- stringr::str_split_1(interruption, pattern = '-')[1] |> as.integer()
  interruptionB <- stringr::str_split_1(interruption, pattern = '-')[2] |> as.integer()


  # Get persons that are relevant

  # get persons who only have multiple eras
  same_drug_many_times <- whoReturnsToSameTreatment(th)
  nn <- length(same_drug_many_times)

  # Summarize

  # get persons from first event
  t1_a <- th |>
    dplyr::filter(
      subject_id %in% same_drug_many_times,
      event_seq %in% seqGapA
    ) |>
    tidyr::unite(
      col = event_a,
      event_seq, event_cohort_name,
      sep = "."
    ) |>
    dplyr::select(
      subject_id, event_a, event_end
    )

  # get persons from second event
  t1_b <- th |>
    dplyr::filter(
      subject_id %in% same_drug_many_times,
      event_seq %in% seqGapB
    ) |>
    tidyr::unite(
      col = event_b,
      event_seq, event_cohort_name,
      sep = "."
    ) |>
    dplyr::select(
      subject_id, event_b, event_start
    )
  # join the two sides and filter those who had interruption in period
  t2 <- t1_b |>
    dplyr::inner_join(
      t1_a, by = c("subject_id")
    ) |>
    dplyr::mutate(
      gap = event_start - event_end,
      gap = as.integer(gap),
      name = gsub(".*\\.", "", event_b),
    ) |>
    dplyr::filter(
      dplyr::between(gap, interruptionA, interruptionB)
    ) |>
    dplyr::group_by(name) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct = (n / nn) * 100,
      name = glue::glue("Interruption {interruption} at gap {seqGap}: {name}")
    )
  return(t2)
}




howManyStoppedTxBefore <- function(th, days = 365, event_seq = 1) {

  # get number of unique persons
  #TODO Check this is the correct denominator
  nn <- th |>
    dplyr::distinct(subject_id) |>
    nrow()

  t1 <- th |>
    dplyr::filter(
      event_seq == !!event_seq
    ) |>
    dplyr::mutate(
      gap = event_end - event_start,
      gap = as.integer(gap)
    ) |>
    dplyr::filter(
      gap <= days
    )

  anyLine <- t1 |>
    dplyr::count() |>
    dplyr::mutate(
      pct = (n / nn) * 100,
      name = glue::glue("Discontinued treatment before {days}: any")
    )

  byLine <- t1 |>
    # dplyr::select(
    #   subject_id, event_cohort_name, event_start, event_end, gap
    # ) |>
    dplyr::group_by(event_cohort_name) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct = (n / nn) * 100,
      name = glue::glue("Discontinued treatment before {days}: {event_cohort_name}")
    ) |>
    dplyr::select(
      name, n, pct
    )

  tb <- dplyr::bind_rows(anyLine, byLine) |>
    dplyr::select(name, n, pct)
  return(tb)

}


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
