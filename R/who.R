# identify populations --------------------
single_era <- function(th) {
  single_era_ids <- th |>
    dplyr::count(subject_id) |>
    dplyr::filter(n == 1) |>
    dplyr::pull(subject_id)
  return(single_era_ids)
}

multi_era <- function(th) {
  multi_era_ids <- th |>
    dplyr::count(subject_id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(subject_id)
  return(multi_era_ids)
}

# Find the persons who only took one medication through their patient history

# 1) could be someone who only have one era in entire history or
# 2) could be someone who took the same drug many times in separate eras
whoHasSingleTreatment <- function(th) {

  # Get persons with only a single era
  single_era_ids <- single_era(th)

  # get persons who only have multiple eras
  multi_era_ids <- multi_era(th)

  # pick those with same drug throughout
  same_drug_many_times <- th |>
    dplyr::filter(subject_id %in% multi_era_ids) |>
    dplyr::distinct(subject_id, event_cohort_name) |>
    dplyr::count(subject_id) |>
    dplyr::filter(n == 1) |>
    dplyr::pull(subject_id)

  single_treatment_ids <- c(single_era_ids, same_drug_many_times)
  return(single_treatment_ids)
}

# Find the persons who took different medications through their patient history
whoHasMultipleTreatments <- function(th) {

  # get persons who only have multiple eras
  multi_era_ids <- multi_era(th)

  # find those who have more than one drug across multiple eras
  multi_treatment_ids <- th |>
    dplyr::filter(subject_id %in% multi_era_ids) |>
    dplyr::distinct(subject_id, event_cohort_name) |>
    dplyr::count(subject_id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(subject_id)

  return(multi_treatment_ids)
}

# find persons who had a combination treatment
whoHasCombinationTreatments <- function(th) {

  combo_ids <-  th |>
    dplyr::filter(
      grepl("\\+", event_cohort_name)
    ) |>
    dplyr::distinct(subject_id) |>
    dplyr::pull(subject_id)

  return(combo_ids)
}

whoReturnsToSameTreatment <- function(th) {

  # get persons who only have multiple eras
  multi_era_ids <- multi_era(th)

  # pick those with same drug throughout
  same_drug_many_times <- th |>
    dplyr::filter(subject_id %in% multi_era_ids) |>
    dplyr::distinct(subject_id, event_cohort_name) |>
    dplyr::count(subject_id) |>
    dplyr::filter(n == 1) |>
    dplyr::pull(subject_id)

  return(same_drug_many_times)
}
