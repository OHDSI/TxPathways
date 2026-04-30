# Utilities --------------

# quick make truncate and drop sql
trunc_drop_tbl <- function(table) {
  sql <- glue::glue("TRUNCATE TABLE {table}; DROP TABLE {table};")
  return(sql)
}

# quick make a collect all sql
pull_tbl <- function(table) {
  sql <- glue::glue("SELECT * FROM {table};")
  return(sql)
}

# fn to read a file, render it with params, and translate it to the correct dialect
read_render_translate <- function(sqlFile, dbms, tempEmulationSchema, ...) {

  #read file
  sql <- readr::read_file(sqlFile)

  #render sql
  sqlRendered <- SqlRender::render(
    sql = sql,
    ...
  )

  # translate file to dialect
  if (dbms == "snowflake") {
    sqlTranslate <- SqlRender::translate(
      sql = sqlRendered,
      targetDialect = dbms,
      tempEmulationSchema = tempEmulationSchema)
  } else {
    sqlTranslate <- SqlRender::translate(
      sql = sqlRendered,
      targetDialect = dbms)
  }

  return(sqlTranslate)
}

# event combo functions -------------

get_combo <- function(ids, ll, type = c("char", "int")) {
  combs <- combn(ids, ll)
  type <- match.arg(type)
  if (type == "char") {
    out <- apply(combs, 2, function(x) paste(x, collapse = "+"))
  }
  if (type == "int") {
    out <- colSums(combs)
  }
  return(out)
}

event_combos_ids <- function(ids) {
  ii <- seq_along(ids)
  res <- purrr::map(ii, ~get_combo(ids = ids, ll = .x, type = "int")) |>
    purrr::list_c()
  return(res)
}

event_combo_names <- function(names) {
  ii <- seq_along(names)
  res <- purrr::map(ii, ~get_combo(ids = names, ll = .x, type = "char")) |>
    purrr::list_c()
}


format_event_combos <- function(eventCohortKey) {

  key <- tibble::tibble(
    event_cohort_id = event_combos_ids(eventCohortKey$event_cohort_id),
    event_cohort_name = event_combo_names(eventCohortKey$event_cohort_name)
  )
  return(key)
}


stepMask <- function(ids) {
  rr <- sum(2 ^ (ids))
  return(rr)
}

event_combo_ids <- function(eventCohortKey) {

  #get ids and names
  eventIds <- eventCohortKey$event_cohort_id
  eventNames <- eventCohortKey$event_cohort_name

  # get cmbIds
  cmbIds <- purrr::map(
    seq_along(eventIds),
    ~combn(eventIds, .x, FUN = list)
  ) |>
    purrr::flatten()
  # find bitW
  seqId <- purrr::map_int(cmbIds, ~stepMask(.x))
  cmbIds_txt <- cmbIds |>
    purrr::map_chr(~paste(.x, collapse = "+"))


  cmbNms <- purrr::map(
    seq_along(eventNames),
    ~combn(eventNames, .x, FUN = list)
  ) |>
    purrr::flatten() |>
    purrr::map_chr(~paste(.x, collapse = "+"))


  eventComboKey <- tibble::tibble(
    'mask' = seqId,
    'combo_id' = cmbIds_txt,
    'combo_name' = cmbNms
  )

  return(eventComboKey)

}
