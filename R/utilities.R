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
