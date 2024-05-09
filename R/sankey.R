


prep_sankey <- function(th, maxPathLength = 2) {

  treatment_pathways <- th |>
    dplyr::filter(event_seq <= maxPathLength) |>
    tidyr::pivot_wider(id_cols = subject_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name) |>
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) |>
    dplyr::mutate(End = "end", .before = "n")

  links <- treatment_pathways |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') |>
    dplyr::mutate(column = match(column, names(treatment_pathways))) |>
    tidyr::drop_na(source) |>
    dplyr::mutate(source = paste0(source, '__', column)) |>
    dplyr::group_by(row) |>
    dplyr::mutate(target = dplyr::lead(source, order_by = column)) |>
    tidyr::drop_na(target, source) |>
    dplyr::group_by(source, target) |>
    dplyr::summarise(value = sum(n), .groups = 'drop') |>
    dplyr::arrange(desc(value))

  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)
  data.table::setorder(links, cols = - "value")

  res <- list(
    'treatmentPatterns' = treatment_pathways,
    'links' = links,
    'nodes' = nodes
  )

  return(res)

}


plot_sankey <- function(sankey) {

  links <- sankey$links
  nodes <- sankey$nodes

  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")

  kelly_colors <- unname(grafify::graf_palettes$kelly)[-1]

  col <- kelly_colors[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")

  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')

  #plot sankeyNetwork
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = 'source',
    Target = 'target',
    Value = 'value',
    NodeID = 'name',
    fontSize = 11,
    sinksRight = FALSE,
    colourScale = myCol
  )

  return(sankey)

}

#' Make the sankey diagram for the treatment history
#' @param th the treatment history table
#' @param maxPathLength the maximum number of paths in the sequence
#' @return a sankey diagram
#' @export
viewSankey <- function(th, maxPathLength = 2) {

  dt <- prep_sankey(th = th, maxPathLength = maxPathLength)
  pp <- plot_sankey(dt)
  return(pp)
}
