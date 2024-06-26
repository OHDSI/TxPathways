% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drugHistory.R
\name{getTxHistoryTable}
\alias{getTxHistoryTable}
\title{Function that retrieves and formats tx history table}
\usage{
getTxHistoryTable(
  connection,
  targetCohortKey,
  eventCohortKey,
  tableName,
  workDatabaseSchema,
  dropTable = TRUE
)
}
\arguments{
\item{connection}{the connection to an omop database}

\item{targetCohortKey}{a tibble with the target cohort id and name to use as a merge key for formatting}

\item{eventCohortKey}{a tibble with the event cohort id and name to use as a merge key for formatting}

\item{tableName}{the name of the table to save the treatment history table}

\item{dropTable}{toggle to drop table, defaults to true}
}
\value{
a tibble with the treatment history information
}
\description{
Function that retrieves and formats tx history table
}
