% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/howMany.R
\name{howManyDiscontinue}
\alias{howManyDiscontinue}
\title{How many patients have discontinuation}
\usage{
howManyDiscontinue(th, threshold = 60, days = 365)
}
\arguments{
\item{th}{the treatment history table}
}
\value{
a tibble summarizing the number of persons and percentage with a discontinuation at 365d
}
\description{
How many patients have discontinuation
}
