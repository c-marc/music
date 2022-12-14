% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_unit_to_fifths.R
\name{set_unit_to_fifths}
\alias{set_unit_to_fifths}
\alias{set_unit_to_fifths.m_index}
\alias{set_unit_to_fifths.m_xtibble}
\title{Set unit of an index so that it gets ordered by fifths}
\usage{
set_unit_to_fifths(x)

\method{set_unit_to_fifths}{m_index}(x)

\method{set_unit_to_fifths}{m_xtibble}(x)
}
\arguments{
\item{x}{An object.}
}
\value{
The object with a modified index.
}
\description{
Transform the index so that its unit equals the one that corresponds to a fifth interval.
Values are projected between \code{-period \%/\% 2} and \code{period \%/\% 2} with the exception of 12-TET where values are projected between -5 and 6.
This wraps \code{\link[=set_unit]{set_unit()}}.
}
\examples{
index(0:11, period = 12L, unit = 1L) |> set_unit_to_fifths()
start_with_mode() |> as_12_TET() |> set_unit_to_fifths()
}
