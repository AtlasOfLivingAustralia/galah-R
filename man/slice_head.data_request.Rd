% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice_head.R
\name{slice_head.data_request}
\alias{slice_head.data_request}
\alias{slice_head.metadata_request}
\title{Subset rows using their positions}
\usage{
\method{slice_head}{data_request}(.data, ..., n, prop, by = NULL)

\method{slice_head}{metadata_request}(.data, ..., n, prop, by = NULL)
}
\arguments{
\item{.data}{An object of class \code{data_request}, created using \code{\link[=galah_call]{galah_call()}}}

\item{...}{Currently ignored}

\item{n}{The number of rows to be returned. If data are grouped
\code{\link[=group_by.data_request]{group_by()}}, this operation will be
performed on each group.}

\item{prop}{Currently ignored.}

\item{by}{Currently ignored.}
}
\value{
An amended \code{data_request} with a completed \code{slice} slot.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{slice()} lets you index rows by their (integer) locations. For objects of
classes \code{data_request} or \code{metadata_request}, only \code{slice_head()} is
currently implemented, and selects the first \code{n} rows.

If \code{.data} has been grouped using
\code{\link[=group_by.data_request]{group_by()}}, the operation will be
performed on each group, so that (e.g.) \code{slice_head(df, n = 5)} will select
the first five rows in each group.
}
\examples{
\dontrun{
# Limit number of rows returned to 3.
# In this case, our query returns the top 3 years with most records.
galah_call() |>
  identify("perameles") |>
  filter(year > 2010) |>
  group_by(year) |>
  count() |>
  slice_head(n = 3) |>
  collect()
}
}
