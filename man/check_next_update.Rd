% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_next_update.R
\name{check_next_update}
\alias{check_next_update}
\title{check_next_update}
\usage{
check_next_update(metadata_dataset, lovs = ddhconnect::get_lovs())
}
\arguments{
\item{metadata_dataset}{list: object returned by get_metadata()}

\item{lovs}{dataframe: object returned by the get_lovs() function}
}
\value{
vector
}
\description{
Check if update date has passed
}
