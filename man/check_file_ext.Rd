% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_file_ext.R
\name{check_file_ext}
\alias{check_file_ext}
\title{check_file_ext}
\usage{
check_file_ext(metadata_resource, lovs = ddhconnect::get_lovs())
}
\arguments{
\item{metadata_resource}{named vector: object returned by get_metadata() on a resource}

\item{lovs}{dataframe: object returned by the get_lovs() function}
}
\value{
dataframe
}
\description{
Check if the resource file extension matches the expected field_form value
}
