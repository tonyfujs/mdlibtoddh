% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_existing_dataset.R
\name{update_existing_dataset}
\alias{update_existing_dataset}
\title{update_existing_dataset()}
\usage{
update_existing_dataset(
  md_internal_id,
  md_token,
  master,
  ddh_fields = ddhconnect::get_fields(),
  lovs = ddhconnect::get_lovs(),
  root_url = dkanr::get_url(),
  iso_3_df = ddhconnect::get_iso3(),
  credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())
)
}
\arguments{
\item{md_internal_id}{character: Microdata internal ID of the dataset to be added}

\item{md_token}{character: Microdata API authentication token}

\item{master}{dataframe: Master lookup table}

\item{ddh_fields}{dataframe: table of all the data catalog fields by node type}

\item{lovs}{dataframe: lookup table of the data catalog tids and values}

\item{root_url}{character: Root URL to use for the API (Staging or Production)}

\item{iso_3_df}{dataframe: Table of iso_3 codes and corresponding country names}

\item{credentials}{list: DDH API authentication token and cookie}
}
\value{
character
}
\description{
Update a full microdata record in DDH (metadata + resources)
}
