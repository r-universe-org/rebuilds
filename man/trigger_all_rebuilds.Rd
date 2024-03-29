% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedule.R
\name{trigger_all_rebuilds}
\alias{trigger_all_rebuilds}
\title{Trigger rebuilds}
\usage{
trigger_all_rebuilds(retry_days = 3, rebuild_days = 30)
}
\arguments{
\item{retry_days}{number of days to retry failures builds}

\item{rebuild_days}{number of days after which to do a full fresh rebuild}
}
\description{
Invoke retries and full rebuilds for all the packages.
}
