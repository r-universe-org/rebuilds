% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rebuilds.R
\name{trigger_rebuilds}
\alias{trigger_rebuilds}
\alias{retry_failures}
\alias{retry_everything}
\alias{rebuild_vignettes}
\alias{list_all_packages}
\alias{rebuild_oldies}
\alias{rebuild_failed_vignettes}
\alias{rebuild_all_missing_field}
\alias{rebuild_bioc_packages}
\alias{rebuild_all_oldies}
\alias{rebuild_missing_binaries}
\alias{retry_failed_binaries}
\alias{rebuild_missing_arm64}
\alias{rebuild_all_missing_arm64}
\alias{rebuild_missing_sources}
\alias{rebuild_mixed_case_login}
\alias{rebuild_all_missing_sources}
\alias{rebuild_all_missing_binaries}
\alias{cancel_queued_builds}
\alias{cancel_all_queued_builds}
\alias{delete_environments}
\alias{delete_all_environments}
\alias{delete_one}
\alias{delete_all_old_binaries}
\alias{delete_all_old_binaries_fast}
\alias{trigger_all_cleanups}
\alias{rebuild_all_remotes}
\alias{rebuild_universe_remotes_only}
\alias{remove_packages_with_remotes}
\alias{remove_all_packages_with_remotes}
\alias{rebuild_old_bioc}
\alias{rebuild_graphics_packages}
\alias{trigger_revdeps}
\title{Rebuild packages}
\usage{
trigger_rebuilds(repository = "r-universe/jeroen", delete_after = 90)

retry_failures(universe = NULL, rebuild = FALSE)

retry_everything(universe)

rebuild_vignettes(universe = "jeroen")

list_all_packages(fields = "_upstream", type = "src")

rebuild_oldies(universe, before = Sys.Date() - 32, types = c("src", "failure"))

rebuild_failed_vignettes(universe = NULL)

rebuild_all_missing_field(field = "_jobs")

rebuild_bioc_packages()

rebuild_all_oldies(before = "2022-04-01")

rebuild_missing_binaries(universe = "ropensci")

retry_failed_binaries(universe = "ropensci")

rebuild_missing_arm64(universe = "ropensci")

rebuild_all_missing_arm64()

rebuild_missing_sources(universe = "ropensci")

rebuild_mixed_case_login()

rebuild_all_missing_sources()

rebuild_all_missing_binaries()

cancel_queued_builds(universe = "ropensci")

cancel_all_queued_builds()

delete_environments(universe = "ropensci")

delete_all_environments()

delete_one(universe, pkg, version = "all", type = "all", build = "all")

delete_all_old_binaries()

delete_all_old_binaries_fast()

trigger_all_cleanups()

rebuild_all_remotes()

rebuild_universe_remotes_only(universe)

remove_packages_with_remotes(org)

remove_all_packages_with_remotes()

rebuild_old_bioc(delay = 900)

rebuild_graphics_packages()

trigger_revdeps(package)
}
\arguments{
\item{repository}{name of the github repository}

\item{universe}{name of the universe, use NULL for all universes}

\item{fields}{extra fields you want to get}

\item{before}{date before which to rebuild}

\item{pkg}{name of package to delete}

\item{workflow}{name of the workflow to trigger}

\item{days}{trigger rebuild every n days}
}
\description{
Automatically trigger workflows once every n days.
}
