% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EmailNotifiation.R
\name{EmailExternal}
\alias{EmailExternal}
\title{test}
\usage{
EmailExternal(results = readRDS(fhi::DashboardFolder("results",
  "outbreaks_alert_external.RDS")),
  alerts = readxl::read_excel(file.path("/etc", "gmailr",
  "emails_sykdomspuls_alert.xlsx")), isTest = TRUE, forceNoOutbreak = FALSE,
  forceYesOutbreak = FALSE)
}
\arguments{
\item{results}{a}

\item{alerts}{a}

\item{isTest}{a}

\item{forceNoOutbreak}{a}

\item{forceYesOutbreak}{a}
}
\description{
test
}
