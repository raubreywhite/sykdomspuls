% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CleanData.R
\name{FormatData}
\alias{FormatData}
\title{test}
\usage{
FormatData(d, SYNDROME, population = readRDS(system.file("extdata", "pop.RDS",
  package = "sykdomspuls")), hellidager = fread(system.file("extdata",
  "DatoerMedHelligdager.txt", package = "sykdomspuls"))[, c("Dato",
  "HelligdagIndikator"), with = FALSE],
  testIfHelligdagIndikatorFileIsOutdated = TRUE,
  removeMunicipsWithoutConsults = FALSE)
}
\arguments{
\item{d}{a}

\item{SYNDROME}{a}

\item{population}{a}

\item{hellidager}{a}

\item{testIfHelligdagIndikatorFileIsOutdated}{a}

\item{removeMunicipsWithoutConsults}{a}
}
\description{
test
}
