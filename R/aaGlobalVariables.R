#' variablesAlgorithmDaily
#' @export variablesAlgorithmDaily
variablesAlgorithmDaily <- c("date")

#' variablesAlgorithmWeekly
#' @export variablesAlgorithmWeekly
variablesAlgorithmWeekly <- c("year","week")

#' variablesAlgorithmBasic
#' @export variablesAlgorithmBasic
variablesAlgorithmBasic <- c("n","consult","pop","HelligdagIndikator")

#' variablesAlgorithmProduced
#' @export variablesAlgorithmProduced
variablesAlgorithmProduced <- c("threshold0","threshold2","threshold4","threshold6","cumE1","cumL1","cumU1","zscore","failed")

#' CONFIG
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$VERSION <- 1
CONFIG$VERSIONS <- 1:2
CONFIG$SYNDROMES_DOCTOR <- c("influensa")
CONFIG$SYNDROMES_ALL <- c("gastro",
                          "respiratory",
                          "respiratoryinternal",
                          "respiratoryexternal",
                          "lungebetennelse",
                          "bronkitt")
CONFIG$SYNDROMES_ALL <- c("gastro",
                          "respiratory")

CONFIG$SYNDROMES <- c(CONFIG$SYNDROMES_DOCTOR, CONFIG$SYNDROMES_ALL)

CONFIG$SYNDROMES_ALERT_INTERNAL <- c("influensa",
                                     "gastro",
                                     "respiratory")

CONFIG$SYNDROMES_ALERT_EXTERNAL <- c("gastro",
                                     "respiratory")

CONFIG$AGES <- c(
  "Totalt",
  "0-4",
  "5-14",
  "15-19",
  "20-29",
  "30-64",
  "65+"
)

#' norwayLocations
#' @export norwayLocations
norwayLocations <- GenNorwayLocations()

#' norwayMunicipMerging
#' @export norwayMunicipMerging
norwayMunicipMerging <- GenNorwayMunicipMerging()
