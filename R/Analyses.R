#' PROJ
#' @import data.table
#' @export AnalyseYearLine
AnalyseYearLine <- function(data,  v) {

  dataset <- data[, .(n = sum(value),
                      consult = sum(consult),
                      pop = sum(pop),
                      HelligdagIndikator=mean(HelligdagIndikator)), by = .(date)]

  res <- QuasipoissonAlgorithm(dataset, predinterval=8*7, isDaily=FALSE, v=v)
  res <- res[!is.na(threshold2)]

  res[week>=30,x:=week-29]
  res[week<30,x:=week+23]
  res[, wkyr := paste0(year, "-", formatC(week, flag = "0", width = 2))]

  dates <- data.table(day = seq.Date(as.Date("2000-01-01"), as.Date("2030-01-01"), by = "days"))
  dates[, wkyr := format.Date(day, format = "%G-%V")]
  dates <- dates[, .(displayDay = max(day)), by = .(wkyr)]
  res <- merge(res, dates, by = "wkyr")
  res <- res[,c("displayDay", "wkyr", "x", variablesAlgorithmWeekly, variablesAlgorithmBasic, variablesAlgorithmProduced),with=F]
  return(res)
}

#' PROJ
#' @import data.table
#' @export AnalyseRecentLine
AnalyseRecentLine <- function(data, v) {
  dataset <- data[, .(n = sum(value),
                                      consult = sum(consult),
                                      pop = sum(pop),
                                      HelligdagIndikator=mean(HelligdagIndikator)), by = .(date)]

  res <- QuasipoissonAlgorithm(dataset, predinterval = 28*2, isDaily=TRUE, v=v)
  res <- res[,c(variablesAlgorithmDaily, variablesAlgorithmBasic, variablesAlgorithmProduced),with=F]

  return(res)
}

#' RunOneAnalysis
#' @param analysesStack a
#' @param analysisData a
#' @import data.table
#' @export RunOneAnalysis
RunOneAnalysis <- function(analysesStack,analysisData){
  #runData <- GetDataFromControlStack(analysesIter)
  if(analysesStack$type=="influensa"){
    setnames(analysisData,"consultWithInfluensa","consult")
  } else {
    setnames(analysisData,"consultWithoutInfluensa","consult")
  }

  retval <- NULL
  if(analysesStack$granularity=="Daily"){
    retval <- AnalyseRecentLine(
          data = analysisData,
          v = analysesStack$v)
  } else {
    retval <- AnalyseYearLine(
      data = analysisData,
      v = analysesStack$v)
  }

  retval[, age := analysesStack$age]
  retval[, type := analysesStack$type]
  retval[, location := analysesStack$location]
  retval[, locationName := GetLocationName(analysesStack$location, norwayLocations=norwayLocations)]

  retval[, status := "Normal"]
  retval[n > threshold2, status := "Medium"]
  retval[n > threshold4, status := "High"]
  return(retval)
}

#' PROB
#' @import data.table
#' @export GetLocationName
GetLocationName <- function(location, norwayLocations) {
  locationName <- "Norge"
  locationHTML <- "Norge"

  if (location != "Norge") {
    if (sum(norwayLocations$municip == location) > 0) {
      locationName <- as.character(norwayLocations$municipName[norwayLocations$municip == location])
    } else if (sum(norwayLocations$county == location) > 0) {
      locationName <- as.character(norwayLocations$countyName[norwayLocations$county == location])
    }
  }

  return(locationName)
}

#' PROB
#' @import data.table
#' @export GetCountyFromMunicip
GetCountyFromMunicip <- function(location, norwayLocations) {
  if (sum(norwayLocations$municip == location) > 0) {
    location <- as.character(norwayLocations$county[norwayLocations$municip == location])
  }

  return(location)
}


