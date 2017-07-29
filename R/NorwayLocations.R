#' GenNorwayLocations
#' @importFrom readxl read_excel
#' @export GenNorwayLocations
GenNorwayLocations <- function(){
  norwayLocations <- readxl::read_excel(system.file("extdata", "norwayLocations.xlsx", package = "sykdomspuls"))
  norwayLocations <- norwayLocations[is.na(norwayLocations$yearEnd),]
  return(norwayLocations)
}

#' GenNorwayMunicipMerging
#' Last updated 2017-07-29
#' @importFrom lubridate year today
#' @importFrom readxl read_excel
#' @import data.table
#' @importFrom zoo na.locf
#' @export GenNorwayMunicipMerging
GenNorwayMunicipMerging <- function(){
  masterData <- data.table(readxl::read_excel(system.file("extdata", "norwayLocations.xlsx", package = "sykdomspuls")))
  masterData[yearStart <= 2006, yearStart := 2006]
  setnames(masterData,"yearStart","year")
  skeleton <- expand.grid(year = as.numeric(2006:lubridate::year(lubridate::today())),municip=unique(masterData$municip),stringsAsFactors = FALSE)
  skeleton <- data.table(merge(skeleton, masterData, by=c("municip","year"), all.x=T))
  setorder(skeleton,municip,year)
  skeleton[is.na(yearEnd),yearEnd:=2017]
  skeleton[,yearEnd:=min(yearEnd,na.rm=T),by=municip]
  skeleton <- skeleton[year<=yearEnd]
  skeleton[,yearEnd:=NULL]
  skeleton[!is.na(municipName),yearStart:=year]
  skeleton[,yearStart:=min(yearStart,na.rm=T),by=municip]
  skeleton <- skeleton[year>=yearStart]
  skeleton[,yearStart:=NULL]

  skeleton[is.na(municipEnd) & !is.na(municipName),municipEnd:=municip]

  skeleton[,municipEnd:=zoo::na.locf(municipEnd)]
  skeleton[,municipName:=zoo::na.locf(municipName)]
  skeleton[,county:=zoo::na.locf(county)]
  skeleton[,countyName:=zoo::na.locf(countyName)]
  skeleton[,region:=zoo::na.locf(region)]
  skeleton[,regionName:=zoo::na.locf(regionName)]

  return(skeleton)
}
