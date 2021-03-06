#' test
#' @param df a
#' @param dk a
#' @param saveFiles a
#' @import data.table
#' @import fhi
#' @import stringr
#' @export GenerateOutbreakListInternal
GenerateOutbreakListInternal <- function(df=readRDS(fhi::DashboardFolder("results","resYearLine.RDS")),
                          dk=readRDS(fhi::DashboardFolder("results","resYearLineMunicip.RDS")),
                          saveFiles=c(
                            fhi::DashboardFolder("results","outbreaks.RDS"),
                            fhi::DashboardFolder("data_app","outbreaks.RDS"))){
  # variables used in data.table functions in this function
  . <- NULL
  status <- NULL
  wkyr <- NULL
  age <- NULL
  county <- NULL
  location <- NULL
  locationName <- NULL
  zscore <- NULL
  type <- NULL
  cumE1 <- NULL
  meanZScore <- NULL
  sumCum <- NULL
  sumCumNorge <- NULL
  countyName <- NULL
  # end

  counties <- unique(df[,c("location","locationName"),with=F])
  setnames(counties,c("county","countyName"))

  df <- df[,c("wkyr","age","type","locationName","status","zscore","cumE1"),with=F]
  dk <- dk[,c("wkyr","age","type","location","locationName","status","county","zscore","cumE1"),with=F]
  dk <- merge(dk,counties,by="county")

  setorder(df,status,-wkyr,-age)
  setorder(dk,status,-wkyr,-age,county,location)

  df[,locationName:=sprintf("%s (%s)",locationName,formatC(zscore,digits=2,format="f"))]
  dk[,locationName:=sprintf("%s (%s)",locationName,formatC(zscore,digits=2,format="f"))]

  df[status!="High",locationName:=""]
  dk[status!="High",locationName:=""]

  df[,status:=NULL]
  dk[,status:=NULL]

  dk[,location:=NULL]


  df1 <- df[,lapply(
    .SD, paste0, collapse= ", "
  ),by=.(
    wkyr,
    age,
    type
  )]
  df1[,zscore:=NULL]
  df1[,cumE1:=NULL]

  df2 <- df[locationName!="",.(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ),by=.(
    wkyr,
    age,
    type
  )]
  df3 <- df[stringr::str_detect(locationName, "Norge"),.(
    sumCumNorge = sum(cumE1)
  ),by=.(
    wkyr,
    age,
    type
  )]
  df <- merge(df1,df2,by=c("wkyr","age","type"),all.x=T)
  df <- merge(df,df3,by=c("wkyr","age","type"),all.x=T)
  df[is.na(meanZScore),meanZScore:=0]
  df[is.na(sumCum),sumCum:=0]
  df[is.na(sumCumNorge),sumCumNorge:=0]
  df[stringr::str_detect(locationName, "Norge"),sumCum:=sumCumNorge]
  df[,sumCumNorge:=NULL]
  df[,meanZScore:=formatC(meanZScore,digits=2,format="f")]
  df[meanZScore=="0.00",meanZScore:=""]
  df[,sumCum:=round(sumCum)]
  df[sumCum==0,sumCum:=NA]

  dk1 <- dk[,lapply(
    .SD, paste0, collapse= ", "
  ),by=.(
    wkyr,
    age,
    type,
    county,
    countyName
  )]
  dk1[,zscore:=NULL]
  dk1[,cumE1:=NULL]

  dk2 <- dk[locationName!="",.(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ),by=.(
    wkyr,
    age,
    type,
    county
  )]
  dk <- merge(dk1,dk2,by=c("wkyr","age","type","county"),all.x=T)
  dk[is.na(meanZScore),meanZScore:=0]
  dk[is.na(sumCum),sumCum:=0]
  dk[,meanZScore:=formatC(meanZScore,digits=2,format="f")]
  dk[meanZScore=="0.00",meanZScore:=""]
  dk[,sumCum:=round(sumCum)]
  dk[sumCum==0,sumCum:=NA]

  df[,locationName:=gsub(", , ","",locationName)]
  df[,locationName:=gsub(", $","",locationName)]
  df[,locationName:=gsub("^, ","",locationName)]
  setorder(df,type,-wkyr,-age)
  setnames(df,"locationName","High")

  df[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-19","20-29","30-64","65+"))]
  setorder(df,type,-wkyr,age)
  setcolorder(df,c("type","wkyr","age","High","meanZScore","sumCum"))

  dk[,locationName:=gsub(", , ","",locationName)]
  dk[,locationName:=gsub(", $","",locationName)]
  dk[,locationName:=gsub("^, ","",locationName)]
  dk[,age:=factor(age,levels=c("Totalt","0-4","5-14","15-19","20-29","30-64","65+"))]
  setorder(dk,type,-wkyr,age,county)
  dk[,county:=NULL]
  setcolorder(dk,c("type","wkyr","age","countyName","locationName","meanZScore","sumCum"))
  setnames(dk,"locationName","High")

  outbreaks <- list(df=df,dk=dk)
  if(!is.null(saveFiles)){
    SaveData(outbreaks, saveFiles)
  } else return(outbreaks)
}


#' test
#' @param df a
#' @param dk a
#' @param saveFiles a
#' @param alerts a
#' @import data.table
#' @import stringr
#' @export GenerateOutbreakListExternal
GenerateOutbreakListExternal <- function(df=readRDS(fhi::DashboardFolder("results","resYearLine.RDS")),
                                         dk=readRDS(fhi::DashboardFolder("results","resYearLineMunicip.RDS")),
                                         saveFiles=fhi::DashboardFolder("results","outbreaks_alert_external.RDS"),
                                         alerts = readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert.xlsx"))
){
  # variables used in data.table functions in this function
  . <- NULL
  status <- NULL
  displayDay <- NULL
  email <- NULL
  age <- NULL
  type <- NULL
  location <- NULL
  # end

  df <- df[displayDay==max(displayDay) & type %in% CONFIG$SYNDROMES_ALERT_EXTERNAL]
  dk <- dk[displayDay==max(displayDay) & type %in% CONFIG$SYNDROMES_ALERT_EXTERNAL]

  resultsk <- resultsf <- list()
  for(i in 1:nrow(alerts)){
    resultsf[[i]] <- df[status!="Normal" & stringr::str_detect(location, alerts$location[i])]
    resultsk[[i]] <- dk[status!="Normal" & stringr::str_detect(location, alerts$location[i])]

    resultsf[[i]][,email:=alerts$email[i]]
    resultsk[[i]][,email:=alerts$email[i]]
  }

  resultsf <- rbindlist(resultsf)
  resultsk <- rbindlist(resultsk)

  resultsf[,age:=factor(age,levels=CONFIG$AGES)]
  setorder(resultsf,type,location,age)

  resultsk[,age:=factor(age,levels=CONFIG$AGES)]
  setorder(resultsk,type,location,age)

  results <- rbind(resultsf,resultsk,fill=T)

  if(!is.null(saveFiles)){
    SaveData(results, saveFiles)
  } else return(results)
}

