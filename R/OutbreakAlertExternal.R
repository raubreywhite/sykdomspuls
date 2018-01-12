#' test
#' @param df a
#' @param dk a
#' @param saveFiles a
#' @param alerts a
#' @import data.table
#' @import stringr
#' @export OutbreakAlertExternal
OutbreakAlertExternal <- function(df=readRDS(fhi::DashboardFolder("results","resYearLine.RDS")),
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
