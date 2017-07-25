#' test
#' @param files a
#' @import fhi
#' @export EmailNotificationOfNewData
EmailNotificationOfNewData <- function(files){
  emailText <- paste0(
"New Sykdomspulsen data has been received and signal processing has begun.
<br><br>
New results should be available in around two hours.
<br><br>
Files being processed are: ",paste0(files,collapse=", "),"
")

  if(Sys.getenv("COMPUTER")=="smhb"){
    fhi::DashboardEmail("sykdomspuls_data",
                             "New Sykdomspuls data",
                             emailText)
  }
}

#' test
#' @import fhi
#' @export EmailNotificationOfNewResults
EmailNotificationOfNewResults <- function(){
  emailText <- "
  New Sykdomspulsen results available at <a href='http://smhb.fhi.no/'>http://smhb.fhi.no/</a>
  "
  if(Sys.getenv("COMPUTER")=="smhb"){
    fhi::DashboardEmail("sykdomspuls_results",
                            "New Sykdomspuls results available",
                            emailText)
  }
}

#' test
#' @param lastEmailedUtbruddFile a
#' @import fhi
#' @importFrom lubridate today
#' @export EmailNotificationUtbrudd
EmailNotificationUtbrudd <- function(lastEmailedUtbruddFile=fhi::DashboardFolder("results","lastEmailedUtbrudd.RDS")){
  thisWeek <- format.Date(lubridate::today(),"%U")
  sendEmail <- TRUE

  if(file.exists(lastEmailedUtbruddFile)){
    x <- readRDS(lastEmailedUtbruddFile)
    if(thisWeek==x){
      sendEmail <- FALSE
    }
  }
  emailText <- "
  Sykdomspulsen sin interne utbruddsovervåkning er nå oppdatert med nye tall.
  <br><br>
  Innlogging:<br>
  <a href='http://smhb.fhi.no/'>http://smhb.fhi.no/</a><br>
  NB! Bruk Google Chrome når du logger deg inn!<br>
  Brukernavn og passord står i arbeidsrutiner for utbruddsansvarlig.
  <br><br>
  Se på oversiktsbilde for de siste ukene for både mage-tarminfeksjoner og øvre luftveisinfeksjoner og meld ifra til fagansvarlig dersom:<br>
  - Et eller flere av fylkene har rød farge en av de to siste ukene<br>
  - Et eller flere av fylkene har gul farge to av de tre siste ukene
  "

  if(Sys.getenv("COMPUTER")=="smhb" & sendEmail){
    fhi::DashboardEmail("sykdomspuls_utbrudd",
                            "Nye tall for Sykdomspulsen",
                            emailText)

    CheckForOutbreaksUtbrudd()
  }
  saveRDS(thisWeek,file=lastEmailedUtbruddFile)
}

#' test
#' @import fhi
#' @export EmailNotificationOfFailedResults
EmailNotificationOfFailedResults <- function(){

  emailText <- "ERROR WITH SYKDOMSPULSEN DATA UPLOAD"
  if(Sys.getenv("COMPUTER")=="smhb"){
    fhi::DashboardEmail("sykdomspuls_data",
                            "New Sykdomspuls results available",
                            emailText)
  }
}
