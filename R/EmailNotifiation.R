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
  Sykdomspulsen sin interne utbruddsoverv\u00E5kning er n\u00E5 oppdatert med nye tall.
  <br><br>
  Innlogging:<br>
  <a href='http://smhb.fhi.no/'>http://smhb.fhi.no/</a><br>
  NB! Bruk Google Chrome n\u00E5r du logger deg inn!<br>
  Brukernavn og passord st\u00E5r i arbeidsrutiner for utbruddsansvarlig.
  <br><br>
  Se p\u00E5 oversiktsbilde for de siste ukene for b\u00E5de mage-tarminfeksjoner og \u00F8vre luftveisinfeksjoner og meld ifra til fagansvarlig dersom:<br>
  - Et eller flere av fylkene har r\u00F8d farge en av de to siste ukene<br>
  - Et eller flere av fylkene har gul farge to av de tre siste ukene
  "

  if(Sys.getenv("COMPUTER")=="smhb" & sendEmail){
    fhi::DashboardEmail("sykdomspuls_utbrudd",
                            "Nye tall for Sykdomspulsen",
                            emailText)

    CheckForOutbreaksUtbrudd()
    EmailNotificationKommuneLeger()
  }
  saveRDS(thisWeek,file=lastEmailedUtbruddFile)
}

#' test
#' @import fhi
#' @export EmailNotificationKommuneLeger
EmailNotificationKommuneLeger <- function(){

  emailText <- "
  Nye resultater vises p\u00E5 websiden om ca. 10 min.<br><br>
  Innlogging:<br>
  Webadresse: <a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a><br>
  Det er ikke noe brukernavn eller passord, du kommer direkte inn p\u00E5 nettsiden og den er klar til bruk.<br>
  NB! Bruk Google Chrome n\u00E5r du logger deg inn!<br>
  NB! Dette er et pilotprosjekt. Du f\u00E5r n\u00E5 mulighet til \u00E5 bruke websiden n\u00E5r du vil og s\u00E5 mye du vil.<br>
  Du kan ogs\u00E5 vise enkeltsider av websiden til andre som jobber innenfor kommunehelsetjenesten.<br>
  MEN vi ber om at du ikke distribuerer webadressen til andre, hverken til ansatte i kommunehelsetjenesten eller utenfor.<br>
  Det er fordi dette er et pilotprosjekt der vi \u00F8nsker \u00E5 ha oversikt over hvem som bruker systemet.<br>
  Dersom noen andre enn deg \u00F8nsker \u00E5 f\u00E5 tilgang til websiden kan de kontakte oss p\u00E5 sykdomspulsen@fhi.no
  <br><br>
  Vi \u00F8nsker tilbakemeldinger!
  <br><br>
  Dersom du har problemer med websiden, forslag til forbedringer, ris eller ros kan du sende oss en mail: sykdomspulsen@fhi.no<br>
  Dersom du ikke \u00F8nsker \u00E5 f\u00E5 denne e-posten n\u00E5r vi oppdaterer Sykdomspulsen med nye tall s\u00E5 kan du gi oss beskjed ved \u00E5 sende en mail til adressen over.
  <br><br>
  Hilsen:<br>
  Sykdomspulsen ved Folkehelseinstituttet<br>
  v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)
  "

  if(Sys.getenv("COMPUTER")=="smhb"){
    fhi::DashboardEmail("sykdomspuls_kommuneleger",
                        "Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er n\u00E5 oppdatert med nye tall",
                        emailText)
  }
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
