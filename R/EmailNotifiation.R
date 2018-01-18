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
  <br><br>
  Se ogs\u00E5 p\u00E5 Signaler (ukentlig) b\u00E5de for fylker og kommuner og meld ifra til fagansvarlig dersom det st\u00E5r noe p\u00E5 disse sidene.
  <br><br>
  OBS: Nord og S\u00F8r-Tr\u00F8ndelag har fra 01.01.2018 blitt sl\u00E5tt sammen til Tr\u00F8ndelag. Det vil derfor bare v\u00E6re mulig \u00E5 finne Tr\u00F8ndelag i nedtrekkslisten for Fylkene.
  "

  if(Sys.getenv("COMPUTER")=="smhb" & sendEmail){
    #fhi::DashboardEmail("sykdomspuls_utbrudd",
                            "Nye tall for Sykdomspulsen",
                            emailText)

    try(CheckForOutbreaksUtbrudd(),TRUE)
    try(EmailNotificationKommuneLeger(),TRUE)
    try(EmailAlertExternal(),TRUE)
  }
  saveRDS(thisWeek,file=lastEmailedUtbruddFile)
}

#' test
#' @import fhi
#' @export EmailNotificationKommuneLeger
EmailNotificationKommuneLeger <- function(){

  emailText <- "
  Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er oppdatert med nye tall<br>
  Nye resultater vises p\u00E5 websiden om ca. 10 min.<br><br>
  Innlogging:<br>
  Webadresse: <a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a><br>
  Det er ikke noe brukernavn eller passord, du kommer direkte inn p\u00E5 nettsiden og den er klar til bruk.<br>
  Bruk Google Chrome n\u00E5r du logger deg inn.<br><br>
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
  <b>Ny funksjon for oversikt-siden</b><br>
  Etter tilbakemelding fra en av pilotbrukerne har vi forbedret funksjonen p\u00E5 oversikt-siden.
  N\u00E5 er det mulig \u00E5 klikke p\u00E5 feltene i diagrammet p\u00E5 oversikt-siden.
  Du vil da komme direkte til ukentlig-siden der grafen vil vise samme sykdom/symptom, kommune og aldersgruppe som du klikket p\u00E5.
  <br><br>
  OBS: Nord og S\u00F8r-Tr\u00F8ndelag har fra 01.01.2018 blitt sl\u00E5tt sammen til Tr\u00F8ndelag. Det vil derfor bare v\u00E6re mulig \u00E5 finne Tr\u00F8ndelag i nedtrekkslisten for Fylkene.
  <br><br>
  Hilsen:<br>
  Sykdomspulsen ved Folkehelseinstituttet<br>
  v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)<br><br><br>
  "

  if(Sys.getenv("COMPUTER")=="smhb"){
    fhi::DashboardEmail("sykdomspuls_kommuneleger",
                        "Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er oppdatert med nye tall",
                        emailText,
                        emailFooter=FALSE)
  } else if(Sys.getenv("COMPUTER")=="test"){
    fhi::DashboardEmail("test",
                        "Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er oppdatert med nye tall",
                        emailText,
                        emailFooter=FALSE)
  }
}

#' test
#' @param results a
#' @importFrom RAWmisc Format
#' @import fhi
#' @export EmailAlertExternal
EmailAlertExternal <- function(results=readRDS(fhi::DashboardFolder("results","outbreaks_alert_external.RDS"))){
  # variables used in data.table functions in this function
  output <- NULL
  type <- NULL
  locationName <- NULL
  location <- NULL
  age <- NULL
  cumE1 <- NULL
  zscore <- NULL
  email <- NULL
  # end

  if(nrow(results)==0){
    return(0)
  }
  emails <- unique(results$email)

  emailHeader <-
    "<style>
html {
  font-family: sans-serif;
}

table {
  border-collapse: collapse;
  border: 2px solid rgb(200,200,200);
  letter-spacing: 1px;
  font-size: 0.8rem;
}

td, th {
  border: 1px solid rgb(190,190,190);
  padding: 10px 20px;
}

th {
  background-color: rgb(235,235,235);
}

td {
  text-align: center;
}

tr:nth-child(even) td {
  background-color: rgb(250,250,250);
}

tr:nth-child(odd) td {
  background-color: rgb(245,245,245);
}

caption {
  padding: 10px;
}
</style>"

  results[,output:=sprintf("<tr> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> </tr>", type, locationName, location, age, round(cumE1), RAWmisc::Format(zscore,digits=2))]

  for(em in emails){
    r <- results[email %in% em]
    if(nrow(r)==0){
      next
    }
    emailText <- paste0(emailHeader,"Outbreaks:<br><br><table style='width:90%'><tr><th>Syndrome</th> <th>Location</th> <th>Location</th> <th>Age</th> <th>Excess</th> <th>Z-score</th></tr>")
    for(i in 1:nrow(r)){
      emailText <- sprintf("%s%s", emailText, r$output[i])
    }
    emailText <- sprintf("%s</table>", emailText)
    fhi::DashboardEmailSpecific(emailBCC = em,
                                emailSubject = "Outbreaks",
                                emailText = emailText)
    Sys.sleep(5)
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
