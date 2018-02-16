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
#' @export EmailTechnicalNewResults
EmailTechnicalNewResults <- function(){
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
#' @param resYearLine a
#' @param isTest a
#' @import fhi
#' @import data.table
#' @export EmailInternal
EmailInternal <- function(
  resYearLine=readRDS(fhi::DashboardFolder("results","resYearLine.RDS")),
  isTest=TRUE){
  # variables used in data.table functions in this function
  status <- NULL
  location <- NULL
  statusNum0 <- NULL
  statusNum1 <- NULL
  statusNum2 <- NULL
  statusSum2weeks <- NULL
  statusSum3weeks <- NULL
  statusYellow <- NULL
  statusRed <- NULL
  age <- NULL
  wkyr <- NULL
  type <- NULL
  # end

  currentWeek <- max(resYearLine$wkyr)

  resYearLine[,statusNum0:=0]
  resYearLine[status=="Medium",statusNum0:=1]
  resYearLine[status=="High",statusNum0:=2]
  resYearLine[,statusNum1:=shift(statusNum0),by=location]
  resYearLine[,statusNum2:=shift(statusNum0,n=2L),by=location]
  resYearLine[,statusSum2weeks:=as.numeric(statusNum0==2)+as.numeric(statusNum1==2)]
  resYearLine[,statusSum3weeks:=as.numeric(statusNum0>=1)+as.numeric(statusNum1>=1)+as.numeric(statusNum2>=1)]

  resYearLine[,statusYellow:=0]
  resYearLine[statusSum3weeks>=2,statusYellow:=1]

  resYearLine[,statusRed:=0]
  resYearLine[statusSum2weeks>=1,statusRed:=1]

  resYearLine <- resYearLine[age=="Totalt"]
  outbreaks <- resYearLine[statusYellow==1 | statusRed==1,c("wkyr","type","location","locationName","status","statusYellow","statusRed"),with=F]
  setorder(outbreaks,-wkyr,location)

  outbreaksGastro <- unique(outbreaks[wkyr==currentWeek & type=="gastro"]$locationName)
  outbreaksRespiratory <- unique(outbreaks[wkyr==currentWeek & type=="respiratoryinternal"]$locationName)
  outbreaksInfluensa <- unique(outbreaks[wkyr==currentWeek & type=="influensa"]$locationName)
  outbreaksLungebetennelse <- unique(outbreaks[wkyr==currentWeek & type=="lungebetennelse"]$locationName)
  outbreaksBronkitt <- unique(outbreaks[wkyr==currentWeek & type=="bronkitt"]$locationName)

  if(length(outbreaksGastro)>0 |
     length(outbreaksRespiratory)>0 |
     length(outbreaksInfluensa)>0 |
     length(outbreaksLungebetennelse)>0 |
     length(outbreaksBronkitt)>0){

    outbreaksGastro <- paste0(outbreaksGastro,collapse=", ")
    outbreaksRespiratory <- paste0(outbreaksRespiratory,collapse=", ")
    outbreaksInfluensa <- paste0(outbreaksInfluensa,collapse=", ")
    outbreaksLungebetennelse <- paste0(outbreaksLungebetennelse,collapse=", ")
    outbreaksBronkitt <- paste0(outbreaksBronkitt,collapse=", ")

    if(outbreaksGastro=="") outbreaksGastro <- "Ingen"
    if(outbreaksRespiratory=="") outbreaksRespiratory <- "Ingen"
    if(outbreaksInfluensa=="") outbreaksInfluensa <- "Ingen"
    if(outbreaksLungebetennelse=="") outbreaksLungebetennelse <- "Ingen"
    if(outbreaksBronkitt=="") outbreaksBronkitt <- "Ingen"

    emailText <- sprintf("
                         OBS-Varsel fra Sykdomspulsen uke %s:
                         <br><br>
                         OBS varslet er basert p\u00E5 oversiktsbildet for de siste ukene for mage-tarminfeksjoner, luftveisinfeksjoner, og influensa.<br>
                         Det blir generert et varsel dersom:<br>
                         - Et eller flere av fylkene har r\u00F8d farge en av de to siste ukene<br>
                         - Et eller flere av fylkene har gul farge to av de tre siste ukene
                         <br><br>
                         Ved OBS varsel b\u00F8r mottaksansvarlig melde ifra til fagansvarlig i riktig avdeling.
                         <br><br>
                         <br><br>
                         Mage-tarminfeksjoner:
                         <br>
                         %s
                         <br><br>
                         Luftveisinfeksjoner:
                         <br>
                         %s
                         <br><br>
                         Influensa:
                         <br>
                         %s
                         <br><br>
                         Lungebetennelse:
                         <br>
                         %s
                         <br><br>
                         Akutt bronkitt/bronkiolitt:
                         <br>
                         %s
                         <br><br>
                         Se ogs\u00E5 p\u00E5 Signaler (ukentlig) b\u00E5de for fylker og kommuner og meld ifra til fagansvarlig dersom det st\u00E5r noe p\u00E5 disse sidene.
                         ",currentWeek,
                         outbreaksGastro,
                         outbreaksRespiratory,
                         outbreaksInfluensa,
                         outbreaksLungebetennelse,
                         outbreaksBronkitt)

    if(isTest){
      fhi::DashboardEmail("test",
                          emailSubject="TESTING EmailInternal",
                          emailText)
    } else {
      fhi::DashboardEmail("sykdomspuls_utbrudd",
                          sprintf("OBS-Varsel fra Sykdomspulsen uke %s",currentWeek),
                          emailText)
    }
  }
  return(0)
}


#' test
#' @param results a
#' @param alerts a
#' @param isTest a
#' @importFrom RAWmisc Format
#' @import fhi
#' @export EmailExternal
EmailExternal <- function(
  results=readRDS(fhi::DashboardFolder("results","outbreaks_alert_external.RDS")),
  alerts = readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert.xlsx")),
  isTest = TRUE){
  # variables used in data.table functions in this function
  output <- NULL
  type <- NULL
  locationName <- NULL
  location <- NULL
  age <- NULL
  cumE1 <- NULL
  zscore <- NULL
  email <- NULL
  link <- NULL
  county <- NULL
  # end

  setDT(alerts)
  emails <- unique(alerts$email)

  if(isTest){
    emailSubject <- "TESTING EmailAlertExternal"
    if(length(unique(alerts$email))>1) stop("THIS IS NOT A TEST EMAIL DATASET")
  } else {
    emailSubject <- "Nye Sykdomspulsen resultater"
  }

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
    </style>

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

  alerts[,output:=sprintf("<tr> <td>%s</td> </tr>", location)]

  results[,link:=sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>Klikk</a>",county,location,type,age)]
  results[is.na(county),link:=sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>Klikk</a>",location,location,type,age)]
  results[,output:=sprintf("<tr> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> </tr>", link, type, locationName, location, age, round(cumE1), RAWmisc::Format(zscore,digits=2))]

  for(em in emails){
    r <- results[email %in% em]
    a <- alerts[email %in% em]

    emailText <- paste0(emailHeader,"Registered for outbreaks in:<br><br><table style='width:90%'><tr><th>location</th></tr>")
    for(i in 1:nrow(a)){
      emailText <- sprintf("%s%s", emailText, a$output[i])
    }
    emailText <- sprintf("%s</table><br><br>", emailText)

    if(isTest){
      if(nrow(r)==0){
        emailText <- paste0(emailText,"Outbreaks:<br><br>No outbreaks recorded")
      } else {
        emailText <- paste0(emailText,"Outbreaks:<br><br><table style='width:90%'><tr><th>Til nettsiden</th> <th>Syndrome</th> <th>Location</th> <th>Location</th> <th>Age</th> <th>Excess</th> <th>Z-score</th></tr>")
        for(i in 1:nrow(r)){
          emailText <- sprintf("%s%s", emailText, r$output[i])
        }
        emailText <- sprintf("%s</table>", emailText)
      }
    }
    fhi::DashboardEmailSpecific(emailBCC = em,
                                emailSubject = emailSubject,
                                emailText = emailText)
    Sys.sleep(5)
  }
  return(0)
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


#' test
#' @param lastEmailedUtbruddFile a
#' @import fhi
#' @importFrom lubridate today
#' @export EmailNotificationOfNewResults
EmailNotificationOfNewResults <- function(lastEmailedUtbruddFile=fhi::DashboardFolder("results","lastEmailedUtbrudd.RDS")){
  thisWeek <- format.Date(lubridate::today(),"%U")
  sendEmail <- TRUE

  if(file.exists(lastEmailedUtbruddFile)){
    x <- readRDS(lastEmailedUtbruddFile)
    if(thisWeek==x){
      sendEmail <- FALSE
    }
  }

  try(EmailTechnicalNewResults(),TRUE)
  if(Sys.getenv("COMPUTER")=="smhb" & sendEmail){
    try(EmailInternal(isTest=FALSE),TRUE)
    try(EmailExternal(isTest=FALSE),TRUE)
  }
  saveRDS(thisWeek,file=lastEmailedUtbruddFile)
}
