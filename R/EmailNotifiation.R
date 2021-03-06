#' test
#' @param files a
#' @param isTest a
#' @import fhi
#' @export EmailNotificationOfNewData
EmailNotificationOfNewData <- function(files, isTest=TRUE) {
  emailText <- paste0(
    "New Sykdomspulsen data has been received and signal processing has begun.
<br><br>
New results should be available in around two hours.
<br><br>
Syndromes being processed are: ", paste0(CONFIG$SYNDROMES, collapse = ", "), "
<br><br>
Files being processed are: ", paste0(files, collapse = ", "), "
"
  )

  if (isTest) {
    fhi::DashboardEmail(
      "test",
      emailSubject = "TESTING EmailNotificationOfNewData",
      emailText
    )
  } else {
    fhi::DashboardEmail(
      "sykdomspuls_data",
      "New Sykdomspuls data",
      emailText
    )
  }

  return(0)
}

#' test
#' @import fhi
#' @export EmailTechnicalNewResults
EmailTechnicalNewResults <- function() {
  emailText <- "
  New Sykdomspulsen results available at <a href='http://smhb.fhi.no/'>http://smhb.fhi.no/</a>
  "
  if (Sys.getenv("COMPUTER") == "smhb") {
    fhi::DashboardEmail(
      "sykdomspuls_results",
      "New Sykdomspuls results available",
      emailText
    )
  }
}

#' test
#' @param resYearLine a
#' @param isTest a
#' @import fhi
#' @import data.table
#' @export EmailInternal
EmailInternal <- function(
                          resYearLine=readRDS(fhi::DashboardFolder("results", "resYearLine.RDS")),
                          isTest=TRUE) {
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

  resYearLine[, statusNum0 := 0]
  resYearLine[status == "Medium", statusNum0 := 1]
  resYearLine[status == "High", statusNum0 := 2]
  resYearLine[, statusNum1 := shift(statusNum0), by = location]
  resYearLine[, statusNum2 := shift(statusNum0, n = 2L), by = location]
  resYearLine[, statusSum2weeks := as.numeric(statusNum0 == 2) + as.numeric(statusNum1 == 2)]
  resYearLine[, statusSum3weeks := as.numeric(statusNum0 >= 1) + as.numeric(statusNum1 >= 1) + as.numeric(statusNum2 >= 1)]

  resYearLine[, statusYellow := 0]
  resYearLine[statusSum3weeks >= 2, statusYellow := 1]

  resYearLine[, statusRed := 0]
  resYearLine[statusSum2weeks >= 1, statusRed := 1]

  resYearLine <- resYearLine[age == "Totalt"]
  outbreaks <- resYearLine[statusYellow == 1 | statusRed == 1, c("wkyr", "type", "location", "locationName", "status", "statusYellow", "statusRed"), with = F]
  setorder(outbreaks, -wkyr, location)

  outbreaksGastro <- unique(outbreaks[wkyr == currentWeek & type == "gastro"]$locationName)
  outbreaksRespiratory <- unique(outbreaks[wkyr == currentWeek & type == "respiratoryinternal"]$locationName)
  outbreaksInfluensa <- unique(outbreaks[wkyr == currentWeek & type == "influensa"]$locationName)
  outbreaksLungebetennelse <- unique(outbreaks[wkyr == currentWeek & type == "lungebetennelse"]$locationName)
  outbreaksBronkitt <- unique(outbreaks[wkyr == currentWeek & type == "bronkitt"]$locationName)

  if (length(outbreaksGastro) > 0 |
    length(outbreaksRespiratory) > 0 |
    length(outbreaksInfluensa) > 0 |
    length(outbreaksLungebetennelse) > 0 |
    length(outbreaksBronkitt) > 0 |
    isTest) {
    outbreaksGastro <- paste0(outbreaksGastro, collapse = ", ")
    outbreaksRespiratory <- paste0(outbreaksRespiratory, collapse = ", ")
    outbreaksInfluensa <- paste0(outbreaksInfluensa, collapse = ", ")
    outbreaksLungebetennelse <- paste0(outbreaksLungebetennelse, collapse = ", ")
    outbreaksBronkitt <- paste0(outbreaksBronkitt, collapse = ", ")

    if (outbreaksGastro == "") outbreaksGastro <- "Ingen"
    if (outbreaksRespiratory == "") outbreaksRespiratory <- "Ingen"
    if (outbreaksInfluensa == "") outbreaksInfluensa <- "Ingen"
    if (outbreaksLungebetennelse == "") outbreaksLungebetennelse <- "Ingen"
    if (outbreaksBronkitt == "") outbreaksBronkitt <- "Ingen"

    emailText <- sprintf(
      "
                         OBS-Varsel fra Sykdomspulsen uke %s:
                         <br><br>
                         OBS varslet er basert p\u00E5 oversiktsbildet for de siste ukene for mage-tarminfeksjoner, luftveisinfeksjoner, og influensa.<br>
                         Det blir generert et varsel dersom:<br>
                         - Et eller flere av fylkene har r\u00F8d farge en av de to siste ukene<br>
                         - Et eller flere av fylkene har gul farge to av de tre siste ukene
                         <br><br>
                         Ved OBS varsel b\u00F8r mottaksansvarlig melde ifra til fagansvarlig i riktig avdeling.
                         <br><br>
Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en eller to konsultasjoner for et symptom/sykdom. Dette sees som oftest i sm\u00E5 kommuner der det vanligvis ikke er mange konsultasjoner. For ikke \u00E5 bli forstyrret av slike signaler har vi n\u00E5 lagt inn en nedre grense for gult signal p\u00E5 to konsultasjoner og en nedre grense for r\u00F8dt signal p\u00E5 tre konsultasjoner.<br><br>
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
                         ", currentWeek,
      outbreaksGastro,
      outbreaksRespiratory,
      outbreaksInfluensa,
      outbreaksLungebetennelse,
      outbreaksBronkitt
    )

    if (isTest) {
      fhi::DashboardEmail(
        "test",
        emailSubject = "TESTING EmailInternal",
        emailText
      )
    } else {
      fhi::DashboardEmail(
        "sykdomspuls_utbrudd",
        sprintf("OBS-Varsel fra Sykdomspulsen uke %s", currentWeek),
        emailText
      )
    }
  }
  return(0)
}

#' EmailExternalGenerateTable
#' @param results a
#' @param xtype a
#' @param xemail a
#' @import data.table
#' @importFrom RAWmisc Format RecodeDT
#' @export EmailExternalGenerateTable
EmailExternalGenerateTable <- function(results,xtype,xemail){
  . <- NULL
  zscore <- NULL
  link <- NULL
  county <- NULL
  location <- NULL
  type <- NULL
  age <- NULL
  type_pretty <- NULL
  output <- NULL
  locationName <- NULL
  cumE1 <- NULL
  email <- NULL

  setorder(results,-zscore)
  results[, link := sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>Klikk</a>", county, location, type, age)]
  results[is.na(county), link := sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>Klikk</a>", location, location, type, age)]
  # this turns "dirty" type (eg gastro) into "pretty" type (e.g. mage-tarm syndrome)
  results[,type_pretty:=type]
  RAWmisc::RecodeDT(results, switch = CONFIG$SYNDROMES, var = "type_pretty", oldOnLeft = FALSE)
  results[, output := sprintf("<tr> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> <td>%s</td> </tr>", link, type_pretty, locationName, location, age, n, round(cumE1), RAWmisc::Format(zscore, digits = 2))]

  r <- results[email == xemail & type==xtype]
  if(nrow(r)==0) return(sprintf("%s utbrudd:<br><br>Ingen utbrudd registrert",names(CONFIG$SYNDROMES)[CONFIG$SYNDROMES==xtype]))

  emailText <- sprintf("%s utbrudd:<br><br><table style='width:90%%'><tr><th>Til nettsiden</th> <th>Syndrom</th> <th>Geografisk omr\u00E5de</th> <th>Geografisk omr\u00E5de</th> <th>Alder</th> <th>Meldte tilfeller</th> <th>Eksess</th> <th>Z-verdi</th></tr>",names(CONFIG$SYNDROMES)[CONFIG$SYNDROMES==xtype])
  for (i in 1:nrow(r)) {
    emailText <- sprintf("%s%s", emailText, r$output[i])
  }
  emailText <- sprintf("%s</table>", emailText)

  return(emailText)
}

#' test
#' @param results a
#' @param alerts a
#' @param isTest a
#' @param forceNoOutbreak a
#' @param forceYesOutbreak a
#' @importFrom RAWmisc Format RecodeDT
#' @import fhi
#' @export EmailExternal
EmailExternal <- function(
                          results=readRDS(fhi::DashboardFolder("results", "outbreaks_alert_external.RDS")),
                          alerts = readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert.xlsx")),
                          isTest = TRUE,
                          forceNoOutbreak = FALSE,
                          forceYesOutbreak = FALSE) {
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
"

  if (isTest) {
    emailSubjectNoOutbreak <- "TESTING EmailAlertExternal"
    emailSubjectYesOutbreak <- "TESTING EmailAlertExternal"
    if (length(unique(alerts$email)) != 3) stop("THIS IS NOT A TEST EMAIL DATASET")
    if (forceNoOutbreak & forceYesOutbreak) stop("both forceNoOutbreak/forceYesOutbreak set")
  } else {
    if (forceNoOutbreak | forceYesOutbreak) stop("forceNoOutbreak/forceYesOutbreak set when not testing")
    emailSubjectNoOutbreak <- "Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er oppdatert med nye tall"
    emailSubjectYesOutbreak <- "OBS varsel fra Sykdomspulsen"
  }


  emailNoOutbreak <-
    "Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er oppdatert med nye tall.<br>
  Nye resultater vises p\u00E5 websiden om ca. 10 min.<br><br>
  Innlogging<br>
  Webadresse: <a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a><br>
  Det er ikke noe brukernavn eller passord, du kommer direkte inn p\u00E5 nettsiden og den er klar til bruk.
  Bruk Google Chrome n\u00E5r du logger deg inn.<br><br>
  NB! Dette er et pilotprosjekt. Du f\u00E5r n\u00E5 mulighet til \u00E5 bruke websiden n\u00E5r du vil og s\u00E5 mye du vil.
  Du kan ogs\u00E5 vise enkeltsider av websiden til andre som jobber innenfor kommunehelsetjenesten.
  MEN vi ber om at du ikke distribuerer webadressen til andre, hverken til ansatte i kommunehelsetjenesten eller utenfor.
  Det er fordi dette er et pilotprosjekt der vi \u00F8nsker \u00E5 ha oversikt over hvem som bruker systemet.
  Dersom noen andre enn deg \u00F8nsker \u00E5 f\u00E5 tilgang til websiden kan de kontakte oss p\u00E5 sykdomspulsen@fhi.no<br><br>
  NB! Fra n\u00E5 vil det bli sendt ut OBS varsel fra Sykdomspulsen.<br><br>
  OBS varselet inneb\u00E6rer at du vil f\u00E5 en mail dersom deres kommune eller fylke har flere konsultasjoner enn forventet av henholdsvis mage-tarminfeksjoner eller luftveisinfeksjoner sist uke.<br><br>
  E-posten vil ha overskrift <b>OBS varsel fra Sykdomspulsen uke xx</b>.<br><br>
  E-posten vil inneholde en tabell med informasjon om stedet der det er mer enn forventet antall konsultasjoner med aldersgruppe, antallet konsultasjoner som er over forventet verdi (excess) og en verdi som viser hvor ekstremt signalet er (z-score).<br><br>
  Varselet er en informasjon om at det kan v\u00E6re noe som b\u00F8r f\u00F8lges opp i deres kommune eller i et fylke. Det anbefales \u00E5 g\u00E5 inn i Sykdomspulsen websiden og sjekke det ut. Varselet beh\u00F8ver ikke \u00E5 bety noe alvorlig.<br><br>
  Nederst i denne mailen viser vi hvilke(n) kommune(r) du f\u00E5r varsel for. Alle f\u00E5r varsel for alle fylker og hele Norge. Dersom det ikke st\u00E5r noen kommune i tabellen mangler vi det for deg og ber deg kontakte oss for \u00E5 f\u00E5 satt opp riktig kommune(r).<br><br>
  Send oss gjerne en tilbakemelding dersom du \u00F8nsker varsel for andre kommuner. Vi \u00F8nsker ogs\u00E5 tilbakemelding p\u00E5 om dette varselet er nyttig for dere eller ikke p\u00E5 sykdomspulsen@fhi.no<br><br>
  Nord og S\u00F8r-Tr\u00F8ndelag har fra 01.01.2018 blitt sl\u00E5tt sammen til Tr\u00F8ndelag. Det vil derfor bare v\u00E6re mulig \u00E5 finne Tr\u00F8ndelag i nedtrekks listen for Fylkene.<br><br>
Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en eller to konsultasjoner for et symptom/sykdom. Dette sees som oftest i sm\u00E5 kommuner der det vanligvis ikke er mange konsultasjoner. For ikke \u00E5 bli forstyrret av slike signaler har vi n\u00E5 lagt inn en nedre grense for gult signal p\u00E5 to konsultasjoner og en nedre grense for r\u00F8dt signal p\u00E5 tre konsultasjoner.<br><br>
  Dersom du har problemer med websiden, forslag til forbedringer, ris eller ros kan du sende oss en mail: sykdomspulsen@fhi.no<br><br>
  Dersom du ikke \u00F8nsker \u00E5 f\u00E5 denne e-posten n\u00E5r vi oppdaterer Sykdomspulsen med nye tall s\u00E5 kan du gi oss beskjed ved \u00E5 sende en mail til adressen over.<br><br>
  Hilsen:<br>
  Sykdomspulsen ved Folkehelseinstituttet<br>
  v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)<br><br><br>
  "

  emailYesOutbreak <-
    "Dette er et OBS varsel fra Sykdomspulsen.<br><br>
  OBS varselet inneb\u00E6rer at alle dere som deltar i pilotprosjektet <b>Sykdomspulsen til kommunehelsetjenesten</b> f\u00E5r et varsel p\u00E5 e-post dersom deres kommune eller et fylke har flere konsultasjoner enn forventet av henholdsvis mage-tarminfeksjoner eller luftveisinfeksjoner sist uke.<br><br>
  Tabellen under viser informasjon om stedet der det er mer enn forventet antall konsultasjoner og aldersgruppe, antallet konsultasjoner som er over forventet verdi (excess) og en verdi som viser hvor ekstremt signalet er (z-score). Hvis z-scoret er mellom 2 og 4 er antallet konsultasjoner sist uke h\u00F8yere enn forventet og man vil se at det ligger i gul sone p\u00E5 Sykdomspulsen websiden. Dersom z-scoret er over 4 er antallet konsultasjoner sist uke betydelig h\u00F8yere enn forventet og man vil se at det ligger i r\u00F8d sone p\u00E5 Sykdomspulsen websiden.<br><br>
  I tabellen under er det en link til stedet der du kan se OBS varselet i Sykdomspulsen. Denne virker ikke dersom den \u00E5pnes i Internet explorer. Dersom du har problemer med linken kan du h\u00F8yreklikke p\u00E5 koblingen og kopiere den for deretter \u00E5 lime den inn i for eksempel Google chrome eller en annen nettleser. Du kan ogs\u00E5 logge deg inn p\u00E5 Sykdomspulsen p\u00E5 vanlig m\u00E5te (<a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a>) og selv finne aktuell kommune eller fylke.<br><br>
  Varselet er en informasjon om at det kan v\u00E6re noe som b\u00F8r f\u00F8lges opp i deres kommune eller i et fylke. Det anbefales \u00E5 g\u00E5 inn i Sykdomspulsen websiden og sjekke det ut. Varselet beh\u00F8ver ikke \u00E5 bety noe alvorlig.<br><br>
  Nederst i denne mailen viser vi hvilke(n) kommune(r) du f\u00E5r varsel for. Alle f\u00E5r varsel for alle fylker og hele Norge. Dersom det ikke st\u00E5r noen kommune i tabellen mangler vi det for deg og ber deg kontakte oss for \u00E5 f\u00E5 satt opp riktig kommune(r).<br><br>
Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en eller to konsultasjoner for et symptom/sykdom. Dette sees som oftest i sm\u00E5 kommuner der det vanligvis ikke er mange konsultasjoner. For ikke \u00E5 bli forstyrret av slike signaler har vi n\u00E5 lagt inn en nedre grense for gult signal p\u00E5 to konsultasjoner og en nedre grense for r\u00F8dt signal p\u00E5 tre konsultasjoner.<br><br>
  Ta kontakt med oss om du har sp\u00F8rsm\u00E5l eller om det er noe som er uklart p\u00E5 sykdomspulsen@fhi.no.<br><br>
  Send oss ogs\u00E5 en tilbakemelding dersom du \u00F8nsker varsel for andre kommuner eller fylker.<br><br>
  Vi \u00F8nsker ogs\u00E5 tilbakemelding p\u00E5 om dette varselet er nyttig for dere eller ikke.<br><br>
  Hilsen:<br>
  Sykdomspulsen ved Folkehelseinstituttet<br>
  v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)<br><br>
  "

  alerts[, output := sprintf("<tr> <td>%s</td> </tr>", location)]

  for (em in emails) {
    r <- results[email %in% em]
    a <- alerts[email %in% em]

    noOutbreak <- nrow(r) == 0
    if (forceNoOutbreak) noOutbreak <- TRUE
    if (forceYesOutbreak) noOutbreak <- FALSE

    # no outbreaks
    if (noOutbreak) {
      emailText <- paste0(emailHeader, emailNoOutbreak)
      emailSubject <- emailSubjectNoOutbreak
      useEmail <- "xxxxxxx"
    } else {
      emailText <- paste0(emailHeader, emailYesOutbreak)
      emailSubject <- emailSubjectYesOutbreak
      useEmail <- em
    }

    # include registered places
    emailText <- paste0(emailText, "Du er registrert for \u00E5 motta varsel om utbrudd i:<br><br><table style='width:90%'><tr><th>location</th></tr>")
    for (i in 1:nrow(a)) {
      emailText <- sprintf("%s%s", emailText, a$output[i])
    }
    emailText <- sprintf("%s</table><br><br>", emailText)

    # include outbreaks
    for(type in CONFIG$SYNDROMES_ALERT_EXTERNAL){
      emailText <- paste0(emailText,EmailExternalGenerateTable(results=r,xtype=type,xemail=useEmail),"<br><br>")
    }

    fhi::DashboardEmailSpecific(
      emailBCC = em,
      emailSubject = emailSubject,
      emailText = emailText
    )
    Sys.sleep(5)
  }
  return(0)
}

#' test
#' @import fhi
#' @export EmailNotificationOfFailedResults
EmailNotificationOfFailedResults <- function() {
  emailText <- "ERROR WITH SYKDOMSPULSEN DATA UPLOAD"
  if (Sys.getenv("COMPUTER") == "smhb") {
    fhi::DashboardEmail(
      "sykdomspuls_data",
      "New Sykdomspuls results available",
      emailText
    )
  }
}


#' test
#' @param lastEmailedUtbruddFile a
#' @import fhi
#' @importFrom lubridate today
#' @export EmailNotificationOfNewResults
EmailNotificationOfNewResults <- function(lastEmailedUtbruddFile=fhi::DashboardFolder("results", "lastEmailedUtbrudd.RDS")) {
  thisWeek <- format.Date(lubridate::today(), "%U")
  sendEmail <- TRUE

  if (file.exists(lastEmailedUtbruddFile)) {
    x <- readRDS(lastEmailedUtbruddFile)
    if (thisWeek == x) {
      sendEmail <- FALSE
    }
  }

  try(EmailTechnicalNewResults(), TRUE)
  if (Sys.getenv("COMPUTER") == "smhb" & sendEmail) {
    try(EmailInternal(isTest = FALSE), TRUE)
    try(EmailExternal(isTest = FALSE), TRUE)
  }
  saveRDS(thisWeek, file = lastEmailedUtbruddFile)
}
