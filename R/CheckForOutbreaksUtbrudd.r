#' test
#' @param resYearLine a
#' @import fhi
#' @import data.table
#' @export CheckForOutbreaksUtbrudd
CheckForOutbreaksUtbrudd <- function(resYearLine=readRDS(fhi::DashboardFolder("results","resYearLine.RDS"))){
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

    if(Sys.getenv("COMPUTER")=="smhb"){
      fhi::DashboardEmail("sykdomspuls_utbrudd",
                              sprintf("OBS-Varsel fra Sykdomspulsen uke %s",currentWeek),
                              emailText)
    }
  }

}
