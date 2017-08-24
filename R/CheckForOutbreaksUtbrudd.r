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
  outbreaksRespiratory <- unique(outbreaks[wkyr==currentWeek & type=="respiratory"]$locationName)
  outbreaksInfluensa <- unique(outbreaks[wkyr==currentWeek & type=="influensa"]$locationName)

  if(length(outbreaksGastro)>0 | length(outbreaksRespiratory)>0 | length(outbreaksInfluensa)>0){
    outbreaksGastro <- paste0(outbreaksGastro,collapse=", ")
    outbreaksRespiratory <- paste0(outbreaksRespiratory,collapse=", ")
    outbreaksInfluensa <- paste0(outbreaksInfluensa,collapse=", ")
    if(outbreaksGastro=="") outbreaksGastro <- "Ingen"
    if(outbreaksRespiratory=="") outbreaksRespiratory <- "Ingen"
    if(outbreaksInfluensa=="") outbreaksInfluensa <- "Ingen"

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

",currentWeek,outbreaksGastro,outbreaksRespiratory,outbreaksInfluensa)

    if(Sys.getenv("COMPUTER")=="smhb"){
      fhi::DashboardEmail("sykdomspuls_utbrudd",
                              sprintf("OBS-Varsel fra Sykdomspulsen uke %s",currentWeek),
                              emailText)
    }
  }

}
