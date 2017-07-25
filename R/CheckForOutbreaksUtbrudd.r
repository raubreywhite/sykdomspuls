#' test
#' @import fhi
#' @import data.table
#' @export CheckForOutbreaksUtbrudd
CheckForOutbreaksUtbrudd <- function(resYearLine=readRDS(fhi::DashboardFolder("results","resYearLine.RDS"))){
  currentWeek <- max(resYearLine$wkyr)
  #currentWeek <- "2006-13"
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

  if(length(outbreaksGastro)>0 | length(outbreaksRespiratory)>0){
    outbreaksGastro <- paste0(outbreaksGastro,collapse=", ")
    outbreaksRespiratory <- paste0(outbreaksRespiratory,collapse=", ")
    if(outbreaksGastro=="") outbreaksGastro <- "Ingen"
    if(outbreaksRespiratory=="") outbreaksRespiratory <- "Ingen"

    emailText <- sprintf("

                         De følgende trenger oppfølging for uke %s:
                         <br><br>
                         Mage-tarm diagnose:
                         <br>
                         %s
                         <br><br>
                         Øvre-luftvei diagnose:
                         <br>
                         %s
                         ",currentWeek,outbreaksGastro,outbreaksRespiratory)

    if(Sys.getenv("COMPUTER")=="smhb"){
      fhi::DashboardEmail("sykdomspuls_utbrudd",
                              "OBS-Varsel om Sykdomspulsen signaler",
                              emailText)
    }
  }

}
