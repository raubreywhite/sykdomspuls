#' test
#' @import data.table
#' @import stringr
#' @import stats
#' @export QuasipoissonAlgorithm
QuasipoissonAlgorithm = function(
  dataset,
  predinterval=30,
  historical.data.years=5,
  mod.pred.window=90,
  reweights=1,
  remove.pandemic.year=F,
  remove.highcounts=0,
  sign.level=0.05,
  isDaily=TRUE,
  v=1){

  #FUNCTION quasipoisson.algorithm
  #
  #Description: Applys a surveillance algorithm based on a quasi-poisson regression
  #model to the selected data. The difference from the Farrington algorithm is in how
  #seasonality is accounted for (here it is adjusted for, in Farrington it is removed
  #by design.
  #
  #Input (arguments):
  #dataset: data frame with columns for number of cases, covariates and dates per day
  #datecol: name of date-column (default: 'Dato')
  #predinterval: length of prediction interval (default: 30; last 30 days)
  #historical.data.years: number (should be greater or equal to at least 3) of full years of background data (default: 5)
  #mod.pred.window: number (greater or equal to 0) of days window between datasets used for modelling and prediction(default: 90)
  #reweights: number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
  #remove.pandemic.year: true/false (default: false; keep 2009 data)
  #remove.highcounts: number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
  #sign.level: significance level for the prediction intervals (default: 5%)

  dataset[consult==0,consult:=1]
  dataset[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  dataset[,week := data.table::isoweek(date)] #ISO-week, instead of others (%W and %U)

  #SET REGRESSION FORMULA:
  if(isDaily){
    regformula = n ~ offset(log(consult)) + trend + factor(dayOfWeek) + sin(2*pi*dayOfYear/366) + cos(2*pi*dayOfYear/366) + HelligdagIndikator
    dataset[,trend := 1:.N]
    dataset[,dayOfYear:=data.table::yday(date)]
    dataset[,dayOfWeek:=data.table::wday(date)]

    startpreddate = max(dataset$trend) - predinterval
    stoppreddate = max(dataset$trend)
    #Define period for modelling data (with or without pandemic):
    stopmoddate=startpreddate-1-mod.pred.window

    #If chose, remove pandemic season:
    if(remove.pandemic.year==F){
      startmoddate=stopmoddate-5*366
      moddates = startmoddate:stopmoddate
    }
    if(remove.pandemic.year==T){
      startmoddate=stopmoddate-6*366
      moddates = startmoddate:stopmoddate
      temp = dataset[year == 2009]$trend
      moddates = moddates[!(modates %in% temp)]
    }

  } else {
    dataset <- dataset[year>=2006 & week %in% 1:52]
    dataset <- dataset[,.(n=sum(n),consult=sum(consult),pop=sum(pop),HelligdagIndikator=mean(HelligdagIndikator)),by=.(date,year,week)]
    dataset <- dataset[,.(n=sum(n),consult=sum(consult),pop=mean(pop),HelligdagIndikator=mean(HelligdagIndikator)),by=.(year,week)]
    dataset[consult==0,consult:=1]
    regformula = n ~ offset(log(consult)) + trend + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + HelligdagIndikator
    dataset[,trend:= .I]

    startpreddate = max(dataset$trend) - ceiling(predinterval/7)
    stoppreddate = max(dataset$trend)
    #Define period for modelling data (with or without pandemic):
    stopmoddate=startpreddate-1-round(mod.pred.window/7)

    #If chose, remove pandemic season:
    if(remove.pandemic.year==F){
      startmoddate=stopmoddate-5*52
      moddates = startmoddate:stopmoddate
    }
    if(remove.pandemic.year==T){
      startmoddate=stopmoddate-6*52
      moddates = startmoddate:stopmoddate
      temp = dataset[year == 2009]$trend
      moddates = moddates[!(modates %in% temp)]
    }
  }

  #Define testset for prediction:
  dataset.test <- copy(dataset) #[trend %in% startpreddate:stoppreddate]
  #Define trainingset for modelling:
  dataset.training = dataset[trend %in% moddates]

  #If chosen, remove upper given percentage of counts from the prediction:
  if(remove.highcounts>0){
    dataset.training = dataset.training[n < quantile(n,(1-remove.highcounts)),]
  }
  #FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  poisreg = glm(regformula,data=dataset.training,family=quasipoisson,na.action=na.omit)

  #REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
  w_i = rep(1,nrow(dataset.training))
  dataset.training = cbind(dataset.training,w_i)

  for(i in sort(1:reweights)){
    dispersion_parameter = summary(poisreg)$dispersion
    if (i == 0) {
      break
    }
    anscombe.res = anscombe.residuals(poisreg, dispersion_parameter)
    anscombe.res[anscombe.res < 1] = 1 #Alt. 2.58?
    dataset.training[, w_i := anscombe.res ^ (-2)] #The weight
    Gamma = nrow(dataset.training) / sum(dataset.training$w_i)
    dataset.training[, w_i := Gamma * w_i] #Makes sum(w_i) = n
    poisreg = glm(regformula, data = dataset.training, weights = w_i, family = quasipoisson, na.action = na.omit)
    dispersion_parameter = summary(poisreg)$dispersion
    od <- max(1,sum(poisreg$weights * poisreg$residuals^2)/poisreg$df.r)
  }

  #CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
  pred = predict(poisreg,type="response",se.fit=T,newdata=dataset.test)
  dataset.test[, threshold0 := pred$fit]
  dataset.test[, threshold2 := FarringtonThreshold(pred, phi=dispersion_parameter, z=2, skewness.transform="2/3")]
  dataset.test[, threshold4 := FarringtonThreshold(pred, phi=dispersion_parameter, z=4, skewness.transform="2/3")]
  dataset.test[, threshold6 := FarringtonThreshold(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3")]
  dataset.test[, cumE1 := n - threshold2]
  dataset.test[, zscore := FarringtonZscore(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3", y = n)]

  if(isDaily){
    return(dataset.test[,c(variablesAlgorithmDaily,variablesAlgorithmBasic,variablesAlgorithmProduced),with=F])
  } else {
    return(dataset.test[,c(variablesAlgorithmWeekly,variablesAlgorithmBasic,variablesAlgorithmProduced),with=F])
  }
}


#' test
#' @import data.table
#' @import stringr
#' @import stats
#' @export quasipoisson.algorithm
quasipoisson.algorithm = function(
  dataset,
  predinterval=30,
  historical.data.years=5,
  mod.pred.window=90,
  reweights=1,
  remove.pandemic.year=F,
  remove.highcounts=0,
  sign.level=0.05,
  v=1){

  #FUNCTION quasipoisson.algorithm
  #
  #Description: Applys a surveillance algorithm based on a quasi-poisson regression
  #model to the selected data. The difference from the Farrington algorithm is in how
  #seasonality is accounted for (here it is adjusted for, in Farrington it is removed
  #by design.
  #
  #Input (arguments):
  #dataset: data frame with columns for number of cases, covariates and dates per day
  #datecol: name of date-column (default: 'Dato')
  #predinterval: length of prediction interval (default: 30; last 30 days)
  #historical.data.years: number (should be greater or equal to at least 3) of full years of background data (default: 5)
  #mod.pred.window: number (greater or equal to 0) of days window between datasets used for modelling and prediction(default: 90)
  #reweights: number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
  #remove.pandemic.year: true/false (default: false; keep 2009 data)
  #remove.highcounts: number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
  #sign.level: significance level for the prediction intervals (default: 5%)

  #SET REGRESSION FORMULA:
  regformula = n ~ offset(log(consult)) + trend + factor(dayOfWeek) + sin(2*pi*dayOfYear/366) + cos(2*pi*dayOfYear/366) + HelligdagIndikator

  #CREATE ADDITIONAL VARIABLES NEEDED:
  dataset[consult==0,consult:=1]
  dataset[,trend := 1:.N]
  dataset[,dayOfYear:=as.numeric(strftime(date,format="%j"))]
  dataset[,dayOfWeek:=as.numeric(strftime(date,format="%u"))]

  #DIVIDE THE DATASET INTO A TRAINING SET AND A TEST SET:
  dates = as.Date(sort(unique(dataset$date)))
  #Define start and stop for prediction data:
  startpreddate=dates[length(dates)-predinterval+1]
  stoppreddate=dates[length(dates)]
  #Define period for modelling data (with or without pandemic):
  stopmoddate=dates[length(dates)-predinterval-mod.pred.window]
  startmoddate=seq(stopmoddate, length=2, by="-5 years")[2]
  if(remove.pandemic.year==T){
    startmoddate=seq(stopmoddate, length=2, by="-6 years")[2]
    moddates = seq.Date(startmoddate,stopmoddate,"days")
    moddates = moddates[format(moddates,'%Y') != 2009]
  }
  if(remove.pandemic.year==F){
    startmoddate=seq(stopmoddate, length=2, by="-5 years")[2]
    moddates = seq.Date(startmoddate,stopmoddate,"days")
  }
  #Define testset for prediction:
  dataset.test = dataset #[date>=startpreddate]
  #Define trainingset for modelling:
  dataset.training = dataset[date %in% as.character(moddates)]

  #If chosen, remove upper given percentage of counts from the prediction:
  if(remove.highcounts>0){
    dataset.training = dataset.training[n < quantile(n,(1-remove.highcounts)),]
  }
  dataset.training <- dataset.training[consult>0]
  #FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  poisreg = glm(regformula,data=dataset.training,family=quasipoisson,na.action=na.omit)

  #REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
  w_i = rep(1,nrow(dataset.training))
  dataset.training = cbind(dataset.training,w_i)

  for(i in sort(1:reweights)){
    dispersion_parameter = summary(poisreg)$dispersion
    if (i == 0) {
      break
    }
    anscombe.res = anscombe.residuals(poisreg, dispersion_parameter)
    anscombe.res[anscombe.res < 1] = 1 #Alt. 2.58?
    dataset.training[, w_i := anscombe.res ^ (-2)] #The weight
    Gamma = nrow(dataset.training) / sum(dataset.training$w_i)
    dataset.training[, w_i := Gamma * w_i] #Makes sum(w_i) = n
    poisreg = glm(regformula, data = dataset.training, weights = w_i, family = quasipoisson, na.action = na.omit)
    dispersion_parameter = summary(poisreg)$dispersion
    }

  #CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
  pred = predict(poisreg,type="response",se.fit=T,newdata=dataset.test)
  dataset.test[, threshold0 := pred$fit]
  #dataset.test[,threshold2 := pred$fit*((1+(2/3)*qnorm(1-(0.05/2))*((((dispersion_parameter*pred$fit)+(pred$se.fit^2))/(pred$fit^2))^(1/2)))^(3/2))]
  dataset.test[, threshold2 := FarringtonThreshold(pred, phi=dispersion_parameter, z=2, skewness.transform="2/3")]
  dataset.test[, threshold4 := FarringtonThreshold(pred, phi=dispersion_parameter, z=4, skewness.transform="2/3")]
  dataset.test[, threshold6 := FarringtonThreshold(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3")]
  dataset.test[, zscore := FarringtonZscore(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3", y = n)]

  dataset.test[, trend := NULL]
  dataset.test[, dayOfWeek := NULL]
  dataset.test[, dayOfYear := NULL]

  return(dataset.test[,c(variablesAlgorithmDaily,variablesAlgorithmBasic,variablesAlgorithmProduced),with=F])
}

#' test
#' @import data.table
#' @import stringr
#' @import stats
#' @export quasipoisson.algorithm.week
quasipoisson.algorithm.week = function(
  dataset,
  predinterval=30,
  historical.data.years=5,
  mod.pred.window=90,
  reweights=1,
  remove.pandemic.year=F,
  remove.highcounts=0,
  sign.level=0.05,
  predintervalweeks=NULL,
  v=1){

  #FUNCTION quasipoisson.algorithm
  #
  #Description: Applys a surveillance algorithm based on a quasi-poisson regression
  #model to the selected data. The difference from the Farrington algorithm is in how
  #seasonality is accounted for (here it is adjusted for, in Farrington it is removed
  #by design.
  #
  #Input (arguments):
  #dataset: data frame with columns for number of cases, covariates and dates per day
  #datecol: name of date-column (default: 'Dato')
  #predinterval: length of prediction interval (default: 30; last 30 days)
  #historical.data.years: number (should be greater or equal to at least 3) of full years of background data (default: 5)
  #mod.pred.window: number (greater or equal to 0) of days window between datasets used for modelling and prediction(default: 90)
  #reweights: number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
  #remove.pandemic.year: true/false (default: false; keep 2009 data)
  #remove.highcounts: number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
  #sign.level: significance level for the prediction intervals (default: 5%)

  dataset[,year := format.Date(date,"%G")] #Week-based year, instead of normal year (%Y)
  dataset[,week := as.numeric(format.Date(date,"%V"))] #ISO-week, instead of others (%W and %U)
  dataset <- dataset[year>=2006 & week %in% 1:52]
  dataset <- dataset[,.(n=sum(n),consult=sum(consult),pop=sum(pop),HelligdagIndikator=mean(HelligdagIndikator)),by=.(date,year,week)]
  dataset <- dataset[,.(n=sum(n),consult=sum(consult),pop=mean(pop),HelligdagIndikator=mean(HelligdagIndikator)),by=.(year,week)]
  dataset[consult==0,consult:=1]

  #SET REGRESSION FORMULA:
  regformula = n ~ offset(log(consult)) + trend + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + HelligdagIndikator

  #CREATE ADDITIONAL VARIABLES NEEDED:
  dataset[,trend:= .I]

  #DIVIDE THE DATASET INTO A TRAINING SET AND A TEST SET:

  if(is.null(predintervalweeks)){
    startpreddate = max(dataset$trend) - ceiling(predinterval/7)
  } else startpreddate = max(dataset$trend) - predintervalweeks + 1
  stoppreddate = max(dataset$trend)
  #Define period for modelling data (with or without pandemic):
  stopmoddate=startpreddate-1-round(mod.pred.window/7)
  #If chose, remove pandemic season:
  if(remove.pandemic.year==F){
    startmoddate=stopmoddate-5*52
    moddates = startmoddate:stopmoddate
  }
  if(remove.pandemic.year==T){
    startmoddate=stopmoddate-6*52
    moddates = startmoddate:stopmoddate
    temp = dataset[year == 2009]$trend
    moddates = moddates[!(modates %in% temp)]
  }

  #Define testset for prediction:
  dataset.test <- copy(dataset) #[trend %in% startpreddate:stoppreddate]
  #Define trainingset for modelling:
  dataset.training = dataset[trend %in% moddates]

  #If chosen, remove upper given percentage of counts from the prediction:
  if(remove.highcounts>0){
    dataset.training = dataset.training[n < quantile(n,(1-remove.highcounts)),]
  }
  dataset.training <- dataset.training[consult>0]
  #FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  poisreg = glm(regformula,data=dataset.training,family=quasipoisson,na.action=na.omit)

  #REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
  w_i = rep(1,nrow(dataset.training))
  dataset.training = cbind(dataset.training,w_i)

  for(i in sort(1:reweights)){
    dispersion_parameter = summary(poisreg)$dispersion
    if (i == 0) {
      break
    }
    anscombe.res = anscombe.residuals(poisreg, dispersion_parameter)
    anscombe.res[anscombe.res < 1] = 1 #Alt. 2.58?
    dataset.training[, w_i := anscombe.res ^ (-2)] #The weight
    Gamma = nrow(dataset.training) / sum(dataset.training$w_i)
    dataset.training[, w_i := Gamma * w_i] #Makes sum(w_i) = n
    poisreg = glm(regformula, data = dataset.training, weights = w_i, family = quasipoisson, na.action = na.omit)
    dispersion_parameter = summary(poisreg)$dispersion
    od <- max(1,sum(poisreg$weights * poisreg$residuals^2)/poisreg$df.r)
    }

  #CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
  pred = predict(poisreg,type="response",se.fit=T,newdata=dataset.test)
  dataset.test[, threshold0 := pred$fit]
  #dataset.test[,threshold2 := pred$fit*((1+(2/3)*qnorm(1-(0.05/2))*((((dispersion_parameter*pred$fit)+(pred$se.fit^2))/(pred$fit^2))^(1/2)))^(3/2))]
  dataset.test[, threshold2 := FarringtonThreshold(pred, phi=dispersion_parameter, z=2, skewness.transform="2/3")]
  dataset.test[, threshold4 := FarringtonThreshold(pred, phi=dispersion_parameter, z=4, skewness.transform="2/3")]
  dataset.test[, threshold6 := FarringtonThreshold(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3")]
  dataset.test[, zscore := FarringtonZscore(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3", y = n)]

  return(dataset.test[,c(variablesAlgorithmWeekly,variablesAlgorithmBasic,variablesAlgorithmProduced),with=F])

}
