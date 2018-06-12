FormatDatasetDaily <- function(data){
  # variables used in data.table functions in this function
  . <- NULL
  trend <- NULL
  dayOfYear <- NULL
  dayOfWeek <- NULL
  consult <- NULL
  #end

  data[, trend:=as.numeric(date)-13000]
  data[,dayOfYear:=data.table::yday(date)]
  data[,dayOfWeek:=data.table::wday(date)]
  data[consult==0,consult:=1]

  return(data)
}

FormatDatasetWeekly <- function(data){
  # variables used in data.table functions in this function
  . <- NULL
  n <- NULL
  consult <- NULL
  trend <- NULL
  pop <- NULL
  HelligdagIndikator <- NULL
  #end

  data <- data[year>=2006 & week %in% 1:52]
  data[, trend:=as.numeric(date)-13000]
  data <- data[,.(n=sum(n),
                  consult=sum(consult),
                  pop=sum(pop),
                  HelligdagIndikator=mean(HelligdagIndikator),
                  trend=mean(trend)),
               by=.(date,year,week)]

  data <- data[,.(n=sum(n),
                  consult=sum(consult),
                  pop=mean(pop),
                  HelligdagIndikator=mean(HelligdagIndikator),
                  trend=mean(trend)),
               by=.(year,week)]

  data[consult==0,consult:=1]

  return(data)
}


#' test
#' @param datasetTrain a
#' @param datasetPredict a
#' @param reweights a
#' @param remove.pandemic.year a
#' @param remove.highcounts a
#' @param sign.level a
#' @param isDaily a
#' @param v a
#' @importFrom glm2 glm2
#' @import data.table
#' @import stringr
#' @import stats
#' @export QuasipoissonTrainPredictData
QuasipoissonTrainPredictData = function(
  datasetTrain,
  datasetPredict,
  reweights=1,
  remove.pandemic.year=F,
  remove.highcounts=0,
  sign.level=0.05,
  isDaily=TRUE,
  v=1){
  # variables used in data.table functions in this function
  consult <- NULL
  n <- NULL
  threshold0 <- NULL
  threshold2 <- NULL
  threshold4 <- NULL
  threshold6 <- NULL
  zscore <- NULL
  cumE1 <- NULL
  cumL1 <- NULL
  cumU1 <- NULL
  failed <- NULL
  revcumE1 <- NULL
  revcumL1 <- NULL
  revcumU1 <- NULL
  # end

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

  datasetTrain[consult==0,consult:=1]
  datasetTrain[,year := as.numeric(format.Date(date,"%G"))] #Week-based year, instead of normal year (%Y)
  datasetTrain[,week := as.numeric(format.Date(date,"%V"))] #Week-based year, instead of normal year (%Y)
  #datasetTrain[,week := data.table::isoweek(date)] #ISO-week, instead of others (%W and %U)

  datasetPredict[consult==0,consult:=1]
  datasetPredict[,year := as.numeric(format.Date(date,"%G"))] #Week-based year, instead of normal year (%Y)
  datasetPredict[,week := as.numeric(format.Date(date,"%V"))] #Week-based year, instead of normal year (%Y)
  #datasetPredict[,week := data.table::isoweek(date)] #ISO-week, instead of others (%W and %U)

  #SET REGRESSION FORMULA:
  if(isDaily){
    regformula = n ~ offset(log(consult)) + trend + factor(dayOfWeek) + sin(2*pi*dayOfYear/366) + cos(2*pi*dayOfYear/366) + HelligdagIndikator

    datasetTrain <- FormatDatasetDaily(datasetTrain)
    datasetPredict <- FormatDatasetDaily(datasetPredict)
  } else {
    regformula = n ~ offset(log(consult)) + trend + sin(2*pi*(week-1)/52) + cos(2*pi*(week-1)/52) + HelligdagIndikator

    datasetTrain <- FormatDatasetWeekly(datasetTrain)
    datasetPredict <- FormatDatasetWeekly(datasetPredict)
  }

  if(remove.pandemic.year==T){
    datasetTrain <- datasetTrain[year!="2009"]
  }

  #If chosen, remove upper given percentage of counts from the prediction:
  if(remove.highcounts>0){
    datasetTrain = datasetTrain[n < quantile(n,(1-remove.highcounts)),]
  }

  #FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  normalFunction <- function(regformula, datasetTrain){
    fit <- glm2::glm2(regformula,data=datasetTrain,family=quasipoisson,na.action=na.omit)
    return(list(fit=fit, failed=!fit$converged))
  }
  exceptionalFunction <- function(err){
    return(list(fit=NaN, failed=TRUE))
  }
  poisreg <- tryCatch(normalFunction(regformula, datasetTrain), error=exceptionalFunction, warning=exceptionalFunction)

  if(poisreg$failed){
    datasetPredict[, threshold0 := 0.0]
    datasetPredict[, threshold2 := 5.0]
    datasetPredict[, threshold4 := 10.0]
    datasetPredict[, threshold6 := 15.0]
    datasetPredict[, zscore := 0.0]

    datasetPredict[, cumE1 := 0.0]
    datasetPredict[, cumL1 := -5.0]
    datasetPredict[, cumU1 := 5.0]
    datasetPredict[, failed := TRUE]
  } else {
    #REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
    w_i = rep(1,nrow(datasetTrain))
    datasetTrain = cbind(datasetTrain,w_i)

    for(i in sort(1:reweights)){
      dispersion_parameter = summary(poisreg$fit)$dispersion
      if (i == 0) {
        break
      }
      try({
        anscombe.res = anscombe.residuals(poisreg$fit, dispersion_parameter)
        anscombe.res[anscombe.res < 1] = 1 #Alt. 2.58?
        datasetTrain[, w_i := anscombe.res ^ (-2)] #The weight
        Gamma = nrow(datasetTrain) / sum(datasetTrain$w_i)
        datasetTrain[, w_i := Gamma * w_i] #Makes sum(w_i) = n
        poisreg$fit = glm2::glm2(regformula, data = datasetTrain, weights = w_i, family = quasipoisson, na.action = na.omit)
        dispersion_parameter = summary(poisreg$fit)$dispersion
        od <- max(1,sum(poisreg$fit$weights * poisreg$fit$residuals^2)/poisreg$fit$df.r)
      },TRUE)
    }

    #CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
    pred = predict(poisreg$fit,type="response",se.fit=T,newdata=datasetPredict)
    datasetPredict[, threshold0 := pred$fit]
    datasetPredict[, threshold2 := FarringtonThreshold(pred, phi=dispersion_parameter, z=2, skewness.transform="2/3")]
    datasetPredict[, threshold4 := FarringtonThreshold(pred, phi=dispersion_parameter, z=4, skewness.transform="2/3")]
    datasetPredict[, threshold6 := FarringtonThreshold(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3")]
    datasetPredict[, zscore := FarringtonZscore(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3", y = n)]

    datasetPredict[, stderr := FarringtonSEinGammaSpace(pred, phi=dispersion_parameter, z=6, skewness.transform="2/3")]
    datasetPredict[, cumE1 := n^(2/3)-threshold0^(2/3)]
    datasetPredict[, cumL1 := (cumE1-2*stderr)^(3/2)]
    datasetPredict[, cumU1 := (cumE1+2*stderr)^(3/2)]
    datasetPredict[, cumE1 := cumE1^(3/2)]

    datasetPredict[, revcumE1 := threshold0^(2/3)-n^(2/3)]
    datasetPredict[, revcumL1 := (revcumE1-2*stderr)^(3/2)]
    datasetPredict[, revcumU1 := (revcumE1+2*stderr)^(3/2)]
    datasetPredict[, revcumE1 := revcumE1^(3/2)]

    datasetPredict[is.nan(cumE1), cumE1 := -revcumE1]
    datasetPredict[is.nan(cumL1), cumL1 := -revcumU1]
    datasetPredict[is.nan(cumU1), cumU1 := -revcumL1]
    datasetPredict[,revcumE1:=NULL]
    datasetPredict[,revcumL1:=NULL]
    datasetPredict[,revcumU1:=NULL]
    datasetPredict[,stderr:=NULL]
    datasetPredict[, failed := FALSE]
  }

  if(isDaily){
    return(datasetPredict[,c(variablesAlgorithmDaily,variablesAlgorithmBasic,variablesAlgorithmProduced),with=F])
  } else {
    # adding in some necessary variables
    datasetPredict <- AddXToWeekly(datasetPredict)
    datasetPredict <- AddWkyrAndDisplayDateToWeekly(datasetPredict)
    return(datasetPredict[,c(variablesAlgorithmWeekly,variablesAlgorithmBasic,variablesAlgorithmProduced),with=F])
  }
}

