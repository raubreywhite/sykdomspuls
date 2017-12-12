GenTestData <- function(){
  set.seed(4)
  d <- fread("/analyses/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip0301","municip1503","municip1505")]

  for(i in CONFIG$SYNDROMES){
    d[get(i)<10,(i):=0]
    d[,(i):=get(i)+rpois(.N,500)]
  }
  d[,consult:=influensa+gastro+respiratory+100+rpois(.N,500)]

  saveRDS(d,file="/git/dashboards_sykdomspuls/tests/testthat/data/partially_formatted_2017_05_09.RDS")

  set.seed(4)
  d <- fread("/analyses/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip0706","municip0719","municip0720","municip0710")]

  for(i in CONFIG$SYNDROMES){
    d[get(i)<10,(i):=0]
    d[,(i):=get(i)+rpois(.N,500)]
  }
  d[,consult:=influensa+gastro+respiratory+100+rpois(.N,500)]

  saveRDS(d,file="/git/dashboards_sykdomspuls/tests/testthat/data/partially_formatted_sandefjord.RDS")

  set.seed(4)
  d <- fread("/analyses/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip0301")]
  population <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat","data","pop.RDS"))
  for(i in 1:length(population)){
    if(!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager=readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat","data","hellidager.RDS"))

  res <- sykdomspuls::FormatData(d,
                    population=population,
                    hellidager=hellidager,
                    testIfHelligdagIndikatorFileIsOutdated=FALSE)

  res <- res[age=="Totalt", .(influensa=sum(influensa),
                              gastro=sum(gastro),
                              respiratory=sum(respiratory),
                              respiratoryinternal=sum(respiratoryinternal),
                              respiratoryexternal=sum(respiratoryexternal),
                              lungebetennelse=sum(lungebetennelse),
                              bronkitt=sum(bronkitt),
                              consult=sum(consultWithInfluensa),
                              HelligdagIndikator=mean(HelligdagIndikator)),
         by=.(date,municip)]

  for(i in CONFIG$SYNDROMES){
    if(i=='influensa'){
      next
    }
    d[get(i)<10,(i):=0]
    res[,(i):=get(i)+rpois(.N,50)]
  }
  res[,consult:=influensa+gastro+respiratoryinternal+100+rpois(.N,50)]

  saveRDS(res,file="/git/dashboards_sykdomspuls/tests/testthat/data/formatted_oslo.RDS")

  set.seed(4)
  d <- fread("/analyses/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip1711")]
  population <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat","data","pop.RDS"))
  for(i in 1:length(population)){
    if(!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager=readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat","data","hellidager.RDS"))

  res <- sykdomspuls::FormatData(d,
                                 population=population,
                                 hellidager=hellidager,
                                 testIfHelligdagIndikatorFileIsOutdated=FALSE)
  res <- res[age=="Totalt"]
  res[,influensa:=0]
  res[,gastro:=gastro+sample(c(-5:5),.N,replace=T)]
  res[gastro<0,gastro:=0]

  for(i in CONFIG$SYNDROMES){
    if(i=='gastro'){
      next
    }
    d[,(i):=0]
  }

  res[,consultWithInfluensa:=0]
  res[,consultWithoutInfluensa:=consultWithoutInfluensa+sample(c(-5:5),.N,replace=T)]

  saveRDS(res,file="/git/dashboards_sykdomspuls/tests/testthat/data/formatted_meraker.RDS")

}
