GenTestData <- function(){
  set.seed(4)
  d <- fread("/data_raw/sykdomspuls/partially_formatted_2017_05_09.txt")
  d <- d[municip %in% c("municip0301","municip1503","municip1505")]
  d[influensa<10,influensa:=0]
  d[gastro<10,gastro:=0]
  d[respiratory<10,respiratory:=0]

  d[,influensa:=influensa+rpois(.N,500)]
  d[,gastro:=gastro+rpois(.N,500)]
  d[,respiratory:=respiratory+rpois(.N,500)]
  d[,consult:=influensa+gastro+respiratory+100+rpois(.N,500)]

  saveRDS(d,file="/packages/dashboards_sykdomspuls/tests/testthat/partially_formatted_2017_05_09.RDS")

  set.seed(4)
  d <- fread("/data_raw/sykdomspuls/partially_formatted_2017_05_09.txt")
  d <- d[municip %in% c("municip0706","municip0719","municip0720","municip0710")]
  d[influensa<10,influensa:=0]
  d[gastro<10,gastro:=0]
  d[respiratory<10,respiratory:=0]

  d[,influensa:=influensa+rpois(.N,500)]
  d[,gastro:=gastro+rpois(.N,500)]
  d[,respiratory:=respiratory+rpois(.N,500)]
  d[,consult:=influensa+gastro+respiratory+100+rpois(.N,500)]

  saveRDS(d,file="/packages/dashboards_sykdomspuls/tests/testthat/partially_formatted_sandefjord.RDS")

  set.seed(4)
  d <- fread("/data_raw/sykdomspuls/partially_formatted_2017_05_09.txt")
  d <- d[municip %in% c("municip0301")]
  population <- readRDS(file.path("/packages/dashboards_sykdomspuls/tests/testthat","data","pop.RDS"))
  for(i in 1:length(population)){
    if(!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager=readRDS(file.path("/packages/dashboards_sykdomspuls/tests/testthat","data","hellidager.RDS"))

  res <- sykdomspuls::FormatData(d,
                    population=population,
                    hellidager=hellidager,
                    testIfHelligdagIndikatorFileIsOutdated=FALSE)

  res <- res[age=="Totalt", .(influensa=sum(influensa),gastro=sum(gastro),respiratory=sum(respiratory),consult=sum(consultWithInfluensa),HelligdagIndikator=mean(HelligdagIndikator)),
         by=.(date,municip)]
  res[,influensa:=influensa+rpois(.N,5)]
  res[,gastro:=gastro+rpois(.N,50)]
  res[,respiratory:=respiratory+rpois(.N,50)]
  res[,consult:=influensa+gastro+respiratory+100+rpois(.N,50)]

  res[influensa<10,influensa:=0]
  res[gastro<10,gastro:=0]
  res[respiratory<10,respiratory:=0]

  saveRDS(res,file="/packages/dashboards_sykdomspuls/tests/testthat/data/formatted_oslo.RDS")

}
