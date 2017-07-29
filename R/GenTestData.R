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
}
