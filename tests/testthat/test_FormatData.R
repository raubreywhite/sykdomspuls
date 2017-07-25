context("FormatData")


test_that("Basic Oslo", {
  library(data.table)
  d <- vector("list",7)
  for(i in 0:6){
    d[[i+1]] <- data.table(
      age=c("0-4","5-14","15-19","20-29","30-64","65+","Ukjent"),
      date=as.character(as.Date("2006-01-23")+i),
      Kontaktype=c("Legekontakt"),
      Praksis=c("Fastlege"),
      influensa=c(100),
      gastro=c(100),
      respiratory=c(100),
      consult=c(500),
      municip=c("municip0301")
    )
  }
  d <- rbindlist(d)


  population <- vector("list",3)
  population[[1]] <- data.table(
    year="2006",
    municip="municip0301",
    age=c("0-4","5-14","15-19","20-29","30-64","65+","Totalt"),
    pop=c(100,100,100,100,100,100,600)
  )

  hellidager=data.table(
    Dato=as.character(seq(as.Date("2006-01-23"),as.Date("2006-01-29"),by=1)),
    HelligdagIndikator=0
  )

  res <- FormatData(d,
                    population=population,
                    hellidager=hellidager,
                    testIfHelligdagIndikatorFileIsOutdated=FALSE)

  expectedRes <- data.table(expand.grid(
      date=as.character(seq(as.Date("2006-01-23"),as.Date("2006-01-29"),by=1)),
      age=c("0-4","5-14","15-19","20-29","30-64","65+","Totalt"),
      stringsAsFactors = FALSE
    ))
  expectedRes[,municip:="municip0301"]
  expectedRes[,influensa:=100]
  expectedRes[age=="Totalt",influensa:=700]
  expectedRes[,gastro:=100]
  expectedRes[age=="Totalt",gastro:=700]
  expectedRes[,respiratory:=100]
  expectedRes[age=="Totalt",respiratory:=700]
  expectedRes[,consult:=500]
  expectedRes[age=="Totalt",consult:=3500]
  expectedRes[,pop:=100]
  expectedRes[age=="Totalt",pop:=600]
  expectedRes[,county:="county03"]
  expectedRes[,HelligdagIndikator:=0]
  setcolorder(expectedRes,c("date","municip","age","influensa","gastro","respiratory","consult","pop","county","HelligdagIndikator"))
  setorder(expectedRes,date,age)
  setkey(expectedRes,date)

  expect_equal(res,expectedRes)
})

