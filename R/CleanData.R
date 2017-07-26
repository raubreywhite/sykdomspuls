#' test
#' @param clean a
#' @import fhi
#' @export LatestDatasets
LatestDatasets <- function(clean=list.files(fhi::DashboardFolder("data_clean"),"done_")){

  clean <- gsub(".txt","",gsub("done_","",clean))
  clean <- max(clean)

  return(list(
    "everyone_everyone"=paste0(clean,"_cleaned_everyone_everyone.RDS"),
    "everyone_fastlege"=paste0(clean,"_cleaned_everyone_fastlege.RDS"),
    "legekontakt_everyone"=paste0(clean,"_cleaned_legekontakt_everyone.RDS"),
    "legekontakt_fastlege"=paste0(clean,"_cleaned_legekontakt_fastlege.RDS")
  ))
}

#' test
#' @param raw a
#' @param clean a
#' @import data.table
#' @import fhi
#' @export IdentifyDatasets
IdentifyDatasets <- function(raw=list.files(fhi::DashboardFolder("data_raw"),"^partially_formatted_"),
                             clean=list.files(fhi::DashboardFolder("data_clean"),"done_")){
  raw <- data.table(raw)
  clean <- data.table(clean)

  raw[,id:=gsub(".txt","",gsub("partially_formatted_","",raw))]
  raw[,isRaw:=TRUE]
  clean[,id:=gsub(".txt","",gsub("done_","",clean))]
  clean[,isClean:=TRUE]
  res <- merge(raw,clean,by="id",all=TRUE)

  return(res)
}

#' test
#' @param L a
#' @param U a
#' @param saveFiles a
#' @import fhi
#' @import data.table
#' @importFrom lubridate today
#' @import httr
#' @import jsonlite
#' @export GetPopulation
GetPopulation <- function(
  L=c(0,5,15,20,30,65),
  U=c(4,14,19,29,64,9999),
  saveFiles=fhi::DashboardFolder("data_clean","pop.RDS")){
  municip <- c(
    "0",
    "01",
    "0101",
    "0102",
    "0103",
    "0104",
    "0105",
    "0106",
    "0111",
    "0113",
    "0114",
    "0115",
    "0116",
    "0117",
    "0118",
    "0119",
    "0121",
    "0122",
    "0123",
    "0124",
    "0125",
    "0127",
    "0128",
    "0130",
    "0131",
    "0133",
    "0134",
    "0135",
    "0136",
    "0137",
    "0138",
    "0199",
    "02",
    "0211",
    "0213",
    "0214",
    "0215",
    "0216",
    "0217",
    "0219",
    "0220",
    "0221",
    "0226",
    "0227",
    "0228",
    "0229",
    "0230",
    "0231",
    "0233",
    "0234",
    "0235",
    "0236",
    "0237",
    "0238",
    "0239",
    "0299",
    "03",
    "0301",
    "0399",
    "04",
    "0401",
    "0402",
    "0403",
    "0412",
    "0414",
    "0415",
    "0417",
    "0418",
    "0419",
    "0420",
    "0423",
    "0425",
    "0426",
    "0427",
    "0428",
    "0429",
    "0430",
    "0432",
    "0434",
    "0435",
    "0436",
    "0437",
    "0438",
    "0439",
    "0441",
    "0499",
    "05",
    "0501",
    "0502",
    "0511",
    "0512",
    "0513",
    "0514",
    "0515",
    "0516",
    "0517",
    "0518",
    "0519",
    "0520",
    "0521",
    "0522",
    "0528",
    "0529",
    "0532",
    "0533",
    "0534",
    "0536",
    "0538",
    "0540",
    "0541",
    "0542",
    "0543",
    "0544",
    "0545",
    "0599",
    "06",
    "0601",
    "0602",
    "0604",
    "0605",
    "0612",
    "0615",
    "0616",
    "0617",
    "0618",
    "0619",
    "0620",
    "0621",
    "0622",
    "0623",
    "0624",
    "0625",
    "0626",
    "0627",
    "0628",
    "0631",
    "0632",
    "0633",
    "0699",
    "07",
    "0701",
    "0702",
    "0703",
    "0704",
    "0705",
    "0706",
    "0707",
    "0708",
    "0709",
    "0711",
    "0713",
    "0714",
    "0716",
    "0717",
    "0718",
    "0719",
    "0720",
    "0721",
    "0722",
    "0723",
    "0724",
    "0725",
    "0726",
    "0727",
    "0728",
    "0799",
    "08",
    "0805",
    "0806",
    "0807",
    "0811",
    "0814",
    "0815",
    "0817",
    "0819",
    "0821",
    "0822",
    "0826",
    "0827",
    "0828",
    "0829",
    "0830",
    "0831",
    "0833",
    "0834",
    "0899",
    "09",
    "0901",
    "0903",
    "0904",
    "0906",
    "0911",
    "0912",
    "0914",
    "0918",
    "0919",
    "0920",
    "0921",
    "0922",
    "0923",
    "0924",
    "0926",
    "0928",
    "0929",
    "0932",
    "0933",
    "0935",
    "0937",
    "0938",
    "0940",
    "0941",
    "0999",
    "10",
    "1001",
    "1002",
    "1003",
    "1004",
    "1014",
    "1017",
    "1018",
    "1021",
    "1026",
    "1027",
    "1029",
    "1032",
    "1034",
    "1037",
    "1046",
    "1099",
    "11",
    "1101",
    "1102",
    "1103",
    "1106",
    "1111",
    "1112",
    "1114",
    "1119",
    "1120",
    "1121",
    "1122",
    "1124",
    "1127",
    "1129",
    "1130",
    "1133",
    "1134",
    "1135",
    "1141",
    "1142",
    "1144",
    "1145",
    "1146",
    "1149",
    "1151",
    "1154",
    "1159",
    "1160",
    "1199",
    "12",
    "1201",
    "1211",
    "1214",
    "1216",
    "1219",
    "1221",
    "1222",
    "1223",
    "1224",
    "1227",
    "1228",
    "1230",
    "1231",
    "1232",
    "1233",
    "1234",
    "1235",
    "1238",
    "1241",
    "1242",
    "1243",
    "1244",
    "1245",
    "1246",
    "1247",
    "1248",
    "1249",
    "1250",
    "1251",
    "1252",
    "1253",
    "1255",
    "1256",
    "1259",
    "1260",
    "1263",
    "1264",
    "1265",
    "1266",
    "1299",
    "13",
    "1301",
    "14",
    "1401",
    "1411",
    "1412",
    "1413",
    "1416",
    "1417",
    "1418",
    "1419",
    "1420",
    "1421",
    "1422",
    "1424",
    "1426",
    "1428",
    "1429",
    "1430",
    "1431",
    "1432",
    "1433",
    "1438",
    "1439",
    "1441",
    "1443",
    "1444",
    "1445",
    "1448",
    "1449",
    "1499",
    "15",
    "1501",
    "1502",
    "1503",
    "1504",
    "1505",
    "1511",
    "1514",
    "1515",
    "1516",
    "1517",
    "1519",
    "1520",
    "1523",
    "1524",
    "1525",
    "1526",
    "1527",
    "1528",
    "1529",
    "1531",
    "1532",
    "1534",
    "1535",
    "1539",
    "1543",
    "1545",
    "1546",
    "1547",
    "1548",
    "1551",
    "1554",
    "1556",
    "1557",
    "1560",
    "1563",
    "1566",
    "1567",
    "1569",
    "1571",
    "1572",
    "1573",
    "1576",
    "1599",
    "16",
    "1601",
    "1612",
    "1613",
    "1617",
    "1620",
    "1621",
    "1622",
    "1624",
    "1627",
    "1630",
    "1632",
    "1633",
    "1634",
    "1635",
    "1636",
    "1638",
    "1640",
    "1644",
    "1645",
    "1648",
    "1653",
    "1657",
    "1662",
    "1663",
    "1664",
    "1665",
    "1699",
    "17",
    "1702",
    "1703",
    "1711",
    "1714",
    "1717",
    "1718",
    "1719",
    "1721",
    "1723",
    "1724",
    "1725",
    "1729",
    "1736",
    "1738",
    "1739",
    "1740",
    "1742",
    "1743",
    "1744",
    "1748",
    "1749",
    "1750",
    "1751",
    "1755",
    "1756",
    "1799",
    "18",
    "1804",
    "1805",
    "1811",
    "1812",
    "1813",
    "1814",
    "1815",
    "1816",
    "1818",
    "1820",
    "1822",
    "1824",
    "1825",
    "1826",
    "1827",
    "1828",
    "1832",
    "1833",
    "1834",
    "1835",
    "1836",
    "1837",
    "1838",
    "1839",
    "1840",
    "1841",
    "1842",
    "1843",
    "1845",
    "1848",
    "1849",
    "1850",
    "1851",
    "1852",
    "1853",
    "1854",
    "1855",
    "1856",
    "1857",
    "1858",
    "1859",
    "1860",
    "1865",
    "1866",
    "1867",
    "1868",
    "1870",
    "1871",
    "1874",
    "1899",
    "19",
    "1901",
    "1902",
    "1903",
    "1911",
    "1913",
    "1915",
    "1917",
    "1919",
    "1920",
    "1921",
    "1922",
    "1923",
    "1924",
    "1925",
    "1926",
    "1927",
    "1928",
    "1929",
    "1931",
    "1933",
    "1936",
    "1938",
    "1939",
    "1940",
    "1941",
    "1942",
    "1943",
    "1999",
    "20",
    "2001",
    "2002",
    "2003",
    "2004",
    "2011",
    "2012",
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020",
    "2021",
    "2022",
    "2023",
    "2024",
    "2025",
    "2027",
    "2028",
    "2030",
    "2099",
    "21",
    "2111",
    "2112",
    "2113",
    "2114",
    "2115",
    "2121",
    "2131",
    "2199",
    "22",
    "2211",
    "2299",
    "23",
    "2300",
    "2311",
    "2321",
    "2399",
    "25",
    "26",
    "88",
    "99",
    "9999"
  )
  municip <- municip[nchar(municip)==4 & municip!="9999"]

  ages <- c(
    "000",
    "001",
    "002",
    "003",
    "004",
    "005",
    "006",
    "007",
    "008",
    "009",
    "010",
    "011",
    "012",
    "013",
    "014",
    "015",
    "016",
    "017",
    "018",
    "019",
    "020",
    "021",
    "022",
    "023",
    "024",
    "025",
    "026",
    "027",
    "028",
    "029",
    "030",
    "031",
    "032",
    "033",
    "034",
    "035",
    "036",
    "037",
    "038",
    "039",
    "040",
    "041",
    "042",
    "043",
    "044",
    "045",
    "046",
    "047",
    "048",
    "049",
    "050",
    "051",
    "052",
    "053",
    "054",
    "055",
    "056",
    "057",
    "058",
    "059",
    "060",
    "061",
    "062",
    "063",
    "064",
    "065",
    "066",
    "067",
    "068",
    "069",
    "070",
    "071",
    "072",
    "073",
    "074",
    "075",
    "076",
    "077",
    "078",
    "079",
    "080",
    "081",
    "082",
    "083",
    "084",
    "085",
    "086",
    "087",
    "088",
    "089",
    "090",
    "091",
    "092",
    "093",
    "094",
    "095",
    "096",
    "097",
    "098",
    "099",
    "100",
    "101",
    "102",
    "103",
    "104",
    "105+"
  )

  lastYear <- data.table::year(lubridate::today())
  if(data.table::month(lubridate::today())<3){
    lastYear <- lastYear-1
  }
  years <- as.character(c((data.table::year(lubridate::today())-15):lastYear))

  retval <- vector("list",length(years)+2)
  for(i in 1:length(years)){
    useYear <- years[i]

    b <- paste0('
  {
    "query": [
      {
        "code": "Region",
        "selection": {
          "filter": "item",
          "values": [
  ',paste0('"',municip,'"',collapse=","),'
          ]
        }
      },
      {
        "code": "Kjonn",
        "selection": {
          "filter": "item",
          "values": [
            "1",
            "2"
          ]
        }
      },
      {
        "code": "Alder",
        "selection": {
          "filter": "item",
          "values": [
  ',paste0('"',ages,'"',collapse=","),'
          ]
        }
      },
      {
        "code": "ContentsCode",
        "selection": {
          "filter": "item",
          "values": [
            "Personer1"
          ]
        }
      },
      {
        "code": "Tid",
        "selection": {
          "filter": "item",
          "values": [
  ',paste0('"',useYear,'"',collapse=","),'
          ]
        }
      }
    ],
    "response": {
      "format": "json-stat"
    }
  }

    ')

    x <- httr::POST("http://data.ssb.no/api/v0/no/table/07459",
                    body=b,encode="json")

    y <- jsonlite::fromJSON(httr::content(x,"text"))
    d1 <- municip #unlist(y$dataset$dimension$Region$category$label)
    d2 <- unlist(y$dataset$dimension$Kjonn$category$label)
    d3 <- unlist(y$dataset$dimension$Alder$category$label)
    d4 <- unlist(y$dataset$dimension$Tid$category$label)
    res <- expand.grid(d4,d3,d2,d1)
    res$value <- as.character(y$dataset$value)
    res <- data.table(res)
    res[,value:=as.numeric(value)]
    res[,age:=as.numeric(stringr::str_extract(Var2,"^[0-9]*"))]
    res <- res[,c("Var1","Var4","value","age"),with=F]
    setnames(res,c("year","municip","pop","agecont"))
    res[,age:=paste0(L[1],"-",U[1])]
    for(j in 2:(length(L)-1)){
      res[agecont>=L[j] & agecont<=U[j],age:=paste0(L[j],"-",U[j])]
    }
    res[agecont>=L[length(L)],age:=paste0(L[length(L)],"+")]

    res[,municip:=paste0("municip",municip)]
    res <- res[,.(pop=sum(pop)),by=.(year,municip,age)]
    x <- res[,.(pop=sum(pop)),by=.(year,municip)]
    x[,age:="Totalt"]
    res <- rbind(res,x)
    res[,year:=as.character(year)]

    retval[[i]] <- res
  }
  SaveData(retval,saveFiles)
}

#' test
#' @param d a
#' @param population a
#' @param hellidager a
#' @param testIfHelligdagIndikatorFileIsOutdated a
#' @import data.table
#' @importFrom lubridate today
#' @export FormatData
FormatData <- function(d,
                       population=readRDS(fhi::DashboardFolder("data_clean","pop.RDS")),
                       hellidager=fread(system.file("extdata", "DatoerMedHelligdager.txt", package = "sykdomspuls"))[,c("Dato","HelligdagIndikator"),with=FALSE],
                       testIfHelligdagIndikatorFileIsOutdated=TRUE){
  d <- d[,.(
    influensa=sum(influensa),
    gastro=sum(gastro),
    respiratory=sum(respiratory),
    consult=sum(consult)
  ),by=.(
    age,date,municip
  )]
  skeleton <- data.table(expand.grid(unique(d$municip),unique(d$age),unique(d$date)))
  setnames(skeleton, c("municip","age","date"))
  data <- merge(skeleton,d,by=c("municip","age","date"),all.x=TRUE)
  data[is.na(influensa),influensa:=0]
  data[is.na(gastro),gastro:=0]
  data[is.na(respiratory),respiratory:=0]
  data[is.na(consult),consult:=0]

  total <- data[,.(influensa=sum(influensa),gastro=sum(gastro),respiratory=sum(respiratory),consult=sum(consult)),
                by=.(municip,date)]
  total[,age:="Totalt"]
  data <- rbind(total,data[age!="Ukjent"])

  dates <- unique(data[,"date",with=F])
  dates[,datex:=date]
  dates[,yrwk := format.Date(datex,"%G-%V")] #Week-based year, instead of normal year (%Y)
  dates[,year:=as.numeric(format.Date(date,"%G"))]

  # delete last day of data if it is not a sunday
  if(format.Date(max(dates$datex),"%u")!=7){
    dates <- dates[yrwk!=max(yrwk)]
  }
  dates[,datex:=NULL]
  dates[,yrwk:=NULL]
  data <- merge(data,dates,by="date")

  # POPULATION

  lastYr <- as.numeric(population[[length(population)-2]]$year[1])

  population[[length(population)-1]] <- copy(population[[length(population)-2]])
  population[[length(population)]] <- copy(population[[length(population)-2]])

  population[[length(population)-1]][,year:=as.character(lastYr+1)]
  population[[length(population)]][,year:=as.character(lastYr+2)]

  population <- rbindlist(population)
  population[,year:=as.numeric(year)]

  data <- merge(data,population,by=c("municip","year","age"),all.x=TRUE)

  # merging in municipalitiy-fylke names
  data("countyToMunicip",package="fhi")
  data <- merge(data,countyToMunicip[,c("municip","county"),with=FALSE],by="municip")
  data[,influensa:=as.numeric(influensa)]
  data[,gastro:=as.numeric(gastro)]
  data[,respiratory:=as.numeric(respiratory)]
  data[,consultWithInfluensa:=as.numeric(consult)]
  data[,consultWithoutInfluensa:=consultWithInfluensa-influensa]
  data[,consult:=NULL]
  data[,pop:=as.numeric(pop)]

  data <- data[date>=data.table::as.IDate("2006-01-01")]
  data[,municip:=as.character(municip)]


  setnames(hellidager,c("date","HelligdagIndikator"))
  hellidager[,date:=data.table::as.IDate(date)]
  if(testIfHelligdagIndikatorFileIsOutdated & lubridate::today()>max(hellidager$date)){
    error("HELLIGDAGER NEEDS UPDATING")
  }
  dim(data)
  data <- merge(data,hellidager,by="date")
  dim(data)

  data[,year:=NULL]

  setcolorder(data,c("date",
                     "HelligdagIndikator",
                     "county",
                     "municip",
                     "age",
                     "influensa",
                     "gastro",
                     "respiratory",
                     "consultWithInfluensa",
                     "consultWithoutInfluensa",
                     "pop"))

  return(data)
}

#' test
#' @export LatestRawID
LatestRawID <- function(){
  f <- IdentifyDatasets()
  return(max(f$id))
}

#' test
#' @param file a
#' @import fhi
#' @export DeleteLatestDoneFile
DeleteLatestDoneFile <- function(file=fhi::DashboardFolder("data_clean",paste0("done_",LatestRawID(),".txt"))){
  try(unlink(file),TRUE)
  #try(unlink(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
}

#' test
#' @param file a
#' @import fhi
#' @export CreateLatestDoneFile
CreateLatestDoneFile <- function(file=fhi::DashboardFolder("data_clean",paste0("done_",LatestRawID(),".txt"))){
  try(file.create(file),TRUE)
  #try(file.create(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
}

#' test
#' @export InitialiseStatus
InitialiseStatus <- function(){
  f <- IdentifyDatasets()
  dates <- list()
  dates$program <- "Sykdomspuls"
  dates$clean <- as.Date(f[clean==max(clean,na.rm=T)]$id,format="%Y_%m_%d")
  dates$raw <- as.Date(f[raw==max(raw,na.rm=T)]$id,format="%Y_%m_%d")
  dates$run <- as.Date(Sys.time())
  return(dates)
}

#' test
#' @param ageStrings a
#' @import stringr
#' @export GetAgesLU
GetAgesLU <- function(ageStrings){
  ageStrings <- ageStrings[ageStrings!="Ukjent"]
  ageStrings <- gsub("\\+","",ageStrings)
  ageStrings <- stringr::str_split(ageStrings,"-")
  L <- c()
  for(i in 1:length(ageStrings)) L[i] <- as.numeric(ageStrings[[i]][1])
  L <- as.numeric(L)
  L <- sort(L)
  U <- L[2:length(L)]-1
  U <- c(U,99999)
  return(list(
    L=L,
    U=U
  ))
}

#' test
#' @import data.table
#' @import fhi
#' @importFrom RAWmisc IsFileStable
#' @export UpdateData
UpdateData <- function(){
  files <- IdentifyDatasets()
  print(files)
  files <- files[is.na(isClean)]
  if(nrow(files)==0){
    print("R/SYKDOMSPULS No new data")
    print("R/SYKDOMSPULS Finished without new data")
    return(FALSE)
  } else {
    print("R/SYKDOMSPULS Updating data")
    EmailNotificationOfNewData(files$id)
    for(i in 1:nrow(files)){
      if(!RAWmisc::IsFileStable(fhi::DashboardFolder("data_raw",files[i]$raw))){
        print(paste0("R/SYKDOMSPULS Unstable file ",files[i]$raw))
        next
      }
      print(paste0("R/SYKDOMSPULS Cleaning file ",files[i]$id))
      print(sprintf("R/SYKDOMSPULS sykdomspuls RUN Cleaning file %s",files[i]$id))
      d <- fread(fhi::DashboardFolder("data_raw",files[i]$raw))
      d[,date:=data.table::as.IDate(date)]

      LU <- GetAgesLU(ageStrings=unique(d$age))
      GetPopulation(L=LU$L,U=LU$U)

      print("R/SYKDOMSPULS legekontakt_everyone")
      res <- FormatData(d[Kontaktype=="Legekontakt"])
      saveRDS(res,file=fhi::DashboardFolder("data_clean",paste0(files[i]$id,"_cleaned_legekontakt_everyone.RDS")))

      #print("legekontakt_fastlege")
      #res <- FormatData(d[Kontaktype=="Legekontakt" & Praksis=="Fastlege"])
      #saveRDS(res,file=fhi::DashboardFolder("data_clean",paste0(files[i]$id,"_cleaned_legekontakt_fastlege.RDS")))

      #print("everyone_fastlege")
      #res <- FormatData(d[Praksis=="Fastlege"])
      #saveRDS(res,file=fhi::DashboardFolder("data_clean",paste0(files[i]$id,"_cleaned_everyone_fastlege.RDS")))

      print("R/SYKDOMSPULS everyone_everyone")
      res <- FormatData(d)
      saveRDS(res,file=fhi::DashboardFolder("data_clean",paste0(files[i]$id,"_cleaned_everyone_everyone.RDS")))

      file.create(fhi::DashboardFolder("data_clean",paste0("done_",files[i]$id,".txt")))
    }
    print("R/SYKDOMSPULS New data now exists")
    return(TRUE)
  }
}

