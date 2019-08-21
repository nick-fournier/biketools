#load packages
library(data.table)
library(optimx)
library(ggplot2)

#load in trip data
load("./data/tripdata.RData")
load("./data/stationdata.RData")

#remove person attributes we don't need
trips <- trips[ , !c("usertype","gender","bikeid","trip_duration","start_station_docks","end_station_docks")]

#extract day, month, year
trips[ , start_date := as.Date(start_date)]
trips[ , end_date := as.Date(end_date)]

#### Count station totals ####
ADB.station <- merge(trips[ , .N, by = .(start_date,start_station_id)],
                     trips[ , .N, by = .(end_date,end_station_id)],
                     by.x = c("start_date","start_station_id"),
                     by.y = c("end_date","end_station_id"),
                     suffixes = c(".start",".end"), all = T)
#set NA to 0
ADB.station[is.na(N.start), N.start := 0]
ADB.station[is.na(N.end), N.end := 0]
#total trips at each station
ADB.station[ , N.total := N.start + N.end]
ADB.station[ , N.end := NULL]
ADB.station[ , N.start := NULL]
#rename date and station columns
setnames(ADB.station, c("start_date","start_station_id"),c("date","station_id"))
#format year-month, year, month
#ADB[ , year_month := format(start_date, "%Y-%m")]
ADB.station[ , year := as.numeric(format(date, "%Y"))]
ADB.station[ , month := as.numeric(format(date, "%m"))]
#average daily trips per station per month per year (ADB_smy)
ADB.station <- ADB.station[ , mean(N.total), by = .(year,month,station_id)]
setnames(ADB.station, "V1","ADB.total")
#Minimum of 5 months of data
ADB.station <- merge(ADB.station, ADB.station[ , .N, by = .(station_id,year)][N>=5,], by = c("station_id","year"))

#### pairwise ####
ADB.pairs <- trips[ , .N , by = c(colnames(trips)[-which(colnames(trips)=="end_date")])]
setnames(ADB.pairs, "start_date","date")
ADB.pairs[ , year := as.numeric(format(date, "%Y"))]
ADB.pairs[ , month := as.numeric(format(date, "%m"))]
ADB.pairs <- ADB.pairs[ , sum(N), by = .(year,month,start_station_id,end_station_id)]
setnames(ADB.pairs, "V1","ADB")
ADB.pairs <- merge(ADB.pairs,
                   ADB.pairs[ , .N, by = .(start_station_id,end_station_id,year)][N>=5,],
                   by = c("start_station_id","end_station_id","year"))

#### Estimating alpha, AADB, and phi ####
sinefit <- function(adb,mos) {
  #objective function
  func <- function(x) sum((adb - x[2]*(x[1]*sin((pi/6)*(mos-x[3]+3)) + 1))^2)
  #optimization
  fit <- suppressWarnings(optimx(par=c(0.5,mean(adb),6), fn=func,method="L-BFGS-B", lower=c(0.1,1,1), upper=c(1,max(adb),12)))
  #formatting
  fit <- as.numeric(fit)[1:4]
  names(fit) <- c("alpha","AADB","phi","sumsqerr")
  return(as.list(fit))
}

#Computing for each station and year... May take a while
print("Estimating parameters for each station...")
fit <- ADB.station[ , sinefit(ADB.total,month), by = .(station_id,year)]
print("done")
#Merge fitted values back to ADB table
ADB.station <- merge(ADB.station,fit, by = c("station_id","year"))

#average across years per station
ADB.station[ , alpha.average := mean(alpha) , by = station_id]

#Add continuous date
ADB.station[ , date := as.Date(paste(year,month,"01",sep="/"),"%Y/%m/%d")]

#Add stations
ADB.station <- merge(ADB.station, stations[,.(station_id,municipality,latitude,longitude)], by="station_id")

#save
save(ADB.station, file = "./data/ADBdata.RData")

rm(list=ls())

