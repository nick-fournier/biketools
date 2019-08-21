#Formats the hubway data, aggregates, and combines the data for sinusoidal model
library(data.table)
library(pbapply)
library(fasttime)
library(geosphere)

#Getting zip file list
files <- list.files('./data/zips', full.names = T) 

#Extracting & importing data ####
print("Importing trips from 2011-2014")
#legacy format trips (2011 - 2014)
trips.11_14 <- pblapply(files[grepl("Trips",files)], function(x) {
  fread(x, sep=",", na.strings = "", colClasses = "character")
})

print("Importing trips from 2015---")
#Newer format trips (2015 - 2017)
trips.15 <- pblapply(files[grepl("tripdata",files)], function(x) {
  #unzip and load data
  trip = fread( unzip(x, exdir = "./data"), sep=",", na.strings = "", colClasses = "character")
  #delete unzipped file
  file.remove(paste(gsub("zips/|\\.zip","",x),".csv",sep = ""))
  return(trip)
})

#Formatting stations data ####
print("Importing station data")
stations <- lapply(files[grepl("Stations",files)], function(x) fread(x, sep=",", na.strings = "", colClasses = "character"))
#format column names
stations <- lapply(stations, function(x) {
  setnames(x,c("Station ID","Station","# of Docks"),c("station_id","station_name","docks"))
  colnames(x) <- tolower(colnames(x))
  x <- x[ , .(station_id,station_name,municipality,docks,latitude,longitude)]
})
#merge into single station table
stations <- rbindlist(stations)
#remove duplicates
stations <- stations[!duplicated(stations[,station_id]),]
#set station column data classes
cols <- c("latitude","longitude","docks")
stations[, (cols) := lapply(.SD, as.numeric), .SDcol = cols]
rm(cols)

#Formatting trip column headers ####
print("Formatting trip data headers")
#merging trips into two tables
trips.15 <- rbindlist(trips.15)
trips.11_14 <- rbindlist(trips.11_14)

#Formatting columns in 2011-2014 trips table
colnames(trips.11_14) <- tolower(colnames(trips.11_14))
colnames(trips.11_14) <- gsub(" ", "_", colnames(trips.11_14))
setnames(trips.11_14, c("duration","start_station_number","end_station_number","bike_number","member_type"),
                      c("trip_duration","start_station_id","end_station_id","bikeid","usertype"))
#formatting columns in 2015-2017 trips table
colnames(trips.15) <- gsub(" ", "_", colnames(trips.15))
setnames(trips.15, c("tripduration","starttime","stoptime"),c("trip_duration","start_date","end_date"))

#removing incompatible columns from both
trips.11_14 <- trips.11_14[ , !c("start_station_name","end_station_name","zip_code")]
trips.15 <- trips.15[ , !"birth_year"]

#Formatting trip station data ####
#for 2011-2014
print("Formatting 2011-2014 trip station data")
#adding start station data to 2011-2014 data
trips.11_14 <- merge(trips.11_14,
                     stations,
                     by.x = "start_station_id", by.y = "station_id")
setnames(trips.11_14,c("station_name","latitude","longitude","docks","municipality"),
         c("start_station_name","start_station_latitude","start_station_longitude",
           "start_station_docks","start_station_municipality"))
#adding end station data to 2011-2014 data
trips.11_14 <- merge(trips.11_14,
                     stations,
                     by.x = "end_station_id", by.y = "station_id")
setnames(trips.11_14,c("station_name","latitude","longitude","docks","municipality"),
         c("end_station_name","end_station_latitude","end_station_longitude",
           "end_station_docks","end_station_municipality"))

#for 2011-2014
print("Formatting 2015- -- trip station data")

#Fixing weird naming conventions
trips.15[ , start_station_name := gsub(" TEMPORARY WINTER LOCATION","",start_station_name)]
trips.15[ , end_station_name := gsub(" TEMPORARY WINTER LOCATION","",end_station_name)]

#manually fixing mismatch stations
missingstarts <- merge(trips.15, stations[!duplicated(stations$station_name),], by.x = "start_station_name", by.y = "station_name", all = T)
missingstarts <- missingstarts[is.na(station_id), .N, by = .(start_station_name,start_station_id,start_station_latitude,start_station_longitude)]
missingstarts <- missingstarts[start_station_latitude!="0.0",]
missingends <- merge(trips.15, stations[!duplicated(stations$station_name),], by.x = "end_station_name", by.y = "station_name", all = T)
missingends <- missingends[is.na(station_id), .N, by = .(end_station_name,end_station_id,end_station_latitude,end_station_longitude)]
missingends <- missingends[end_station_latitude!="0.0",]
#making dummy columns
colnames(missingstarts) <- gsub("start","trip",colnames(missingstarts))
colnames(missingends) <- gsub("end","trip",colnames(missingends))
#cleaning up
missing <- rbind(missingstarts,missingends)
missing <- missing[!duplicated(trip_station_name) & trip_station_name!="\\N",]
rm(missingends, missingstarts)

#checking if already exist by location
for(i in 1:nrow(missing)){
  #set up lat/long comparison
  tmp <- data.table(missing[i,.(trip_station_latitude,trip_station_longitude)], 
                    stations[ , .(latitude,longitude,station_id,station_name)])
  #as numeric
  tmp <- tmp[ , lapply(.SD, as.numeric), by=.(station_id,station_name)]
  #calculate distance in meters
  tmp[ , dist := t(distm(tmp[1,.(trip_station_longitude, trip_station_latitude)], cbind(longitude, latitude), fun = distHaversine))]
  print(paste(tmp[which.min(dist),dist], tmp[which.min(dist),station_name], missing[i,trip_station_name],sep="|"))
  #finding correct name for matching within 200m
  if(tmp[which.min(dist), dist]<200) {
    correctname <- tmp[which.min(dist), station_name]
  } else { 
    correctname <- NA
  }
  #assigning back to data
  missing[i, trip_station_name2 := correctname]
}

#Correcting the weird names
for(i in 1:nrow(missing[!is.na(trip_station_name2), ])){
  cat(paste(i," ",sep=""))
  old = missing[!is.na(trip_station_name2), ][i,trip_station_name]
  new = missing[!is.na(trip_station_name2), ][i,trip_station_name2]
  trips.15[start_station_name == old, start_station_name := new]
  trips.15[end_station_name == old, end_station_name := new]
}
#Remaining missing stations are added to stations list, probably new stations that came after data upload
missing <- missing[is.na(trip_station_name2), .(trip_station_id,trip_station_name,trip_station_latitude,trip_station_longitude) ]
setnames(missing,c("trip_station_id","trip_station_name","trip_station_latitude","trip_station_longitude"), c("station_id","station_name","latitude","longitude"))
missing[ , station_id := paste("X",station_id,sep="")]

missingmuni <- c("X1"="Boston","X153"="Montreal","X82"="Brookline","X219"="Boston","X42"="Boston",
  "X193"="Brookline","X112"="Somerville","X232"="Boston","X181"="Cambridge","X228"="Cambridge",
  "X157"="Boston","X132"="Somerville","X222"="Boston")
missingmuni <- data.table(station_id = names(missingmuni), municipality = missingmuni, docks=NA)
missing <- merge(missing, missingmuni, by = "station_id")
missing <- missing[ , colnames(stations), with=F]
missing <- missing[municipality!="Montreal",]
#final station list
stations <- rbind(stations, missing)

#merge start station data to 2015-2017 trips
trips.15 <- merge(trips.15, stations[!duplicated(stations$station_name),],
                     by.x = "start_station_name", by.y = "station_name")
#Removing old ID columns, replacing with new ones
trips.15 <- trips.15[ , !c("start_station_id","start_station_latitude","start_station_longitude")]
setnames(trips.15,c("station_id","latitude","longitude","docks","municipality"),
         c("start_station_id","start_station_latitude","start_station_longitude",
           "start_station_docks","start_station_municipality"))

#merge end station data to 2015-2017 trips
trips.15 <- merge(trips.15, stations[!duplicated(stations$station_name),],
                     by.x = "end_station_name", by.y = "station_name")
#Removing old ID columns, replacing with new ones
trips.15 <- trips.15[ , !c("end_station_id","end_station_latitude","end_station_longitude")]
setnames(trips.15,c("station_id","latitude","longitude","docks","municipality"),
         c("end_station_id","end_station_latitude","end_station_longitude",
           "end_station_docks","end_station_municipality"))

#Formatting trip data classes ####
print("Formatting dates")
#fix date format in 2011-2014 to be in yyyy-mm-dd H:M:S format"
trips.11_14[ , start_date := paste(as.Date(start_date,format='%m/%d/%Y %H:%M'),
                                   substr(start_date, 1+as.integer(gregexpr(' ', start_date)), nchar(start_date)))]
trips.11_14[ , end_date := paste(as.Date(end_date,format='%m/%d/%Y %H:%M'),
                                 substr(end_date, 1+as.integer(gregexpr(' ', end_date)), nchar(end_date)))]

#merge into single data table
print("Combining trips into single list")
#sorting columns
trips <- lapply(list(trips.11_14,trips.15), function(x) x[ , .(usertype, gender, bikeid, trip_duration,
                                                                  start_date, end_date,
                                                                  start_station_id, end_station_id,
                                                                  start_station_name, end_station_name,
                                                                  start_station_latitude, end_station_latitude,
                                                                  start_station_longitude, end_station_longitude,
                                                                  start_station_docks, end_station_docks,
                                                                  start_station_municipality, end_station_municipality)])
#merging into single trip list
trips <- rbindlist(trips)
#fixing data classes
print("Formatting trip columns from raw character into proper data types")
trips[ , trip_duration := as.numeric(trip_duration)]
trips[ , end_date := fastPOSIXct(end_date, tz='UTC')]
trips[ , start_date := fastPOSIXct(start_date, tz='UTC')]

#misc formatting
trips[gender==0, gender := NA]
trips[gender==1, gender := "Male"]
trips[gender==2, gender := "Female"]
trips[usertype=="Customer", usertype := "Casual"]
trips[usertype=="Subscriber", usertype := "Member"]

#Cleanup & Save####
fwrite(stations, file = "./data/stations.csv")
fwrite(missing, file = "./data/stations_new.csv")
save(trips, file = "./data/tripdata.RData")
save(stations, file = "./data/stationdata.RData")
rm(list=ls())
