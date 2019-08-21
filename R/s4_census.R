library(data.table)
library(pbapply)

#### Reading Data ####
lodes.workers = fread("./data/LODES/ma_wac_S000_JT00_2015.csv", sep = ",", integer64 = "character")
lodes.headers = fread("./data/LODES/wacheaders.csv", sep = ",", integer64 = "character")
lodes.naicskey = fread("./data/LODES/naicskey.csv", sep = ",")
census.race = fread("./data/census/blocks/DEC_10_SF1_P5_with_ann.csv", sep = ",", integer64 = "character")
census.agesex = fread("./data/census/blocks/DEC_10_SF1_P12_with_ann.csv", sep = ",", integer64 = "character")
census.income = fread("./data/census/tracts/ACS_16_5YR_S1902_with_ann.csv", skip = 1, select = c(2,62), integer64 = "character")
census.ed = fread("./data/census/tracts/ACS_16_5YR_S1501_with_ann.csv", skip = 1, select = c(2, seq(4,148,by=12)), integer64 = "character")
station.xy = fread("./data/stations.csv", sep = ",", integer64 = "character", colClasses = "character")
station.landuse = fread("./data/stations_landuse.csv", sep = ",", integer64 = "character")
station.blocks = fread("./data/stations_blocks.csv", sep = ",", integer64 = "character", colClasses = "character")
station.routes = fread("./data/stations_routes.csv", sep = ",", integer64 = "character", colClasses = "character")
#
#### Cleanup station block set ####
station.blocks <- station.blocks[ , .(station_id,latitude,longitude,GEOID10,POP100_RE,HU100_RE,AREA_m2)]
#Add X Y
station.blocks <- merge(station.xy[,.(station_id,X,Y)], station.blocks, by = "station_id", all = T)
#to numeric
cols <- colnames(station.blocks)[-grep("GEOID10|station_id|X|Y",colnames(station.blocks))]
station.blocks[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
setnames(station.blocks, "GEOID10","GEOID")

#Tract ID
station.blocks[ , TRACTID := substr(GEOID,0,11)]

#### Census Data ####
#agesex
colnames(census.agesex) <- as.character(census.agesex[1,])
census.agesex <- census.agesex[-1,c(-1,-3)]
#Fix weird entries in total column
census.agesex[grepl("\\(",`Total:`), `Total:` := tstrsplit(`Total:`,"\\(")[[1]]]
#to numeric
cols <- colnames(census.agesex)[-1]
census.agesex[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
census.agesex[ , (cols) := lapply(.SD, function(x) ifelse(is.na(x),0,x)), .SDcols = cols]
#condensing
for(i in unique(gsub("Male: - |Female: - ","",colnames(census.agesex)[grepl("-",colnames(census.agesex))]))) {
  census.agesex[ , (paste("Age_",i,sep="")) := rowSums(census.agesex[ , colnames(census.agesex)[grep(i,colnames(census.agesex))], with=F])]
}
census.agesex <- census.agesex[ , !grep(": - ",colnames(census.agesex)), with=F]
colnames(census.agesex) <- gsub(":| ","", colnames(census.agesex))
setnames(census.agesex,"Id2","GEOID")

#race
colnames(census.race) <- as.character(census.race[1,])
#cleanup
census.race <- census.race[-1,c(-1,-(3:5),-grep("^Hispanic or Latino: -",colnames(census.race))), with=F]
colnames(census.race) <- gsub("Not Hispanic or Latino:|:| - ","", colnames(census.race))
colnames(census.race) <- c("GEOID","Race_White","Race_Black","Race_Native","Race_Asian","Race_Pacific","Race_Other","Race_Twoormore","Race_HispOrLatin")

#to numeric
cols <- colnames(census.race)[-1]
census.race[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
census.race[ , (cols) := lapply(.SD, function(x) ifelse(is.na(x),0,x)), .SDcols = cols]

#Income
census.income <- setNames(census.income, c("TRACTID","Mean_Income"))
#Fix weird entries in total column
census.income[Mean_Income == "-", Mean_Income := "0"]
census.income[ , Mean_Income := as.numeric(Mean_Income)]
census.income[is.na(Mean_Income), Mean_Income := 0]
#Education
colnames(census.ed) <- gsub("Total; Estimate; Population ", "", colnames(census.ed))
census.ed[ , ED_LessHighschool :=  rowSums(census.ed[ , grepl("Less than| 12th", colnames(census.ed)), with=F])]
census.ed[ , ED_Highschool :=  rowSums(census.ed[ , grepl("High school", colnames(census.ed)), with=F])]
census.ed[ , ED_Assoc :=  rowSums(census.ed[ , grepl("Some college|Associate's", colnames(census.ed)), with=F])]
census.ed[ , ED_Bach :=  rowSums(census.ed[ , grepl("Bachelor", colnames(census.ed)), with=F])]
census.ed[ , ED_Grad :=  rowSums(census.ed[ , grepl("Graduate", colnames(census.ed)), with=F])]
census.ed <- census.ed[ , .(Id2,ED_LessHighschool,ED_Highschool,ED_Assoc,ED_Bach,ED_Grad)]
setnames(census.ed,"Id2","TRACTID")

#For blocks, adding station ID, aggregating, and taking percentages
census.agesex <- merge(station.blocks[,.(GEOID,station_id)], census.agesex, by = "GEOID")
census.agesex <- census.agesex[ , lapply(.SD,sum), .SDcols = colnames(census.agesex)[-1:-2], by = station_id]
census.agesex <- census.agesex[ , lapply(.SD,function(x) x/Total), by = .(station_id,Total)]
census.agesex <- census.agesex[ , lapply(.SD, function(x) ifelse(is.na(x),0,x)), by = .(station_id)]

# census.agesex[ , Age_15to19 := Age_15to17 + Age_18and19]
# census.agesex[ , Age_20to24 := Age_20 + Age_21 + Age_22to24]
# census.agesex[ , Age_60to64 := Age_60and61 + Age_62to64]
# census.agesex[ , Age_65to69 := Age_65and66 + Age_67to69]
# census.agesex <- census.agesex[ , !c("Age_15to17","Age_18and19","Age_20","Age_21","Age_22to24",
#                                      "Age_60and61","Age_62to64","Age_65and66","Age_67to69"), with=F]
# census.agesex <- census.agesex[ , c("station_id","Total","Male","Female",
#                                     "Age_Under5","Age_5to9","Age_10to14","Age_15to19","Age_20to24","Age_25to29",
#                                     "Age_30to34","Age_35to39","Age_40to44","Age_45to49","Age_50to54","Age_55to59",
#                                     "Age_60to64","Age_65to69","Age_70to74","Age_75to79","Age_80to84","Age_85andover"), with=F]
census.agesex[ , Age_0to9 := Age_Under5years + Age_5to9years]
census.agesex[ , Age_10to17 := Age_10to14years + Age_15to17years]
census.agesex[ , Age_18to24 := Age_18and19years + Age_20years + Age_21years + Age_22to24years]
census.agesex[ , Age_25to29 := Age_25to29years]
census.agesex[ , Age_30to39 := Age_30to34years + Age_35to39years]
census.agesex[ , Age_40to49 := Age_40to44years + Age_45to49years]
census.agesex[ , Age_50to65 := Age_50to54years + Age_55to59years + Age_60and61years + Age_62to64years]
census.agesex[ , Age_65to75 :=  Age_65and66years + Age_67to69years + Age_70to74years]
census.agesex[ , Age_75over := Age_75to79years + Age_80to84years + Age_85yearsandover]
census.agesex <- census.agesex[ , !grep("years",colnames(census.agesex)), with=F]

census.race <- merge(station.blocks[,.(GEOID,station_id)], census.race, by = "GEOID", all.x = T)
census.race <- census.race[ , lapply(.SD,sum), .SDcols = colnames(census.race)[-1:-2], by = station_id]
census.race <- census.race[ , lapply(.SD,function(x) x/rowSums(.SD)), by = .(station_id)]
census.race <- census.race[ , lapply(.SD, function(x) ifelse(is.na(x),0,x)), by = .(station_id)]

#For tracts, adding station ID, aggregating, and taking percentages
census.ed <- merge(station.blocks[,.(TRACTID,station_id)], census.ed, by = "TRACTID")
census.ed <- census.ed[ , lapply(.SD,sum), .SDcols = colnames(census.ed)[-1:-2], by = station_id]
census.ed <- census.ed[ , lapply(.SD,function(x) x/rowSums(.SD)), by = .(station_id)]
census.ed <- census.ed[ , lapply(.SD, function(x) ifelse(is.na(x),0,x)), by = .(station_id)]

census.income <- merge(station.blocks[ , sum(POP100_RE), by = .(TRACTID,station_id)], census.income, by = "TRACTID")
census.income[V1==0, V1:=1] #set missing weight to 1
meanincome <- census.income[,weighted.mean(Mean_Income,V1)]
census.income <- census.income[ , weighted.mean(Mean_Income,V1) / meanincome, by = station_id]
setnames(census.income, "V1","Rel_Mean_Income")

#### Formatting LODES workers ####
#Subset headers we use
lodes.workers <- lodes.workers[,c("w_geocode",lodes.naicskey$VAR), with = F]
#Aggregating column names
for(i in unique(lodes.naicskey$INDUS)) lodes.workers[ , (i) := rowSums(lodes.workers[ , lodes.naicskey[INDUS==i,VAR] , with=F])]
lodes.workers <- lodes.workers[, c("w_geocode", unique(lodes.naicskey$INDUS)), with=F]
setnames(lodes.workers, "w_geocode","GEOID")
colnames(lodes.workers)[-1] <- paste("Work",colnames(lodes.workers)[-1],sep="_")
rm(lodes.headers,lodes.naicskey)
#saving output for mapping
fwrite(file="../QGIS/Data/lodesblocks.csv", setNames(lodes.workers[ , rowSums(.SD), by=GEOID],c("GEOID","WPOP")))

#For blocks, adding station ID, aggregating, and taking percentages
lodes.workers <- merge(station.blocks[,.(GEOID,station_id)], lodes.workers, by = "GEOID")
lodes.workers <- lodes.workers[ , lapply(.SD,sum), .SDcols = colnames(lodes.workers)[-1:-2], by = station_id]
lodes.workers <- lodes.workers[ , Work_Total := rowSums(.SD), by = .(station_id)]
lodes.workers <- lodes.workers[ , lapply(.SD,function(x) x/Work_Total), by = .(station_id,Work_Total)]
lodes.workers <- lodes.workers[ , lapply(.SD, function(x) ifelse(is.na(x),0,x)), by = .(station_id,Work_Total)]

#### Landuse ####
station.landuse[ , station_id := gsub("landuse_station_id_","",layer)]
station.landuse <- station.landuse[ , .(station_id,LU05_DESC,LUCODE,AREA_m2)]
colnames(station.landuse) <- c("station_id","LUDESC","LUCODE","AREA")
#LU coding
station.landuse[LUCODE %in% c(25,3,20,34,6,4,14,37,40,17), LU := "LU_Open"]
station.landuse[LUCODE %in% c(7,9,8,29,26), LU := "LU_Recreation"]
station.landuse[LUCODE %in% c(35,1,36), LU := "LU_Farm"]
station.landuse[LUCODE %in% c(16,39), LU := "LU_Industrial"]
station.landuse[LUCODE %in% c(15), LU := "LU_Commercial"]
station.landuse[LUCODE %in% c(31), LU := "LU_Institutional"]
station.landuse[LUCODE %in% c(18), LU := "LU_Transport"]
station.landuse[LUCODE %in% c(10), LU := "LU_MedDensRes"]
station.landuse[LUCODE %in% c(11), LU := "LU_HighDensRes"]
station.landuse[LUCODE %in% c(12), LU := "LU_MedDensRes"]
station.landuse[LUCODE %in% c(13), LU := "LU_LowDensRes"]
station.landuse[LUCODE %in% c(38), LU := "LU_VLowDensRes"]
#unique(station.landuse[is.na(LU),.(LUCODE,LUDESC)])

station.landuse <- station.landuse[ , sum(AREA), by = .(station_id,LU)]
setnames(station.landuse,"V1","AREA")
station.landuse[ , AREATOT := sum(AREA), by = station_id]
station.landuse[ , LU_PCT := AREA/AREATOT]
station.landuse <- dcast(station.landuse, station_id~LU, value.var = "LU_PCT", fill=0)

#### Bicycle inventory ####
#Cleanup columns & Remove impossible routes
station.routes <- station.routes[start!="", .(station_id,start,end,Fac_Type,length)]
#split the X and Y's
station.routes[, c("start_X", "start_Y") := tstrsplit(start, ", ")]
station.routes[, c("end_X", "end_Y") := tstrsplit(end, ", ")]
#add 6 signif digits to character string
station.routes[ , (c("start_X","start_Y")) := lapply(.SD, function(x) formatC(as.numeric(x),digits=6,format="f")), .SDcols = c("start_X","start_Y")]
setnames(station.routes,"station_id","end_station_id")
#Adding start_station_id
station.routes <- merge(unique(station.blocks[, .(station_id,X,Y)]), station.routes, by.x = c("X","Y"), by.y = c("start_X","start_Y"), all.y=T)
setnames(station.routes,c("station_id","X","Y"),c("start_station_id","start_X","start_Y"))
#Not same OD
station.routes <- station.routes[start_station_id!=end_station_id, ]

#Functional Class
# 0 = Local
# 1 = Interstate
# 2 = Urban or Rural Principal Arterial
# 3 = Urban Principal Arterial or Rural Minor Arterial
# 5 = Urban Minor Arterial or Rural Major Collector
# 6 = Urban Collector or Rural Minor Collector

#Bike Facility Type
station.routes[Fac_Type == "1", Fac_Type := "Fac_BikeLane"] # 1 = On-road marked lane
station.routes[Fac_Type == "2", Fac_Type := "Fac_SepBikeLane"] # 2 = On-road divided lane
station.routes[Fac_Type == "3", Fac_Type := "Fac_SignedRoute"] # 3 = On-road sign-posted bike route
station.routes[Fac_Type == "4", Fac_Type := "Fac_UnsignedRoute"] # 4 = On-road unsigned (route might be described in map or promotional materials)
station.routes[Fac_Type == "5", Fac_Type := "Fac_Path"] # 5 = Off-road shared use path
station.routes[Fac_Type == "6", Fac_Type := "Fac_RoughPath"] # 6 = Off-road minimally improved path
station.routes[Fac_Type == "7", Fac_Type := "Fac_DirtPath"] # 7 = Off-road unimproved route
station.routes[Fac_Type == "9", Fac_Type := "Fac_SharedLane"] # 9 = On-road marked shared lane
station.routes[Fac_Type == "", Fac_Type := "Fac_None"]

#Aggregate bike facility type, calcualte percentage of routes srved by bike facilities
station.routes[ , length := as.numeric(length)]
station.routes <- station.routes[ , sum(length), by = .(start_station_id,Fac_Type)]
station.routes[ , LengthTotal := sum(V1), by = .(start_station_id)]
station.routes[ , pct_length := V1/LengthTotal]
setnames(station.routes, c("start_station_id","V1"), c("station_id","length"))

#cast to wide format
station.routes <- dcast(station.routes, station_id~Fac_Type, value.var = "pct_length", fill = 0)

#### Merging ####
#Making recieving stations data table
stations <- station.blocks[ , lapply(.SD, sum), .SDcols = c("POP100_RE","HU100_RE","AREA_m2"), by = .(station_id,latitude,longitude,X,Y)]
#Doing tha big merge
stations <- merge(stations, lodes.workers, by = "station_id")
stations <- merge(stations, station.routes, by = "station_id")
stations <- merge(stations, station.landuse, by = "station_id")
stations <- merge(stations, census.agesex, by = "station_id")
stations <- merge(stations, census.ed, by = "station_id")
stations <- merge(stations, census.income, by = "station_id")
stations <- merge(stations, census.race, by = "station_id")

#Popdensity relative to the mean density
cols <- c("POP100_RE","HU100_RE","Work_Total")
relvals <- c("income" = meanincome,
             "pdens" = stations[ , 1e6*mean(POP100_RE/AREA_m2)],
             "hdens" = stations[ , 1e6*mean(HU100_RE/AREA_m2)],
             "wdens" = stations[ , 1e6*mean(Work_Total/AREA_m2)])
stations[ , (cols) := lapply(.SD,function(x) x/AREA_m2), .SDcols = cols, by = station_id]
stations[ , POP100_RE := POP100_RE/mean(POP100_RE)]
stations[ , HU100_RE := HU100_RE/mean(HU100_RE)]
stations[ , Work_Total := Work_Total/mean(Work_Total)]
setnames(stations, c("POP100_RE","HU100_RE","Work_Total"), c("Rel_POPDens","Rel_HUDens","Rel_WorkDens"))

#removing non data columns
stations <- stations[ , !c("latitude","longitude","X","Y","AREA_m2","Total"), with=F]

#### Saving ####
save(stations,relvals, file = "./data/censusdata.RData")
#save(inventory_towns, landuse_towns, workers, station_data, file = "./data/censusdata.RData")
rm(list=ls())




