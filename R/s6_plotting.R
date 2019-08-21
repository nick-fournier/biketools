#load packages
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

library(ggplot2)
library(data.table)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)
library(extrafont)
library(lubridate)
library(gridExtra)
library(scales)
library(optimx)


#load data
load('./data/ADBdata.RData')
load('./data/censusdata.RData')
#Empty recieving plot list
plots<-list()
font <- "Times New Roman"

#### Setting up plotting data ####
#raw age sex
census.agesex = fread("./data/census/blocks/DEC_10_SF1_P12_with_ann.csv", sep = ",", integer64 = "character")
station.blocks = fread("./data/stations_blocks.csv", sep = ",", integer64 = "character", colClasses = "character")
station.blocks <- station.blocks[ , .(station_id,GEOID10,POP100_RE,HU100_RE,AREA_m2)]
#agesex
colnames(census.agesex) <- as.character(census.agesex[1,])
census.agesex <- census.agesex[-1,c(-1,-3)]
#Fix weird entries in total column
census.agesex[grepl("\\(",`Total:`), `Total:` := tstrsplit(`Total:`,"\\(")[[1]]]
#for stations
cols <- colnames(station.blocks)[-grep("GEOID10|station_id",colnames(station.blocks))]
station.blocks[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
setnames(station.blocks, "GEOID10","GEOID")
#fix names
census.agesex <- census.agesex[ , grep(": - |Id2",colnames(census.agesex)), with=F]
colnames(census.agesex) <- gsub(": - ","_", colnames(census.agesex))
colnames(census.agesex) <- gsub(" |years","", colnames(census.agesex))
setnames(census.agesex,"Id2","GEOID")
#Merge to stations
census.agesex <- merge(station.blocks[,.(GEOID,station_id)], census.agesex, by = "GEOID")
#to numeric
cols <- colnames(census.agesex)[-1:-2]
census.agesex[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
census.agesex[ , (cols) := lapply(.SD, function(x) ifelse(is.na(x),0,x)), .SDcols = cols]
census.agesex <- census.agesex[ , lapply(.SD,sum), .SDcols = cols, by = station_id]

census.agesex[ , Female_15to19 := Female_15to17 + Female_18and19]
census.agesex[ , Female_20to24 := Female_20 + Female_21 + Female_22to24]
census.agesex[ , Female_60to64 := Female_60and61 + Female_62to64]
census.agesex[ , Female_65to69 := Female_65and66 + Female_67to69]
census.agesex[ , Male_15to19 := Male_15to17 + Male_18and19]
census.agesex[ , Male_20to24 := Male_20 + Male_21 + Male_22to24]
census.agesex[ , Male_60to64 := Male_60and61 + Male_62to64]
census.agesex[ , Male_65to69 := Male_65and66 + Male_67to69]
census.agesex <- census.agesex[ , !c("Female_15to17","Female_18and19","Female_20","Female_21","Female_22to24",
                                     "Female_60and61","Female_62to64","Female_65and66","Female_67to69",
                                     "Male_15to17","Male_18and19","Male_20","Male_21","Male_22to24",
                                     "Male_60and61","Male_62to64","Male_65and66","Male_67to69")]

#splitting and stacking
census.agemale <- census.agesex[ , grepl("Male|station_id",colnames(census.agesex)), with=F]
census.agefemale <- census.agesex[ , grepl("Female|station_id",colnames(census.agesex)), with=F]
colnames(census.agemale) <- gsub("Male_","", colnames(census.agemale))
colnames(census.agefemale) <- gsub("Female_","", colnames(census.agefemale))
census.agemale[ , sex := "Male"]
census.agefemale[ , sex := "Female"]
cols <- c("station_id","Under5","5to9","10to14","15to19","20to24","25to29","30to34","35to39","40to44",
          "45to49","50to54","55to59","60to64","65to69","70to74","75to79","80to84","85andover","sex")
census.agefemale <- census.agefemale[ , cols, with=F]
census.agemale <- census.agemale[ , cols, with=F]
census.agesex <- rbind(melt(census.agefemale, id.vars = c("station_id","sex"), variable.factor = T),
                       melt(census.agemale, id.vars = c("station_id","sex"), variable.factor = T))
agesex <- merge(unique(ADB.station[,.(station_id,municipality)]), census.agesex, by = "station_id")
agesex <- agesex[ , sum(value), by = .(municipality,sex,variable)]
agesex[ , total := sum(V1), by = municipality]
agesex[ , prop := V1/total]
rm(census.agefemale,census.agemale, census.agesex)

#ADB.station <- ADB.station[date<"2017-03-01", ]
ADB.station[ , (c("latitude","longitude")) := lapply(.SD,as.numeric), .SDcols = c("latitude","longitude")]
alpha <- unique(ADB.station[ , .(alpha,station_id,latitude,longitude,municipality,year,AADB,phi,sumsqerr)])
alpha.muni <- alpha[ , mean(alpha), by = .(municipality,year)]
alpha.average <- alpha[ , mean(alpha) , by = .(station_id,municipality,latitude,longitude,AADB,phi,sumsqerr)]
setnames(alpha.muni, "V1","alpha")
setnames(alpha.average, "V1","alpha")

ADB.muni <- ADB.station[, sum(ADB.total), by = .(date,year,month,municipality)]
setnames(ADB.muni, "V1","ADB.total")

stations <- merge(stations,unique(ADB.station[,.(station_id,municipality)]))
#stations[ , Rel_Mean_Income := Rel_Mean_Income*relvals['income']]

#### Estimation ADB for fit plots ####
sine <- function(t,alpha,AADB,phi) AADB*(alpha*sin((pi/6)*(as.numeric(t)-phi+3)) + 1)

#### Estimating alpha, AADB, and phi
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

#demo
#ADB.muni[municipality=="Boston" & year==2012, sinefit(ADB.total,month)]
# round(fit,2)
# ggplot() + theme_classic() + scale_x_continuous(breaks=0:14) + scale_y_continuous(limits = c(0,5000)) +
#   geom_point(data=ADB.muni[municipality=="Boston" & year==2012,], aes(x=month, y=ADB.total)) +
#   geom_line(data=data.table(month=seq(0,14,0.1),ADB.est=sine(seq(0,14,0.1),fit[1],fit[2],fit[3])), aes(x=month, y=ADB.est))

#Computing for each muni and year
fit <- ADB.muni[ , sinefit(ADB.total,month), by = .(municipality,year)]
ADB.muni <- merge(ADB.muni,fit, by = c("municipality","year"))
rm(fit)

#Biggest station
merge(ADB.station[ , .N, by = station_id],
      ADB.station[,max(AADB),by=station_id], by = "station_id")[order(-V1),]
maxstation = "A32010"
fit.station = data.table(date = seq(from=ADB.station[which.min(as.numeric(date)),date],
                            to=ADB.station[which.max(as.numeric(date)),date],
                            by="days"))
fit.station[ , year := as.numeric(format(date,"%Y"))]
fit.station[ , month := as.numeric(format(date,"%m"))]
fit.station[ , day := as.numeric(format(date,"%d"))]
fit.station[ , ndays :=  days_in_month(date)]
fit.station[ , month := month + day/ndays]
fit.station = merge(fit.station, unique(ADB.station[station_id == maxstation, .(alpha,AADB,phi,year)]), by = "year")
fit.station[ , ADB.est := sine(month,alpha,AADB,phi)]

#Now for municipalities
fit.muni = as.data.table(expand.grid(date = seq(from=ADB.muni[which.min(as.numeric(date)),date],
                                           to=ADB.muni[which.max(as.numeric(date)),date],
                                           by="days"), municipality = unique(ADB.muni$municipality)))
fit.muni[ , year := as.numeric(format(date,"%Y"))]
fit.muni[ , month := as.numeric(format(date,"%m"))]
fit.muni[ , day := as.numeric(format(date,"%d"))]
fit.muni[ , ndays :=  days_in_month(date)]
fit.muni[ , month := month + day/ndays]
fit.muni = merge(fit.muni, unique(ADB.muni[,.(alpha,AADB,phi,year,municipality,sumsqerr)]), by = c("year","municipality"))
fit.muni[ , ADB.est := sine(month,alpha,AADB,phi)]

#Bar chart data
#x label order
xorder <- alpha[ , mean(alpha), by=station_id][order(V1),station_id]
#pop density data
popdat <- stations[stations$station_id %in% xorder, .(station_id,Rel_POPDens,Rel_WorkDens)]
popdat[ , WorkDens := Rel_WorkDens*relvals['wdens']]
popdat[ , PopDens := Rel_POPDens*relvals['pdens']]

popdat <- melt(popdat[ , .(station_id,WorkDens,PopDens)], id.vars = "station_id")

#Histogram data
histdat <- merge(as.data.table(expand.grid(year = 2012:2017, month = 1:12)),
                 ADB.station[year<2018 & year>2011, .SD[which.max(ADB.total)], by = .(station_id,year)][ , .N, by = .(month,year)],
                 by = c("year","month"),all.x=T)
histdat[is.na(N),N:=0]

#Alpha plots ####
plots[['alphajit']] <- ggplot(data=alpha[year<2018,]) + #alpha[max_month>1 & min_ADB.total>0,],
  geom_jitter(aes(y=alpha, x=year, group=municipality, color=municipality), shape=1, width = 0.25, size=1, alpha=0.75) +
  stat_smooth(aes(x=year, y=alpha, color="Trend (loess)"), method="loess", fullrange=F, span=1, se=F, linetype='twodash', size=0.75) +
  scale_x_continuous("",expand=c(0,0), limits=c(2010.5,2018.5), breaks=2011:2017) +
  scale_y_continuous(expression(alpha~"value"), expand=c(0,0), limits=c(0,1.05)) +
  scale_color_brewer("Municipality",palette = 'Set1', breaks=c("Boston","Brookline","Cambridge","Somerville","Trend (loess)")) +
  theme_classic() + theme(text=element_text(family=font),
                          legend.position = "none",#c(0.93,0.5),
                          legend.direction = "vertical",
                          legend.background = element_blank())
#plots[['alphajit']]
#Alpha muni plot
plots[['alphamuni']] <- ggplot(data=alpha.muni[year<2018,]) +
  geom_point(aes(x=as.integer(year), y=alpha, color = municipality), size=2) +
  geom_line(aes(x=as.integer(year), y=alpha, color = municipality)) +
  stat_smooth(aes(x=year, y=alpha, color="Trend (loess)"), method="loess", fullrange=F, span=1, se=F, linetype='twodash', size=0.75) +
  scale_x_continuous("", expand=c(0,0), limits=c(2010.5,2018.5), breaks=2011:2017) +
  scale_y_continuous(expression(alpha~"value"), expand=c(0,0), limits=c(0,1.05)) +
  scale_color_brewer("Municipality",palette = 'Set1', breaks=c("Boston","Brookline","Cambridge","Somerville","Trend (loess)")) +
  theme_classic() + theme(text=element_text(family=font),
                          legend.position = "none",#c(0.93,0.5),
                          legend.direction = "vertical",
                          legend.background = element_blank())
#plots[['alphamuni']]
#hist
plots[['histsplit']] <- ggplot(data=histdat) +
  geom_col(aes(x=month, y = N, fill = as.factor(year)), color = "black", position = "dodge") +
  scale_x_continuous(NULL, breaks=1:12, labels = format(seq(ISOdate(2011,1,1), by = "month", length.out = 12),"%b")) +
  scale_y_continuous("Frequency", breaks = seq(0,100,25)) +
  scale_fill_brewer("Year",palette = "Set1") +
  theme_classic() +
  theme(legend.position = c(0.1,0.5),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background = element_rect(color='black',fill='white'),
        text=element_text(family=font))
plots[['histsum']] <- ggplot(data=histdat) +
  geom_col(data=histdat[ , sum(N), by = month], aes(x=month, y = V1), color = "black") +
  scale_x_continuous(NULL, breaks=1:12, labels = format(seq(ISOdate(2011,1,1), by = "month", length.out = 12),"%b")) +
  scale_y_continuous("Frequency", breaks = seq(0,300,50)) +
  scale_fill_brewer("Year",palette = "Set1") +
  theme_classic() + theme(text=element_text(family=font))
#grid.arrange(plots[['histsplit']],plots[['histsum']])

#Maps ####
if(!exists(c("shp_bikes","shp_towns"))) {
  #pulling map data
  if(Sys.info()['sysname']=="Linux") {
    #shp_bikes <- readOGR("/media/nick/Beehive/InSync/Academics/MRP Masters Project/QGIS/Output","bikes_towns_WGS84")
    #shp_towns <- readOGR("/media/nick/Beehive/InSync/Academics/MRP Masters Project/QGIS/Output","towns_poly_hubway_WGS84")
    shp_bikes <- readOGR("/media/nick/Beehive/InSync/Academics/Projects/BikeSeasonality/GIS/mapfiles","bikes_towns_WGS84")
    shp_towns <- readOGR("/media/nick/Beehive/InSync/Academics/Projects/BikeSeasonality/GIS/mapfiles","towns_poly_hubway_WGS84")
  } else {
    #shp_bikes <- readOGR("C:/Users/Nick/Google Drive/Academics/MRP Masters Project/QGIS/Output","bikes_towns_WGS84")
    #shp_towns <- readOGR("C:/Users/Nick/Google Drive/Academics/MRP Masters Project/QGIS/Output","towns_poly_hubway_WGS84")
    shp_bikes <- readOGR("C:/Users/Nick/Google Drive/Academics/Projects/BikeSeasonality/GIS/mapfiles","bikes_towns_WGS84")
    shp_towns <- readOGR("C:/Users/Nick/Google Drive/Academics/Projects/BikeSeasonality/GIS/mapfiles","towns_poly_hubway_WGS84")
  }

  #keeping fac_type column
  shp_bikes@data$id <- rownames(shp_bikes@data)
  Fac_Type <- shp_bikes@data[, c("id","Fac_Type")]
  shp_bikes_df <- data.table(fortify(shp_bikes))# Next the shapefile has to be converted to a dataframe for use in ggplot2
  shp_towns_df <- data.table(fortify(shp_towns))
  shp_bikes_df <- merge(shp_bikes_df, Fac_Type, by = "id") #merging fac_type back
  shp_bikes_df[Fac_Type == "1", Fac_Type := "Bike lane"]
  shp_bikes_df[Fac_Type == "2", Fac_Type := "Separated bike lane"]
  shp_bikes_df[Fac_Type == "3", Fac_Type := "Shared lane (Sharrow)"]
  shp_bikes_df[Fac_Type == "5", Fac_Type := "Paved path"]
  shp_bikes_df[Fac_Type == "6", Fac_Type := "Unpaved path"]
  shp_bikes_df[Fac_Type == "9", Fac_Type := "Shared lane (Sharrow)"]
  #Google map #42.359134, -71.080125
  map <- get_map(location=c(lon=median(alpha$longitude), lat=median(alpha$latitude)), color="bw", source="google", maptype="roadmap", zoom=13)
  #map <- get_map(location=c(lon=-71.080125, lat=42.359134), color="bw", source="google", maptype="roadmap", zoom=13)
}
#spatial plot
plots[['map']] <- ggmap(map) +
  geom_path(data = shp_towns_df, aes(x=long, y=lat, group=group), color='black', size=0.2) +
  geom_point(data=alpha[year==2016,], aes(x=longitude, y=latitude, fill=-alpha, size=AADB), shape=21, stroke=0.25, alpha=0.8) +
  #scale_fill_continuous("Alpha Value", high="#084594", low="#9ecae1", breaks = seq(0,-1,by=-0.2), labels=seq(0,1,by=0.2)) +
  scale_fill_continuous("Alpha Value", high="#3288bd", low="#d53e4f", breaks = seq(0,-1,by=-0.2), labels=seq(0,1,by=0.2)) +
  scale_size_continuous("AADB") +
  theme_void() +
  theme(legend.position = c(0.1,0.3),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background = element_rect(color='black',fill='white'),
        legend.margin = margin(6, 6, 6, 6))
#plots[['map']]

#
plots[['facmap']] <- ggmap(map, darken = c(0.5, "white")) +
  geom_path(data=shp_bikes_df[!(Fac_Type %in% c("4","8","6","7",NA))], aes(x=long, y=lat, group=group,color=Fac_Type), size=1, alpha=0.8) +
  geom_path(data=shp_towns_df, aes(x = long, y = lat, group = group), color = 'black', size = 0.25) +
  geom_point(data=alpha.average, aes(x=longitude, y=latitude, fill=-alpha, size=AADB), shape=21, stroke=0.25, alpha=0.8) +
  scale_color_manual("Bicycle Facility Type", values = c("Shared lane (Sharrow)"="#377eb8",
                                                           "Bike lane"="#4daf4a",
                                                           "Separated bike lane"="#984ea3",
                                                           "Unpaved path"="#ff7f00",
                                                           "Paved path"="#ffff33")) +
  scale_fill_continuous("Alpha Value", high="#3288bd", low="#d53e4f", breaks = seq(0,-1,by=-0.2), labels=seq(0,1,by=0.2)) +
  scale_size_continuous("AADB") +
  theme_void() +
  theme(#legend.position = c(1.2,0.5),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background = element_rect(color='black',fill='white'),
        legend.margin = margin(6, 6, 6, 6))
#plots[['facmap']]

plots[['facmapbase']] <- ggmap(map, darken = c(0.5, "white")) +
  geom_path(data=shp_bikes_df[!(Fac_Type %in% c("4","8","6","7",NA))], aes(x=long, y=lat, group=group,color=Fac_Type), size=1, alpha=0.8) +
  geom_path(data=shp_towns_df, aes(x = long, y = lat, group = group), color = 'black', size = 0.25) +
  #geom_point(data=alpha.average, aes(x=longitude, y=latitude, fill=-alpha, size=AADB), shape=21, stroke=0.25, alpha=0.8) +
  scale_color_manual("Bicycle Facility Type", values = c("Shared lane (Sharrow)"="#377eb8",
                                                         "Bike lane"="#4daf4a",
                                                         "Separated bike lane"="#984ea3",
                                                         "Unpaved path"="#ff7f00",
                                                         "Paved path"="#ffff33")) +
  scale_fill_continuous("Alpha Value", high="#3288bd", low="#d53e4f", breaks = seq(0,-1,by=-0.2), labels=seq(0,1,by=0.2)) +
  scale_size_continuous("AADB") +
  theme_void() +
  theme(#legend.position = c(1.2,0.5),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.background = element_rect(color='black',fill='white'),
    legend.margin = margin(6, 6, 6, 6))
#plots[['facmapbase']]

# 1 = On-road marked lane
# 2 = On-road divided lane
# 3 = On-road sign-posted bike route
# 4 = On-road unsigned (route might be described in map or promotional materials)
# 5 = Off-road shared use path
# 6 = Off-road minimally improved path
# 7 = Off-road unimproved route
# 9 = On-road marked shared lane

#Sine fit  ####
maxlabels <- unique(ADB.station[station_id == maxstation,.(year,alpha,AADB,phi)])
maxlabels[ , date:=as.Date(paste(year,"02-15",sep="-"))]
#Sine fit for one station example
plots[['sinefit']] <- ggplot() + theme_classic() +
  geom_point(data=ADB.station[station_id == maxstation,], aes(x=date,y=ADB.total,color=as.character(year)), shape=1, fill=NA) +
  geom_line(data=fit.station, aes(x=date, y=ADB.est, color=as.character(year))) +
  scale_color_brewer(NULL, palette = "Dark2") +
  scale_y_continuous("Monthly Average\nDaily Bicycles", breaks = seq(0,350,50)) +
  scale_x_date(NULL, date_breaks = "3 month", date_labels = "%b-%Y",
                limits = c(as.Date("2011-01-01"),as.Date("2019-01-01")), expand = c(0,0)) +
  annotate("text", x=maxlabels$date, y=50, hjust=0, label=paste("alpha==",round(maxlabels$alpha,2)), family=font,size=3,parse=T) +
  annotate("text", x=maxlabels$date, y=25, hjust=0, label=paste("AADB =",round(maxlabels$AADB)), family=font,size=3) +
  annotate("text", x=maxlabels$date, y=0, hjust=0, label=paste("phi==",round(maxlabels$phi,2)), family=font,size=3,parse=T) +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=10),
        #legend.position = c(0.95,0.65),
        legend.position = "none",
        legend.direction = "horizontal")
#plots[['sinefit']]

#Continuous sinusoids total ####
plots[['continuousall']] <- ggplot(data=ADB.station) +
  geom_point(aes(x = date, y = ADB.total, color=municipality), size=0.5) +
  scale_color_brewer("Municipality", palette = "Set1") +
  scale_y_continuous("Monthly Average\nDaily Bicycles", label=scales::comma, breaks = seq(0,400,50)) +
  scale_x_date(NULL, date_breaks = "6 month", date_labels = "%b-%y",
               limits = c(as.Date("2011-06-01"),NA), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y = element_text(size=10),
        plot.margin=unit(c(5,5,0,10),"pt"),
        legend.position = "none")
#plots[['continuousall']]
#
plots[['continuousmuni']] <- ggplot() +
  #geom_smooth(aes(x=date, y=ADB.total, color=municipality, group=interaction(municipality,year)),size=0.25, linetype=2, se=F, method="loess") +
  geom_point(data=ADB.muni, aes(x=date, y=ADB.total, color=municipality), size=1) +
  geom_line(data=fit.muni[year<2018,], aes(x=date, y=ADB.est, color=municipality, fill=as.character(year))) +
  scale_color_brewer("Municipality", palette = "Set1") +
  scale_y_continuous("Total Monthly Average\nDaily Bicycles", label=scales::comma, breaks = seq(0,8000,2000)) +
  scale_x_date(NULL, date_breaks = "6 month", date_labels = "%b-%y",
               limits = c(as.Date("2011-06-01"),NA), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y = element_text(size=10),
        plot.margin=unit(c(5,5,0,0),"pt"),
        legend.position = "bottom")
#plots[['continuousmuni']]
#grid.arrange(plots[['continuousall']],plots[['continuousmuni']])
#sine diagram ####
plots[['sineexamplealpha']] <- ggplot(data=data.table(x=c(0,12)), aes(x)) +
  geom_vline(xintercept = 6, linetype="dashed") +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic() +
  scale_x_continuous("Month, t", breaks = 1:12, expand = c(0,0)) +
  scale_y_continuous("Monthly Average Daily Bicycles (MADB)", breaks = c(0,5), limits = c(0,2.5), expand = c(0,0)) +
  stat_function(fun=function(x) sine(x,0.125,1,6)) +
  stat_function(fun=function(x) sine(x,0.50,1,6)) +
  stat_function(fun=function(x) sine(x,1.00,1,6)) +
  annotate("text",label="phi==6",x=6.1, y=0, hjust=0, vjust=-1, parse=T, family=font) +
  annotate("text",label="AADB",x=0.5, y=1.01, hjust=0, vjust=0, parse=T, family=font) +
  annotate("text",label="alpha==0.125",x=6.1, y=1.1, hjust=0, vjust=-1, parse=T, family=font) +
  annotate("text",label="alpha==0.5",x=6.1, y=1.5, hjust=0, vjust=-1, parse=T, family=font) +
  annotate("text",label="alpha==1.0",x=6.1, y=2, hjust=0, vjust=-1, parse=T, family=font) +
  theme(text=element_text(family=font))
#
plots[['sineexampleaadb']] <- ggplot(data=data.table(x=c(0,12)), aes(x)) +
  geom_vline(xintercept = 6, linetype="dashed") +
  geom_hline(yintercept = 0.25, linetype="dashed") +
  geom_hline(yintercept = 0.50, linetype="dashed") +
  geom_hline(yintercept = 1.00, linetype="dashed") +
  theme_classic() +
  scale_x_continuous("Month, t", breaks = 1:12, expand = c(0,0)) +
  scale_y_continuous("Monthly Average Daily Bicycles (MADB)", breaks = c(0,5), limits = c(0,1.75), expand = c(0,0)) +
  stat_function(fun=function(x) sine(x,0.75,0.25,6)) +
  stat_function(fun=function(x) sine(x,0.75,0.5,6)) +
  stat_function(fun=function(x) sine(x,0.75,1.0,6)) +
  annotate("text", label="phi==6", x=4, y=0, hjust=0, vjust=-0.5, parse=T, family=font) +
  annotate("text", label="alpha==0.75", x=4, y=0, hjust=0, vjust=-2, parse=T, family=font) +
  annotate("text", label="AADB==frac(x,4)", x=6.1, y=0.25, hjust=0, vjust=1.1, parse=T, family=font) +
  annotate("text", label="AADB==frac(x,2)", x=6.1, y=0.50, hjust=0, vjust=-0.1, parse=T, family=font) +
  annotate("text", label="AADB==x", x=6.1, y=1.0, hjust=0, vjust=-0.2, parse=T, family=font) +
  theme(text=element_text(family=font))
plots[['sineexample']] <- grid.arrange(plots[['sineexamplealpha']], plots[['sineexampleaadb']],ncol=2)

#Bar charts ####
#Alpha
plots[['alphabar']] <- ggplot(data=alpha[ , mean(alpha), by=station_id]) +
  geom_col(aes(x=station_id, y=V1),fill="#1f78b4") +
  scale_y_continuous(expression(alpha~"value"), breaks=seq(0,1,0.2), expand = c(0,0)) +
  scale_x_discrete("Station (ordered by value)", limits=xorder, label=NULL) +
  scale_fill_brewer(NULL, palette = "Set1", labels = c("WorkDens"="Workforce", "PopDens"="Residential")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=6, vjust=.5),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        legend.position = "bottom")
#plots[['alphabar']]
#Income
plots[['incomebar']] <- ggplot(data=stations[ , Rel_Mean_Income*relvals['income'], .(station_id,Rel_Mean_Income)]) +
  geom_col(aes(x=station_id, y=V1), color="black", size=0.1, fill="#1f78b4") +
  scale_y_continuous(NULL, limits=c(0,120000), breaks=seq(0,12e4,2e4), label = dollar, expand = c(0,0)) +
  scale_x_discrete("Station (ordered by value)", limits=stations[order(Rel_Mean_Income), station_id], label=NULL) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=8),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        legend.position = "bottom")
#plots[['incomebar']]
#age
setorderv(stations, as.data.table(t(stations[ , lapply(.SD, mean), .SDcols = grepl("Age",colnames(stations))]), keep.rownames = T)[order(-V1),rn])
plots[['agebar']] <- ggplot(data=melt(stations[ , grepl("station_id|Age",colnames(stations)),with=F], id.vars = "station_id")) +
  geom_col(aes(x=station_id, y=value, fill=variable),size=0.1, color="black") +
  scale_y_continuous(NULL, limits=c(0,1), label = scales::percent, expand = c(0,0)) +
  scale_x_discrete("Station (ordered by age group prevalence)", limits=stations[,station_id], label=NULL) +
  scale_fill_brewer("Age group", labels=c("Age_0to9"="0 to 9",
                             "Age_10to17"="10 to 17",
                             "Age_18to24"="18 to 24",
                             "Age_25to29"="25 to 29",
                             "Age_30to39"="20 to 39",
                             "Age_40to49"="40 to 49",
                             "Age_50to65"="50 to 65",
                             "Age_65to75"="65 to 75",
                             "Age_75over"="over 75"), palette = "Set1") +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=8),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        legend.position = "bottom")
#plots[['agebar']]
#Pop density
plots[['popbar']] <- ggplot(data=popdat) +
  geom_col(aes(x=station_id, y=value, fill=variable), size=0.1, color="black", position = "dodge") +
  scale_y_continuous(expression("Population density per"~km^2), labels = scales::comma, breaks=seq(0,1.5e5,1e4), limits=c(0,1.5e5),expand = c(0,0)) +
  scale_x_discrete("Station", limits=xorder, label=NULL) +
  scale_fill_brewer(NULL, palette = "Set1", labels = c("WorkDens"="Workforce", "PopDens"="Residential")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=8),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        plot.margin=unit(c(0,0,0,22),"pt"),
        legend.position = "bottom")
#plots[['popbar']]
#Facility bar
setorderv(stations, as.data.table(t(stations[ , lapply(.SD, mean), .SDcols = grepl("Fac",colnames(stations))]), keep.rownames = T)[order(-V1),rn])
plots[['facbar']] <- ggplot(data=melt(stations[ , grepl("station_id|Fac",colnames(stations)),with=F], id.vars = "station_id")[variable!="Fac_None",]) +
  geom_col(aes(x=station_id, y=value, fill=variable), size=0.1, color="black") +
  scale_y_continuous(NULL, labels = scales::percent, breaks=seq(0,1,0.2), limits=c(0,1), expand = c(0,0)) +
  scale_x_discrete("Station (ordered by facility prevalence) ", limits=stations[,station_id], label=NULL) +
  scale_fill_manual("Bicycle facility type",
                    values = c("Fac_None"="#e41a1c",
                               "Fac_SharedLane"="#377eb8",
                               "Fac_BikeLane"="#4daf4a",
                               "Fac_SepBikeLane"="#984ea3",
                               "Fac_DirtPath"="#ff7f00",
                               "Fac_Path"="#ffff33"),
                    labels = c("Fac_None"="None",
                               "Fac_SharedLane"="Shared lane (Sharrow)",
                               "Fac_BikeLane"="Bike lane",
                               "Fac_SepBikeLane"="Separated bike lane",
                               "Fac_DirtPath"="Unpaved path",
                               "Fac_Path"="Paved path")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=8),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,27,0,11),"pt"),
        legend.position = "bottom")
#plots[['facbar']]
#Landuse
setorderv(stations, as.data.table(t(stations[ , lapply(.SD, mean), .SDcols = grepl("LU",colnames(stations))]), keep.rownames = T)[order(-V1),rn])
plots[['landbar']] <- ggplot(data=melt(stations[ , grepl("station_id|LU",colnames(stations)),with=F], id.vars = "station_id")) +
  geom_col(aes(x=station_id, y=value, fill=variable), size=0.1, color="black") +
  scale_y_continuous(NULL, labels = scales::percent, breaks=seq(0,1,.2), expand = c(0,0)) +
  scale_x_discrete("Station (ordered by land use prevalence)", limits=stations[,station_id], label=NULL) +
  scale_fill_manual("Landuse type",
                    values = c("LU_MultiFamRes" = "#b2df8a",
                               "LU_VLowDensRes" = "#a1d99b",
                               "LU_LowDensRes" = "#74c476",
                               "LU_MedDensRes" = "#238b45",
                               "LU_HighDensRes" = "#00441b",
                               "LU_Commercial" = "#1f78b4",
                               "LU_Industrial" = "#b15928",
                               "LU_Institutional" = "#fb9a99",
                               "LU_Recreation" = "#fdbf6f",
                               "LU_Farm" = "#ff7f00",
                               "LU_Transport" = "#ffff99",
                               "LU_Open" = "#f7fcf5"),
                    labels = c("LU_MultiFamRes" = "Multi-family Residential",
                               "LU_VLowDensRes" = "Very Low\nDensity Residential",
                               "LU_LowDensRes" = "Low Density\nResidential",
                               "LU_MedDensRes" = "Medium Density\nResidential",
                               "LU_HighDensRes" = "High Density\nResidential",
                               "LU_Commercial" = "Commercial",
                               "LU_Industrial" = "Industrial",
                               "LU_Institutional" = "Institutional",
                               "LU_Recreation" = "Recreational",
                               "LU_Farm" = "Agricultural",
                               "LU_Transport" = "Transportation ROW",
                               "LU_Open" = "Open/undeveloped land")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=8),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,0,0,13),"pt"),
        legend.position = "bottom")
#plots[['landbar']]
#Workers
setorderv(stations, as.data.table(t(stations[ , lapply(.SD, mean), .SDcols = grepl("Work",colnames(stations))]), keep.rownames = T)[order(-V1),rn])
plots[['workerbar']] <- ggplot(data=melt(stations[ , grepl("station_id|Work_",colnames(stations)),with=F], id.vars = "station_id")) +
  geom_col(aes(x=station_id, y=value, fill=variable), size=0.1, color="black") +
  scale_x_discrete("Station (ordered by industry prevalence)", limits=stations[,station_id], label=NULL) +
  scale_y_continuous(NULL, labels = scales::percent, breaks=seq(0,1,.2), expand = c(0,0)) +
  scale_fill_manual("Industry sector",
                    values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f",
                               "#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928","#969696"),
                    labels = c("Work_NATRES" = "Natural resources",
                               "Work_TRANSUTILS" = "Transportation & utilities",
                               "Work_CONST" = "Construction",
                               "Work_MFG" = "Manufacturing",
                               "Work_WHLTRADE" = "Wholesale trade",
                               "Work_RETTRADE" = "Retail trade",
                               "Work_INFO" = "Information technology",
                               "Work_FINESTATE" = "Finance & realestate",
                               "Work_PROFSCIMGMT" = "Professional, scientific,\nand management",
                               "Work_EDUCSOC" = "Education & social work",
                               "Work_ARTSACCOM" = "Arts and accommodation",
                               "Work_OTHER" = "Other",
                               "Work_PUBADMIN" = "Public administration")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=8),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(8, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "none")
#plots[['workerbar']]
setorderv(stations, as.data.table(t(stations[ , lapply(.SD, mean), .SDcols = grepl("Race",colnames(stations))]), keep.rownames = T)[order(-V1),rn])
plots[['racebar']] <- ggplot(data=melt(stations[ , grepl("station_id|Race",colnames(stations)),with=F], id.vars = "station_id")) +
  geom_col(aes(x=station_id, y=value, fill=variable), size=0.1, color="black") +
  scale_x_discrete("Station (ordered by race prevalence)", limits=stations[,station_id], label=NULL) +
  scale_y_continuous(NULL, labels = scales::percent, breaks=seq(0,1,.2), expand = c(0,0)) +
  scale_fill_brewer(NULL, palette = "Accent",
                    labels = c("Race_White"="White","Race_Black"="Black",
                               "Race_Native"="Native","Race_Asian"="Asian",
                               "Race_Pacific"="Pacific","Race_Other"="Other",
                               "Race_Twoormore"="Two or more","Race_HispOrLatin"="Hispanic or latino"),
                    limits=c("Race_Asian","Race_Black","Race_HispOrLatin","Race_Native",
                             "Race_Pacific","Race_White","Race_Other","Race_Twoormore")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.x = element_text(angle=90, size=8),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(8, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "bottom")
#plots[['racebar']]
#Aggregate bar charts ####
plots[['workerbar_agg']] <- ggplot(data=melt(stations[ , lapply(.SD, mean), .SDcols = grepl("Work_",colnames(stations)), by = municipality], id.vars = "municipality"))+
  geom_col(aes(x=municipality, y=value, fill=variable), size=0.1, color="black") +
  scale_x_discrete(NULL) +
  scale_y_continuous(NULL, labels = scales::percent, limits=c(0,1), breaks=seq(0,1,.2), expand = c(0,0)) +
  scale_fill_manual(NULL,
                    values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c",
                              "#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928","#003c30"),
                    labels = c("Work_NATRES" = "Natural resources",
                               "Work_TRANSUTILS" = "Transportation & utilities",
                               "Work_CONST" = "Construction",
                               "Work_MFG" = "Manufacturing",
                               "Work_WHLTRADE" = "Wholesale trade",
                               "Work_RETTRADE" = "Retail trade",
                               "Work_INFO" = "Information technology",
                               "Work_FINESTATE" = "Finance & realestate",
                               "Work_PROFSCIMGMT" = "Professional, scientific,\nand management",
                               "Work_EDUCSOC" = "Education & social work",
                               "Work_ARTSACCOM" = "Arts and accommodation",
                               "Work_OTHER" = "Other",
                               "Work_PUBADMIN" = "Public administration")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "right")
#plots[['workerbar_agg']]
#Aggregate fac type
plots[['facbar_agg']] <- ggplot(data=melt(stations[ , lapply(.SD, mean), .SDcols = grepl("Fac_",colnames(stations)), by = municipality], id.vars = "municipality")[variable!="Fac_None",])+
  geom_col(aes(x=municipality, y=value, fill=variable), size=0.1, color="black") +
  scale_y_continuous(NULL, labels = scales::percent, breaks=seq(0,1,0.2), limits=c(0,1), expand = c(0,0)) +
  scale_x_discrete(NULL) +
  scale_fill_manual("Bicycle facility type",
                    values = c("Fac_SharedLane"="#ffff33",
                               "Fac_BikeLane"="#4daf4a",
                               "Fac_SepBikeLane"="#984ea3",
                               "Fac_DirtPath"="#ff7f00",
                               "Fac_Path"="#377eb8"),
                    labels = c("Fac_SharedLane"="Shared lane (Sharrow)",
                               "Fac_BikeLane"="Bike lane",
                               "Fac_SepBikeLane"="Separated bike lane",
                               "Fac_DirtPath"="Unpaved path",
                               "Fac_Path"="Paved path"),
                    limits = c("Fac_SharedLane","Fac_BikeLane","Fac_SepBikeLane","Fac_DirtPath","Fac_Path")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "right")
#plots[['facbar_agg']]
#Aggregate landuse
plots[['landbar_agg']] <- ggplot(data=melt(stations[ , lapply(.SD, mean), .SDcols = grepl("LU_",colnames(stations)), by = municipality], id.vars = "municipality"))+
  geom_col(aes(x=municipality, y=value, fill=variable), size=0.1, color="black") +
  scale_y_continuous(NULL, labels = scales::percent, limits=c(0,1), breaks=seq(0,1,0.2), expand = c(0,0)) +
  scale_x_discrete(NULL) +
  scale_fill_brewer(NULL, palette = "Paired",
                    limits = c("LU_VLowDensRes","LU_LowDensRes","LU_MedDensRes",
                              "LU_HighDensRes","LU_Commercial","LU_Industrial",
                              "LU_Institutional","LU_Transport","LU_Recreation","LU_Farm","LU_Open"),
                    labels = c("LU_VLowDensRes" = "Very Low\nDensity Residential",
                               "LU_LowDensRes" = "Low Density Residential",
                               "LU_MedDensRes" = "Medium Density Residential",
                               "LU_HighDensRes" = "High Density Residential",
                               "LU_Commercial" = "Commercial",
                               "LU_Industrial" = "Industrial",
                               "LU_Institutional" = "Institutional",
                               "LU_Recreation" = "Recreational",
                               "LU_Farm" = "Agricultural",
                               "LU_Transport" = "Transportation ROW",
                               "LU_Open" = "Open/undeveloped land"))+
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "right")
#plots[['landbar_agg']]
#Aggregate race
plots[['racebar_agg']] <- ggplot(data=melt(stations[ , lapply(.SD, mean), .SDcols = grepl("Race",colnames(stations)), by = municipality], id.vars = "municipality"))+
  geom_col(aes(x=municipality, y=value, fill=variable), size=0.1, color="black") +
  scale_y_continuous(NULL, labels = scales::percent, breaks=seq(0,1,0.2), expand = c(0,0)) +
  scale_x_discrete(NULL) +
  scale_fill_brewer(NULL, palette = "Accent",
                    labels = c("Race_White"="White","Race_Black"="Black",
                               "Race_Native"="Native","Race_Asian"="Asian",
                               "Race_Pacific"="Pacific","Race_Other"="Other",
                               "Race_Twoormore"="Two or more","Race_HispOrLatin"="Hispanic or latino"),
                    limits=c("Race_Asian","Race_Black","Race_HispOrLatin","Race_Native",
                             "Race_Pacific","Race_White","Race_Other","Race_Twoormore")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "right")
#plots[['racebar_agg']]
plots[['incomebar_agg']] <- ggplot(data=stations[ , mean(Rel_Mean_Income)*relvals['income'], by = municipality])+
  geom_col(aes(x=municipality, y=V1, fill=municipality), size=0.1, color="black") +
  scale_y_continuous(NULL, labels = scales::dollar, limits=c(0,5e4), breaks=seq(0,5e4,1e4), expand = c(0,0)) +
  scale_x_discrete(NULL) +
  scale_fill_brewer(NULL, palette = "Set1") +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "none")
#plots[['incomebar_agg']]
plots[['agebar_agg']] <- ggplot(data=melt(stations[ , lapply(.SD, mean), .SDcols = grepl("Age_",colnames(stations)), by = municipality], id.vars = "municipality"))+
  geom_col(aes(x=municipality, y=value, fill=variable), size=0.1, color="black") +
  scale_y_continuous(NULL, labels = scales::percent, limits=c(0,1), breaks=seq(0,1,0.1), expand = c(0,0)) +
  scale_x_discrete(NULL) +
  scale_fill_brewer("Age group",palette = "Set1",
                    labels=c("Age_0to9"="0 to 9","Age_10to17"="10 to 17","Age_18to24"="18 to 24",
                             "Age_25to29"="25 to 29","Age_30to39"="20 to 39","Age_40to49"="40 to 49",
                             "Age_50to65"="50 to 65","Age_65to75"="65 to 75","Age_75over"="over 75")) +
  theme_classic() +
  theme(text=element_text(family=font),
        axis.text.y = element_text(size=10),
        legend.key.size = unit(10, "pt"),
        #plot.margin=unit(c(0,20,0,13),"pt"),
        legend.position = "right")
#plots[['agebar_agg']]

#agegend
plots[['agesexbar']] <- ggplot(agesex, aes(x = variable, fill = municipality)) +
  geom_col(data=agesex[sex == "Female",], aes(y=-prop), position = "dodge") +
  geom_col(data=agesex[sex == "Male",], aes(y=prop), position = "dodge") +
  annotate("text", x="Under5", y = 0.05, label = "Male", family=font, hjust=0) +
  annotate("text", x="Under5", y = -0.05, label = "Female", family=font, hjust=0) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(NULL, breaks=seq(-0.15, 0.15, 0.05), labels=paste0(c(seq(15, 0, -5), seq(5, 15, 5)), "%"), limits=c(-.15,.15)) +
  scale_x_discrete(NULL, labels = c("Under 5","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34",
                                    "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64",
                                    "65 to 69","70 to 74","75 to 79","80 to 84","85 and over")) +
  scale_fill_brewer(NULL, palette = "Set1") +
  #coord_flip() +
  theme_bw() + theme(text=element_text(family=font),
                     axis.text.x = element_text(angle=45, size=10, hjust=1),
                     axis.text.y = element_text(size=10),
                     legend.background = element_blank(),
                     legend.position = c(0.9,0.25),
                     legend.key.size = unit(10, "pt"))
#plots[['agesexbar']]

#combined bar chart
# plots[['barsall']] <- grid.arrange(plots[['alphabar']],plots[['incomebar']],plots[['popbar']],
#                                    plots[['facbar']],plots[['landbar']],plots[['workerbar']],ncol=1)
# plots[['bars1']] <- grid.arrange(plots[['alphabar']],plots[['incomebar']],plots[['popbar']],ncol=1)
# plots[['bars2']] <- grid.arrange(plots[['facbar']],plots[['landbar']],plots[['workerbar']],ncol=1)

#### Saving plots to files ####
dir = "../../Papers & Abstracts/BikeSeasonality/LaTex_v2/"
dir = "C:/Users/Nick/Downloads/"
#dir = "../LaTex/MRP_Project_v2/"

names(plots)
print("Saving the plots")
ggsave(paste(dir,"Figures/alphajit.pdf",sep=""), plots[['alphajit']], width = 5, height = 2.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/alphamuni.pdf",sep=""), plots[['alphamuni']], width = 5, height = 2.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/histsplit.pdf",sep=""), plots[['histsplit']], width = 6, height = 2, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/histsum.pdf",sep=""), plots[['histsum']], width = 6, height = 2, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/map.pdf",sep=""), plots[['map']], width = 6, height = 6, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/facmap.pdf",sep=""), plots[['facmap']], width = 7.7, height = 6, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/facmapbase.pdf",sep=""), plots[['facmapbase']], width = 7.7, height = 6, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/sinefit.pdf",sep=""), plots[['sinefit']], width = 7, height = 2.25, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/continuousall.pdf",sep=""), plots[['continuousall']], width = 7, height = 2.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/continuousmuni.pdf",sep=""), plots[['continuousmuni']], width = 7, height = 3, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/sineexample.pdf",sep=""), plots[['sineexample']], width = 8, height = 3, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/sineexampleaadb.pdf",sep=""), plots[['sineexampleaadb']], width = 4, height = 3, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/sineexamplealpha.pdf",sep=""), plots[['sineexamplealpha']], width = 4, height = 3, units = "in", device=cairo_pdf)
#agg bars
ggsave(paste(dir,"Figures/facbar_agg.pdf",sep=""), plots[['facbar_agg']], width = 6, height = 1.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/landbar_agg.pdf",sep=""), plots[['landbar_agg']], width = 6, height = 2, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/workerbar_agg.pdf",sep=""), plots[['workerbar_agg']], width = 7, height = 2.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/incomebar_agg.pdf",sep=""), plots[['incomebar_agg']], width = 6, height = 1.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/racebar_agg.pdf",sep=""), plots[['racebar_agg']], width = 6, height = 2, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/agesexbar.pdf",sep=""), plots[['agesexbar']], width = 7, height = 2.75, units = "in", device=cairo_pdf)
#disagg bars
ggsave(paste(dir,"Figures/alphabar.pdf",sep=""), plots[['alphabar']], width = 7, height = 2.0, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/agebar.pdf",sep=""), plots[['agebar']], width = 7, height = 2.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/incomebar.pdf",sep=""), plots[['incomebar']], width = 6, height = 2, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/popbar.pdf",sep=""), plots[['popbar']], width = 6, height = 2, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/facbar.pdf",sep=""), plots[['facbar']], width = 7, height = 2.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/landbar.pdf",sep=""), plots[['landbar']], width = 6, height = 3, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/workerbar.pdf",sep=""), plots[['workerbar']], width = 6, height = 2, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/racebar.pdf",sep=""), plots[['racebar']], width = 7, height = 2.5, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/bars1.pdf",sep=""), plots[['bars1']], width = 12, height = 6, units = "in", device=cairo_pdf)
ggsave(paste(dir,"Figures/bars2.pdf",sep=""), plots[['bars2']], width = 12, height = 8, units = "in", device=cairo_pdf)
#ggsave(paste(dir,"alphabar.pdf"), plots[['bars1']], width = 7, height = 10, units = "in", device=cairo_pdf)

#dims = data.frame(w = c(6,6,4, 6,6,6, 6,9,1.5*5.5*0.75, 7,1.5*5.5*.25), h = c(2.5,2.5,2, 6,6,3.5, 3.5,4.5,2.5, 4,2.5), row.names = names(plots))
#nplots <- names(plots)
#nplots <- c("popbar","landbar")
#for(i in nplots) ggsave(paste(dir,i,".png",sep=""), plot=plots[[i]], width=dims[i,"w"], height=dims[i,"h"], dpi=300)

#plot displaying active dates
monthchart <- data.table(expand.grid(year=unique(ADB.station$year),
                                     month=unique(ADB.station$month),
                                     municipality=unique(ADB.station$municipality)))
monthchart <- merge(monthchart, ADB.station[, .N, by = .(year,month,municipality)], by = c("year","month","municipality"), all = T)
monthchart <- dcast(monthchart,municipality+year~month, value.var = 'N', fill=NA)
monthchart[ , as.character(1:12) := lapply(.SD, function(x) ifelse(!is.na(x),"X",NA)), .SDcols = as.character(1:12)]
colnames(monthchart) <- c("Municipality","Year",format(seq(ISOdate(2011,1,1), by = "month", length.out = 12),"%b"))
monthchart[ , Municipality := c("Boston",rep(NA,7),"Brookline",rep(NA,7),"Cambridge",rep(NA,7),"Somerville",rep(NA,7))]

fwrite(monthchart[Year<2018,], file = paste(dir,"Tables/munimonth.csv",sep=""), col.names = F)
#output tables
fwrite(round(dcast(alpha.muni, year~municipality, value.var = "alpha"),2)[year!=2018,], file = paste(dir,"Tables/munialpha.csv",sep=""), col.names = T)
fwrite(dcast(unique(fit.muni[ ,.(municipality,year,AADB)])[year!=2018,], year~municipality, value.var = "AADB"),
       file = paste(dir,"/Tables/muniAADB.csv",sep=""), col.names = T)
fwrite(dcast(unique(fit.muni[ ,.(municipality,year,phi)])[year!=2018,], year~municipality, value.var = "phi"),
       file = paste(dir,"Tables/muniphi.csv",sep=""), col.names = T)
#fwrite(dcast(alpha.muni, year~municipality, value.var = "min_ADB"), file = paste(dir,"muniADBmin.csv",sep=""), col.names = T)
#fwrite(dcast(alpha.muni, year~municipality, value.var = "max_ADB"), file = paste(dir,"muniADBmax.csv",sep=""), col.names = T)


dcast(alpha.muni, year~municipality, value.var = "alpha")




