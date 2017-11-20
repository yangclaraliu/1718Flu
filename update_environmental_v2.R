require(ncdf4); require(sp)
#this code will be ran in the middle of every month during the prediction exercise in order to capture new environmental data
load("C:/Users/liux3204/Google Drive/Influenza/MERRA-2/HHS_boundary.RData")
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
record_list <- list.files(pattern=glob2rx("record*RData"))
load(record_list[length(record_list)])#read the newest record
#load("record15.RData")

record_no <- as.numeric(gsub(".RData","",gsub("record","",record_list[length(record_list)])))#extract the number of record being read

setwd("F:/")
list <- list.files(pattern='.nc4')
days <- length(list)

day <- 1
target <- list[day]
date <- gsub('MERRA2_400.tavg1_2d_slv_Nx.','',target); date <- gsub('.nc4','',date)
date <- as.character(as.Date(date,format='%Y%m%d'))
MERRA2 <- nc_open(target)

lon <- ncvar_get(MERRA2,'lon')
lat <- ncvar_get(MERRA2,'lat',verbose = F)

TS_array <- ncvar_get(MERRA2,'TS')
T2M_array <- ncvar_get(MERRA2,'T2M')
T2MDEW_array <- ncvar_get(MERRA2,'T2MDEW')
QV2M_array <- ncvar_get(MERRA2,'QV2M')
nc_close(MERRA2)

all_pts <- SpatialPoints(expand.grid(lon,lat),proj4string=CRS(proj4string(HHS_boundary)))
inclusion <- list()
for (i in 1:10) {inclusion[[i]] <-as.data.frame(all_pts[which(!is.na(over(all_pts,HHS_boundary[i])))]) }
template <- data.frame(matrix(NA,ncol=days,nrow=10))
new_TS_min <- new_TS_mean <- new_TS_max <- new_T2M_min <- new_T2M_mean <- new_T2M_max <- new_T2MDEW_mean <- new_QV2M_mean <- template

for (day in 1:days){
  target <- list[day]
  date <- gsub('MERRA2_400.tavg1_2d_slv_Nx.','',target); date <- gsub('.nc4','',date)
  date <- as.character(as.Date(date,format='%Y%m%d'))
  MERRA2 <- nc_open(target)
  TS_array <- ncvar_get(MERRA2,'TS')
  T2M_array <- ncvar_get(MERRA2,'T2M')
  T2MDEW_array <- ncvar_get(MERRA2,'T2MDEW')
  QV2M_array <- ncvar_get(MERRA2,'QV2M')
  nc_close(MERRA2)
  
  for (HHS in 1:10){
    daily_TS <- daily_T2M <- daily_T2MDEW <- daily_QV2M <- rep(NA,24)
    hourly <- inclusion[[HHS]]
    for (hour in 1:24){
      for (i in 1:nrow(hourly)){
        hourly[i,'TS'] <- TS_array[which(lon==hourly[i,1]),which(lat==hourly[i,2]),hour]
        hourly[i,'T2M'] <- T2M_array[which(lon==hourly[i,1]),which(lat==hourly[i,2]),hour]
        hourly[i,'T2MDEW'] <- T2MDEW_array[which(lon==hourly[i,1]),which(lat==hourly[i,2]),hour]
        hourly[i,'QV2M'] <- QV2M_array[which(lon==hourly[i,1]),which(lat==hourly[i,2]),hour]
      }
      daily_TS[hour] <- mean(hourly[,'TS'])
      daily_T2M[hour] <- mean(hourly[,'T2M'])
      daily_T2MDEW[hour] <- mean(hourly[,'T2MDEW'])
      daily_QV2M[hour] <- mean(hourly[,'QV2M'])
    }
    new_TS_min[HHS,day] <- min(daily_TS)
    new_TS_mean[HHS,day] <- mean(daily_TS)
    new_TS_max[HHS,day] <- max(daily_TS)
    new_T2M_min[HHS,day] <- min(daily_T2M)
    new_T2M_mean[HHS,day] <- mean(daily_T2M)
    new_T2M_max[HHS,day] <- max(daily_T2M)
    new_T2MDEW_mean[HHS,day] <- mean(daily_T2MDEW)
    new_QV2M_mean[HHS,day] <- mean(daily_QV2M)
  }
  colnames(new_TS_min)[day] <- colnames(new_TS_mean)[day] <- colnames(new_TS_max)[day] <- colnames(new_T2M_min)[day] <- colnames(new_T2M_mean)[day] <- colnames(new_T2M_max)[day] <- colnames(new_T2MDEW_mean)[day] <- colnames(new_QV2M_mean)[day] <- date
}

TS_min <- cbind(TS_min,new_TS_min)
TS_mean <- cbind(TS_mean,new_TS_mean)
TS_max <- cbind(TS_max,new_TS_max)
T2M_min <- cbind(T2M_min,new_T2M_min)
T2M_mean <- cbind(T2M_mean,new_T2M_mean)
T2M_max <- cbind(T2M_max,new_T2M_max)
T2MDEW_mean <- cbind(T2MDEW_mean,new_T2MDEW_mean)
QV2M_mean <- cbind(QV2M_mean,new_QV2M_mean)

colnames(QV2M_mean) <- seq(as.Date("1997-09-29"),as.Date("2017-10-31"),1)

any(c(dim(TS_min)[2],dim(TS_mean)[2],dim(TS_max)[2],dim(T2M_min)[2],dim(T2M_mean)[2],dim(T2M_max)[2],dim(T2MDEW_mean)[2],dim(QV2M_mean)[2])!=length(seq(as.Date("1997-09-29"),as.Date("2017-10-31"),1))) 

newname <- paste("record",record_no+1,".RData",sep="")
#setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast")
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
save(TS_min,TS_mean,TS_max,T2M_min,lon,lat,T2M_mean,T2M_max,T2MDEW_mean,QV2M_mean,file=newname)
