require(cdcfluview)
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
record_list <- list.files(pattern=glob2rx("record*RData"))
record_list[length(record_list)]
load(record_list[length(record_list)])
weekly_mean <- weekly_sd <- list()

#colnames(QV2M_mean) <- colnames(T2M_min)

for (h in 1:10){
  #specifiy week of year based on date
  HHS_environment <- cbind(unlist(T2M_min[h,]),unlist(T2M_mean[h,]),unlist(T2M_max[h,]),unlist(TS_min[h,]),unlist(TS_mean[h,]),unlist(TS_max[h,]),unlist(T2MDEW_mean[h,]),unlist(QV2M_mean[h,]))
  
  woy  <- as.numeric(strftime(as.POSIXct(rownames(HHS_environment)),format='%V'))
  yr <- as.numeric(strftime(as.POSIXct(rownames(HHS_environment)),format='%Y'))
  HHS_environment <- as.data.frame(HHS_environment)
  colnames(HHS_environment) <- c('TS_min','TS_mean','TS_max','T2M_min','T2M_mean','T2M_max','T2MDEW_mean','QV2M_mean')
  HHS_environment['woy'] <- woy

  HHS_environment['season'] <- NA
  HHS_environment['yr'] <- yr 
  rm(woy, yr)
  
  start <- matrix(which(HHS_environment$woy==40),ncol=7,byrow=T)[,1]
  end <- c(start[2:length(start)]-1,nrow(HHS_environment))
  for (s in 1:length(start)){
    HHS_environment[start[s]:end[s],"season"] <- s
  }

  HHS_environment['time'] <- paste(HHS_environment$season,HHS_environment$woy,sep='-')
  
  #draw tables to fill
  weekly_mean[[h]] <- as.data.frame(unique(HHS_environment$time))
  colnames(weekly_mean[[h]]) <-'time' 
  weekly_sd[[h]] <- as.data.frame(unique(HHS_environment$time))
  colnames(weekly_sd[[h]]) <-'time' 
  weekly_mean[[h]][,c('TS_min','TS_mean','TS_max','T2M_min','T2M_mean','T2M_max','T2MDEW_mean','QV2M_mean')] <- NA
  weekly_sd[[h]][,c('TS_min','TS_mean','TS_max','T2M_min','T2M_mean','T2M_max','T2MDEW_mean','QV2M_mean')] <- NA
  
  #temporal conversion
  for (i in 1:length(unique(HHS_environment$time))){
    temp <- HHS_environment[HHS_environment$time==weekly_mean[[h]][i,'time'],1:8]
    #unit conversion from K to F
    temp[,1:7] <- temp[,1:7]*9/5-459.67
    weekly_mean[[h]][i,2:9] <- sapply(temp,mean)
    weekly_sd[[h]][i,2:9] <- sapply(temp,sd)
  }
  rm(HHS_environment, temp, h, i)
}

for(h in 1:10) {
  if(any(is.na(weekly_sd[[h]][nrow(weekly_sd[[h]]),]))){
    weekly_sd[[h]][nrow(weekly_sd[[h]]),2:9] <- weekly_sd[[h]][nrow(weekly_sd[[h]])-1,2:9]
  }
}


setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
record_no <- max(as.numeric(gsub("weekly_environment","",gsub(".RData","",list.files(pattern=glob2rx("weekly_environment*.RData"))))))
newname <- paste("weekly_environment",record_no+1,".RData",sep="")
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
save(weekly_mean,weekly_sd,file=newname)

