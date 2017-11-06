#setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast")
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
source("draw.wk.sim.R");source("draw.onsettime.sim.R"); source("draw.peakweek.sim.R");source("draw.peakweek.sim.R")
submission <- read.csv("Long_Flu_Submission_Template_1718.csv")
baseline <- read.csv("wILI_Baseline.csv",stringsAsFactors = F)
#setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast")
#submission_lastyear <- read.csv("Long_Flu_Submission_Template_update.csv")
#sapply(1:7,function(x) which(submission[,x]!=submission_lastyear[,x]))

#load part 1
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/RawResults")
list.files(pattern=".RData")
submission_date <- seq(as.Date("2017-11-6"),as.Date("2018-05-14"),7)
report = 1
path <- paste("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/RawResults/ENVR_RawResult_",submission_date[report],".RData",sep="")
load(path)

#load part 2
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/fore_tab_all")
epi_weeks <- c(40:52,1:39)
paste("EW",epi_weeks[report+3],sep="")
load(list.files(pattern=".RData")[which(grepl("43",list.files(pattern=".RData"),fixed = T)==T)])
submission$Value <- NA

for(h in 1:10){
  temp <- (sim_res_all[[h]])
  peaksize <- peaktime <- onsettime <- rep(NA,ncol(temp))
  for(i in 1:ncol(temp)){
    ILIp <- c(fore_tab_all[[h]]$ILIp,temp[,i])
    ILIp <- ILIp[!is.na(ILIp)]
    ILIp_15 <- unlist(tail(ILIp,52))
    onsettime_temp <- (intersect(intersect(which(ILIp_15>baseline[h+1,12]),which(ILIp_15>baseline[h+1,12])+1),which(ILIp_15>baseline[h+1,12])+2)-2)
    if (length(onsettime_temp)!=0)  {onsettime[i] <- min(onsettime_temp)} else {onsettime[i] <- NA}
    peaksize[i] <- max(ILIp_15)
    peaktime[i] <- which.max(ILIp_15)
  }
  
  res <- c(epi_weeks[round(mean(onsettime,na.rm=T))],draw.onset.sim(onsettime)[,2],
           epi_weeks[round(mean(peaktime))],draw.peakweek.sim(peaktime)[,2],
           mean(as.numeric(peaksize)),draw.peak.sim(peaksize)[,3],
           mean(unlist(temp[1,])),draw.wk.sim((unlist(temp[1,])))[,3],
           mean(unlist(temp[2,])),draw.wk.sim((unlist(temp[2,])))[,3],
           mean(unlist(temp[3,])),draw.wk.sim((unlist(temp[3,])))[,3],
           mean(unlist(temp[4,])),draw.wk.sim((unlist(temp[4,])))[,3]
  )
  name <- paste("HHS Region",h)
  submission[submission$Location==name,"Value"] <- res
}

epi_weeks[report+3]
base <- get_flu_data("national",,"ilinet",c(2002:2016))
file_name <- paste(paste(paste("EW",epi_weeks[report+3],"-HumNat-",submission_date[report],sep=""),".csv",sep=""))
file_name
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/Submissions")
write.csv(submission,file_name,row.names = F)

