require(cdcfluview); require(scales)
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
source("draw.wk.sim.R");source("draw.onsettime.sim.R"); source("draw.peakweek.sim.R");source("draw.peakweek.sim.R")
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/local_to_national_model.RData")
baseline <- read.csv("wILI_Baseline.csv",stringsAsFactors = F)

#Gai
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/RawResults")
list.files(pattern=".RData")
submission_date <- seq(as.Date("2017-11-6"),as.Date("2018-05-14"),7)
report = 1
path <- paste("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/RawResults/ENVR_RawResult_",submission_date[report],".RData",sep="")
load(path)

#Gai
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/fore_tab_all")
epi_weeks <- c(40:52,1:39)
paste("EW",epi_weeks[report+3],sep="")
load(list.files(pattern=".RData")[which(grepl("43",list.files(pattern=".RData"),fixed = T)==T)])

#Gai
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/Submissions")
base <- get_flu_data("national",,"ilinet",c(2002:2017))
submission <- read.csv(list.files(pattern=paste(submission_date[report])),stringsAsFactors = F)

nsim = 5000
sim_res_national <- data.frame(matrix(NA,ncol=nsim,nrow=nrow(sim_res_all[[1]])))
for(i in 1:nsim){
  sample1 <- function(h){sim_res_all[[h]][,sample(c(1:ncol(sim_res_all[[h]])),1)]}
  selected <- data.frame(sapply(c(1:10),sample1))
  colnames(selected) <- paste("HHS",c(1:10),sep="")
  sim_res_national[,i] <- (predict(local_to_national_model,selected)) 
}

peaksize <- peaktime <- onsettime <- rep(NA,ncol(sim_res_national))
for(i in 1:ncol(sim_res_national)){
  ILIp <- c(base$`% WEIGHTED ILI`,sim_res_national[,i])
  ILIp <- ILIp[!is.na(ILIp)]
  ILIp_15 <- unlist(tail(ILIp,52))
  onsettime_sim_res_national <- (intersect(intersect(which(ILIp_15>baseline[1,12]),which(ILIp_15>baseline[1,12])+1),which(ILIp_15>baseline[1,12])+2)-2)
  if (length(onsettime_sim_res_national)!=0)  {onsettime[i] <- min(onsettime_sim_res_national)} else {onsettime[i] <- NA}
  peaksize[i] <- max(ILIp_15)
  peaktime[i] <- which.max(ILIp_15)
}

res <- c(epi_weeks[round(mean(onsettime,na.rm=T))],draw.onset.sim(onsettime)[,2],
         epi_weeks[round(mean(peaktime))],draw.peakweek.sim(peaktime)[,2],
         mean(peaksize),draw.peak.sim(peaksize)[,3],
         mean(unlist(sim_res_national[1,])),draw.wk.sim((unlist(sim_res_national[1,])))[,3],
         mean(unlist(sim_res_national[2,])),draw.wk.sim((unlist(sim_res_national[2,])))[,3],
         mean(unlist(sim_res_national[3,])),draw.wk.sim((unlist(sim_res_national[3,])))[,3],
         mean(unlist(sim_res_national[4,])),draw.wk.sim((unlist(sim_res_national[4,])))[,3])
submission[submission$Location=="US National","Value"] <- res
#change
file_name <-paste(paste(paste("EW",epi_weeks[report+3],"-HumNat-",submission_date[report],sep=""),".csv",sep=""))
setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/Submissions")
write.csv(submission,file_name,row.names = F)

#Gai
setwd(paste("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat/Weekly_Forecasts_Plots/Report",report,sep=""))
png.name <- paste(paste("National",submission_date[report],sep="-"),"-4wk.png",sep="")
png(png.name)
week_in <- tail(base$WEEK,1)-40+1#+52
plot(1,xlab="Calendar Week",ylab="wILI%",ylim=c(1,max(unlist(sim_res_national[1:4,]))+0.1),type="n",xlim=c(week_in-2,week_in+4),main=paste("National "," \nForecast Generated Based on EW",40+week_in-1,sep=""),xaxt="n")
points(x=jitter(rep(week_in+1,ncol(sim_res_national))),y=sim_res_national[1,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+2,ncol(sim_res_national))),y=sim_res_national[2,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+3,ncol(sim_res_national))),y=sim_res_national[3,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+4,ncol(sim_res_national))),y=sim_res_national[4,],col=alpha("grey",0.5),pch=20)
points(head(ILIp_15,week_in),col="green",pch=20)
points(y=rowMeans(sim_res_national[1:4,]),x=c((week_in+1):(week_in+4)),pch=20,col="red")
lines(head(ILIp_15,week_in),col="green",lwd=2)
lines(y=c(tail(head(ILIp_15,week_in),1),rowMeans(sim_res_national[1:4,])),x=c((week_in):(week_in+4)),col="red",lty=2,lwd=2)
abline(h=baseline[1,13], lty=2)
axis(1,at=seq(1,30,1),labels=epi_weeks[1:30][seq(1,30,1)])
dev.off()
