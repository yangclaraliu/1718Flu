setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
record_list <- list.files(pattern=glob2rx("weekly_environment*RData"))
record_list[which.max(gsub(".RData","",gsub("weekly_environment","",record_list)))]
load(record_list[which.max(gsub(".RData","",gsub("weekly_environment","",record_list)))])
require(cdcfluview)

HHS <- list()
for (i in 1:10){
  HHS[[i]] <- get_flu_data('hhs',i,'ilinet',c(1997:2017))
}

HHS_regtab <- list()
for(h in 1:10){
  HHS_regtab[[h]] <- data.frame(cbind(HHS[[h]]$YEAR,HHS[[h]]$WEEK,as.numeric(HHS[[h]]$`% WEIGHTED ILI`),as.numeric(HHS[[h]]$`% WEIGHTED ILI`)))
  
  colnames(HHS_regtab[[h]]) <- c("yr","wk","ILIp","ILIc")
  HHS_regtab[[h]]["season"] <- NA
  start <- which(HHS_regtab[[h]]$wk==40)
  end <- c(which(HHS_regtab[[h]]$wk==39),nrow(HHS_regtab[[h]]))
  for(s in 1:length(start)){
    HHS_regtab[[h]][start[s]:end[s],"season"] <- s
  }
  HHS_regtab[[h]]["time"] <- paste(HHS_regtab[[h]]$season,HHS_regtab[[h]]$wk,sep="-")
  HHS_regtab[[h]][c(paste("wm",colnames(weekly_mean[[h]])[2:9],sep="_"),paste("wsd",colnames(weekly_mean[[h]])[2:9],sep="_"))] <- NA
  HHS_regtab[[h]][1:nrow(weekly_mean[[h]]),which(colnames(HHS_regtab[[h]])%in%c(paste("wm",colnames(weekly_mean[[h]])[2:9],sep="_"),paste("wsd",colnames(weekly_mean[[h]])[2:9],sep="_")))] <- cbind(weekly_mean[[h]][,2:9],weekly_sd[[h]][2:9])
}

setwd("C:/Users/liux3204/Google Drive/Influenza/17-18_forecast/HumNat")
record_no <- max(as.numeric(gsub("HHS_timealign","",gsub(".RData","",list.files(pattern=glob2rx("HHS_timealign*.RData"))))))
newname <- paste("HHS_timealign",record_no+1,".RData",sep="")
newname
save(HHS_regtab,file=newname)
