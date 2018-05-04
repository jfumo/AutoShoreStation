rm(list=ls())
setwd("/home/jfumo/AutoShoreStation/Oxygen")
library("chron")
library("RColorBrewer")
library("lattice")
library("ncdf4")

CTD=data.frame(time=as.POSIXct(ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/newport_pier-d02-2018.nc"),"time"),origin='1970-01-01 00:00:00'),temperature=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/newport_pier-d02-2018.nc"),'temperature'),conductivity=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/newport_pier-d02-2018.nc"),'conductivity'),pressure=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/newport_pier-d02-2018.nc"),'pressure'),salinity=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/newport_pier-d02-2018.nc"),'salinity'),oxygen_phase_delay_V=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/newport_pier-d02-2018.nc"),'oxygen_phase_delay_V'),oxygen_phase_delay_us=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/newport_pier-d02-2018.nc"),'oxygen_phase_delay_us'))

library(googledrive)
file=drive_download(drive_get("SASS Inventory and Cleaning",id="1099lNMJ3XZQIFv7oSr0Q-5i4fihx7gnvoxMaVhiUhJE")[1,],type='xlsx',overwrite=T)
library(xlsx)
coef=read.xlsx("SASS Inventory and Cleaning.xlsx",sheetName="SBE 63 O2")
coef$START.TIME=as.POSIXct(strptime(as.character(coef$START.TIME),'%Y-%m-%d %H:%M:%S'))

coef=coef[is.na(coef$START.TIME)==F,]


CTD$OxygenDeployment=NA
coef$OxygenDeployment=NA
for(i in 1:length(coef$START.TIME)){
  CTD$OxygenDeployment[CTD$time>=coef$START.TIME[i]]=i
  CTD=CTD[CTD$time<=coef$START.TIME[i]-3600 | CTD$time>=coef$START.TIME[i]+3600,]
  coef$OxygenDeployment[i]=i
}



#coef
E=.011
SolB0=-6.24523e-3
SolB1=-7.37614e-3
SolB2=-1.03410e-2
SolB3=-8.17083e-3
SolC0=-4.88682e-7


row=CTD[2,]

CalcTempC=function(row){
  V=as.numeric(as.character(row$oxygen_phase_delay_us))
  L=log(100000*V/(3.3-V))
  TempC=1/((coef$TA0)+(coef$TA1*L)+(coef$TA2*(L^2))+(coef$TA3*(L^3)))-273.15
  TempC=TempC[row$OxygenDeployment]
  return(TempC)
}

CalcMl.L=function(row){
  P=as.numeric(as.character(row$pressure))
  Temp=CalcTempC(row)
  V=(as.numeric(as.character(row$oxygen_phase_delay_V))/39.457071)
  Ts=log((298.15-Temp)/(273.15+Temp))
  S=row$salinity
  Scorr=exp(S*(SolB0+SolB1*Ts+SolB3*(Ts^3))+SolC0*(S^2))
  ml.l=(((coef$A0+coef$A1*Temp+coef$A2*(V^2))/(coef$B0+coef$B1*V)-1)/(coef$C0+coef$C1*Temp+coef$C2*(Temp^2)))*Scorr*exp(E*(P/(Temp+273.15)))
  ml.l=ml.l[row$OxygenDeployment]
  return(ml.l)
}

CTD$oxygen=NA
for(i in 1:length(CTD$time)){
  row=CTD[i,]
  CTD$oxygen[i]=CalcMl.L(row)
}

pdf("NB Converted Oxygen Data.pdf",width=11,height=8.5)
plot(CTD$oxygen~CTD$time,type='l',col='forestgreen',main='NB Converted Oxygen Data',xlab='Time',ylab='Oxygen (mL/L)')
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
dev.off()


df=data.frame(time_UTC=CTD$time,oxygen_ml.l=CTD$oxygen)

write.csv(df,'NB Converted Oxygen.csv',row.names = F)


drive_update(file=as_id("https://drive.google.com/open?id=15A_xBFzfqZGvdysQV6-N3VT3DMWPlNe4/view?usp=sharing"),media=paste("~/Desktop/stuff/Routine Scripts/Get Oxygen Data/","NB Converted Oxygen Data.pdf",sep=''))



drive_update(file=as_id("https://drive.google.com/open?id=1caVmibn-NtXAlS7ygRkHM2b5PeCtilBA/view?usp=sharing"),media=paste("~/Desktop/stuff/Routine Scripts/Get Oxygen Data/","NB Converted Oxygen.csv",sep=''))


setwd("~/Desktop/stuff/Routine Scripts/Get Converted pH Data")
pH=read.csv("TRIScorrectedData.csv",header=T,sep=',',stringsAsFactors = F)

pH$time=as.POSIXct(strptime(pH$time,'%Y-%m-%d %H:%M:%S'))


pdf('CompareOxygenAndpH.pdf',width=11,height=8.5)
par(mfrow=c(2,1))
plot(df$oxygen_ml.l[df$time_UTC>=min(pH$time)]~df$time_UTC[df$time_UTC>=min(pH$time)],type='l',ylab="Oxygen (mL/L)",xlab=NA,col='forestgreen')
plot(pH$pH~pH$time,type='l',col='blue',ylab='pH',xlab='Time')
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
dev.off()


drive_update(file=as_id("https://drive.google.com/open?id=1aQ7BT8b4c3-qibTtxyy5GJHWxUth9VdK/view?usp=sharing"),media=paste("~/Desktop/stuff/Routine Scripts/Get Converted pH Data/","CompareOxygenAndpH.pdf",sep=''))








quit(save="no")
