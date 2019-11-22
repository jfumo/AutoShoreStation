rm(list=ls())

SaveToDrive=T

TrisCalTimes=data.frame(time=c("2/2/18 21:37:41","3/1/18 17:05:17","4/3/18 18:50:36","5/16/18 21:32:04","6/22/18 19:42:51","10/3/18 17:35:05","1/29/19 17:56:37","4/3/19 17:08:39","06/12/19 16:52:55","10/3/19 18:58:22","11/07/19 17:06:17"))

wdir=setwd(getwd())

library(ncdf4)
rawData=data.frame(time=as.POSIXct(ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),"time"),origin='1970-01-01 00:00:00'),ph_counts=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph_counts'),ph_voltage=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph_voltage'),ph_raw=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph_raw'),temp_counts=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'temp_counts'),thermistor_voltage=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'thermistor_voltage'),thermistor_raw=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'thermistor_raw'),temperature=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'temperature'),ph=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph'))

rawData2=data.frame(time=as.POSIXct(ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),"time"),origin='1970-01-01 00:00:00'),ph_counts=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'ph_counts'),ph_voltage=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'ph_voltage'),ph_raw=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'ph_raw'),temp_counts=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'temp_counts'),thermistor_voltage=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'thermistor_voltage'),thermistor_raw=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'thermistor_raw'),temperature=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'temperature'),ph=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2019.nc"),'ph'))
rawData=rbind(rawData,rawData2)
rm(rawData2)
rawData[,2:9]=lapply(rawData[,2:9],as.numeric)
#-------------------------------------------------------------------------------------------------------------
#read in google sheets coef and re-calculate ph voltage. 
library(googledrive)
file=drive_download(drive_get("SASS Inventory and Cleaning",id="1099lNMJ3XZQIFv7oSr0Q-5i4fihx7gnvoxMaVhiUhJE")[1,],type='xlsx',overwrite=T)
library(xlsx)
Coef=read.xlsx("SASS Inventory and Cleaning.xlsx",sheetName="pH_NBPier_Coef",colClasses=rep("numeric",12))
rawData$pH_slope=NaN
rawData$pH_intercept=NaN
rawData$temp_slope=NaN
rawData$temp_intercept=NaN
rawData$Ta=NaN
rawData$Tb=NaN
rawData$Tc=NaN
rawData$E0=NaN
rawData$Ts=NaN
rawData$Deployment=NaN
Coef=Coef[is.na(Coef$start_time)==F,]
Coef$start_time=as.POSIXct(Coef$start_time,origin='1970-01-01',tz='UTC')

for(i in 1:length(Coef$start_time)){
  rawData$pH_slope[rawData$time>=Coef$start_time[i]]=Coef$pH_slope[i]
  rawData$pH_intercept[rawData$time>=Coef$start_time[i]]=Coef$pH_intercept[i]
  rawData$temp_slope[rawData$time>=Coef$start_time[i]]=Coef$temp_slope[i]
  rawData$temp_intercept[rawData$time>=Coef$start_time[i]]=Coef$temp_intercept[i]
  rawData$Ta[rawData$time>=Coef$start_time[i]]=Coef$Ta[i]
  rawData$Tb[rawData$time>=Coef$start_time[i]]=Coef$Tb[i]
  rawData$Tc[rawData$time>=Coef$start_time[i]]=Coef$Tc[i]
  rawData$E0[rawData$time>=Coef$start_time[i]]=Coef$E0[i]
  rawData$Ts[rawData$time>=Coef$start_time[i]]=Coef$Ts[i]
  rawData$Deployment[rawData$time>=Coef$start_time[i]]=i
}


rawData$ph_voltage=(rawData$ph_counts*rawData$pH_slope)+rawData$pH_intercept

rawData$ph_voltage[is.na(rawData$temp_slope)==T]=rawData$ph_voltage[is.na(rawData$temp_slope)==T]/1000

rawData$temperature[is.na(rawData$temp_slope)==F]=(rawData$temp_counts[is.na(rawData$temp_slope)==F]*rawData$temp_slope[is.na(rawData$temp_slope)==F])+rawData$pH_intercept[is.na(rawData$temp_slope)==F]
rawData$temperature[is.na(rawData$temp_slope)==T]=((rawData$temp_counts[is.na(rawData$temp_slope)==T]^2)*rawData$Ta[is.na(rawData$temp_slope)==T])+(rawData$temp_counts[is.na(rawData$temp_slope)==T]*rawData$Tb[is.na(rawData$temp_slope)==T])+(rawData$Tc[is.na(rawData$temp_slope)==T])


rawData$TK=rawData$temperature+273.15

#Also use voltage to calculate pH (non-TRIS corrected)
R=Coef$R[1]
Faraday=Coef$F[1]

rawData$ph[is.na(rawData$temp_slope)==F]=(rawData$ph_voltage[is.na(rawData$temp_slope)==F]-rawData$E0[is.na(rawData$temp_slope)==F]-0.001*(rawData$TK[is.na(rawData$temp_slope)==F]-rawData$Ts[is.na(rawData$temp_slope)==F]))/(R*rawData$TK[is.na(rawData$temp_slope)==F]*log(10)/Faraday)

rawData$ph[is.na(rawData$temp_slope)==T]=(Faraday*((rawData$ph_voltage[is.na(rawData$temp_slope)==T])-rawData$E0[is.na(rawData$temp_slope)==T]+0.001*(rawData$temperature[is.na(rawData$temp_slope)==T]-0)))/(R*(273.15+rawData$temperature[is.na(rawData$temp_slope)==T])*log(10))


#-------------------------------------------------------------------------------------------------------------
#create some functions

calcKS_sw <- function(TK,S){
  d = c(-4276.1, 141.328, -23.093)
  c = c(-13856, 324.57, -47.986, 35474, -771.54, 114.723, -2698, 1776)
  I = 19.924*S/(1000-1.005*S)
  lnKS = (d[1]/TK + d[2] + d[3]*log(TK) + (c[1]/TK + c[2] + c[3]*log(TK))*sqrt(I) + (c[4]/TK + c[5] + c[6]*log(TK))*I + c[7]/TK*(I^1.5) + c[8]/TK*I^2) + log(1 - 0.001005*S)
  KS = exp(lnKS)
  return(KS)
}
calc_Eo_int <- function(Vint, TK, pH_insitu){
  TC=TK-273.15
  R = 8.31451
  Faraday = 96487
  dEdtint = -0.00110122833865097
  Svalue = R*(TK)*log(10)/Faraday
  Eoinsitu = Vint - Svalue*pH_insitu
  Eo25C = Eoinsitu + dEdtint*(25-TC)
  return(Eo25C)
}
calc_pH_TRIS <- function(TK, S){
  pH_total_kgsol = (11911.08 - 18.2499*S - 0.039336*(S^2))/TK + (-366.27059 + 0.53993607*S + 0.00016329*(S^2))+ (64.52243 - 0.084041*S)*log(TK) - 0.11149858*TK
  pH_total_kgH2O = pH_total_kgsol + log10(1 - 0.00106*S)
  SO4tot = (0.14/96.062)*(S/1.80655)
  KS=calcKS_sw(TK,S)
  mH_total_kgH2O = 10^-pH_total_kgH2O
  mH_free_kgH2O = mH_total_kgH2O/(1+(SO4tot/KS))
  pH_free_kgH2O = -log10(mH_free_kgH2O)
  return(pH_free_kgH2O)
}
calc_dfet_pHint<-function(Vint, tempC, Eo25C){
  R=8.31451
  Faraday=96487
  dEdtint = -0.00110122833865097
  tempK = tempC + 273.15
  Svalue = R*tempK*log(10)/Faraday
  Eoint = Eo25C + dEdtint*(tempC - 25)
  pH_int=(Vint - Eoint)/Svalue
  return(pH_int)
}


#Create a new variable called Eo25C...
rawData$Eo25C=NaN

#Read in tris times and calculate Eo25C for each calibration point
TrisCalTimes$time=as.POSIXct(strptime(as.character(TrisCalTimes$time),'%m/%d/%y %H:%M:%S'))


for(i in 1:length(TrisCalTimes$time)){
  rowNumber=which(rawData$time==TrisCalTimes$time[i])
  rawData$Eo25C[rowNumber]=calc_Eo_int(rawData$ph_voltage[rowNumber],rawData$TK[rowNumber],calc_pH_TRIS(rawData$TK[rowNumber],35))
}

EoStuff=data.frame(time=rawData$time[as.character(rawData$Eo25C)!="NaN"],Eo25C=rawData$Eo25C[as.character(rawData$Eo25C)!="NaN"],deployment=rawData$Deployment[as.character(rawData$Eo25C)!="NaN"])

#exptrapolate the Eo25C value from the TRIS cal to the all the Eo25C values within each respective deployment
##'deployments' are considered the length of time from one sensor deployment to the next (new coef)
###so all Eo25C values from 1 deployment are averaged and extrapolated across the whole deployment
for(i in 1:length(Coef$start_time)){
  if(i==1){next}
  rawData$Eo25C[rawData$Deployment==i]=mean(rawData$Eo25C[rawData$Deployment==i][which(is.na(rawData$Eo25C[rawData$Deployment==i])==F)])
}




#and remove the portions that are TRIS cals themselves (1 hour on each side of each TRIS cal point)
for(i in 1:length(TrisCalTimes$time)){
  start=TrisCalTimes$time[i]-3600
  end=TrisCalTimes$time[i]+3600
  rawData=rawData[rawData$time<=start | rawData$time>=end,]
}


#Create a new variable called pHint
rawData$pHint=NaN

#and  populate it with this for loop
for(i in 1:length(rawData$time)){
  rawData$pHint[i]=calc_dfet_pHint(rawData$ph_voltage[i],rawData$temperature[i],rawData$Eo25C[i])
}


#remove deployment(s) where thermistor was bad
Retain=cbind(rawData$time[c(51316:78369)],rawData$ph[c(51316:78369)],1,rawData$temperature[c(51316:78369)])
rawData=rawData[-c(51316:78369),]
Retain=rbind(Retain,cbind(rawData$time[c(51316:78369)],rawData$ph[c(51316:78369)],2,rawData$temperature[c(51316:78369)]))
rawData=rawData[-c(51315:96679),]
Retain=rbind(Retain,cbind(rawData$time[c(84565:84582)],rawData$ph[c(84565:84582)],2,rawData$temperature[c(84565:84582)]))
rawData=rawData[-c(84565:84582),]
Retain=rbind(Retain,cbind(rawData$time[c(51314:52030)],rawData$ph[c(51314:52030)],2,rawData$temperature[c(51314:52030)]))
rawData=rawData[-c(51314:53030),]
Retain=rbind(Retain,cbind(rawData$time[c(82819:89935)],rawData$ph[c(82819:89935)],2,rawData$temperature[c(82819:89935)]))
rawData=rawData[-c(82819:89935),]
Retain=rbind(Retain,cbind(rawData$time[c(78847:82819)],rawData$ph[c(78847:82819)],2,rawData$temperature[c(78847:82819)]))
rawData=rawData[-c(78847:82819),]
#
plot(rawData$temperature~rawData$time,type='l')

colnames(Retain)=c("time","pH","Deployment",'Temperature')
Retain=as.data.frame(Retain)
Retain$time=as.POSIXct(Retain$time,origin='1970-01-01')
#-------------------------------------------------------------------------------------------------------------

#plot up the data and save as a .pdf
pdf('TRIScorrectedData.pdf',height=8.5,width=11)
par(mar=c(8,5,5,5))
plot(x=range(rawData$time),y=range(c(rawData$pHint,rawData$ph,Retain$pH)),type='n',main="TRIS Corrected pH data",ylab='pH',xaxt='n',xlab=NA)
lapply(unique(rawData$Deployment),function(x) lines(rawData$ph[rawData$Deployment==x]~rawData$time[rawData$Deployment==x],col='red',type='l'))
lapply(unique(rawData$Deployment),function(x) lines(rawData$pHint[rawData$Deployment==x]~rawData$time[rawData$Deployment==x],col='blue',type='l'))
lapply(unique(Retain$Deployment),function(x) lines(Retain$pH[Retain$Deployment==x]~Retain$time[Retain$Deployment==x],col='gray',type='l'))
legend('topleft',legend=as.vector(c('pH from MBARI Coef','TRIS corrected pH','Flagged Data')),col=c('red','blue','gray'),lty=c(1,1,1),pch=c(NA,NA,NA),bty='n',lwd=2)
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
axis(1,at=as.POSIXct(strptime(seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),'%Y-%m-%d')),labels=seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),las=2)
dev.off()

df=data.frame(time=rawData$time,temperature=rawData$temperature,pH=rawData$pHint)


write.csv(df,row.names = FALSE, "TRIScorrectedData.csv")

#And save that workbook to google drive
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1Fu8sXejMbbj92Rie52B6ajtT4VlPekF3/view?usp=sharing"),
               media="TRIScorrectedData.csv")
}

#And update the plot
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1y_lmprMbP8rqH7nrXsO9sxd0-KaSdJV3/view?usp=sharing"),
               media=("TRIScorrectedData.pdf"))
}

#plot just the TRIS data
pdf('TRIScorrectedPlot.pdf',height=8.5,width=11)
par(mar=c(8,5,5,5))
plot(rawData$pHint~rawData$time,type='n',col='blue',main="NB Pier pH Data",ylab='pH',xlab=NA,xaxt='n')
lapply(unique(rawData$Deployment),function(x) lines(rawData$pHint[rawData$Deployment==x]~rawData$time[rawData$Deployment==x],col='blue',type='l'))
axis(1,at=as.POSIXct(strptime(seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),'%Y-%m-%d')),labels=seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),las=2)
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
dev.off()


#and put that on the server too
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/19G8OhfYsxuFZrTAIeZTEW6A2S6v5Nmo4/view?usp=sharing"),
               media=("TRIScorrectedPlot.pdf"))
}

#And plot the last 48 hours too
pdf("Last48Hours.pdf",width=11,height=8.5)
plot(pHint~time,data=rawData[rawData$time>=tail(rawData$time,1)-(3600*48),],type='l',col='blue',main="NB Pier pH Data- Last 48 Hours",ylab="pH",xlab="Time",xaxt="n")
axis(1,at=as.POSIXct(strptime(paste(unique(substr(rawData$time[rawData$time>=tail(rawData$time,1)-(3600*48)],1,10)),'12:00:00'),'%Y-%m-%d %H:%M:%S')),labels= as.POSIXct(strptime(paste(unique(substr(rawData$time[rawData$time>=tail(rawData$time,1)-(3600*48)],1,10)),'12:00:00'),'%Y-%m-%d %H:%M:%S')))
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
dev.off()

#and put that on the server too
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1TUGqf2aC8Ywv3JCPQB5IjPalJGYHteD9/view?usp=sharing"),media="Last48Hours.pdf")
}


pdf('TRIScorrectedPlotwithTemp.pdf',height=8.5,width=11)
par(mar=c(8,5,5,5))
with(df,plot(temperature~time,col='gray',type='n',xaxt='n',yaxt='n',ylab=NA,xlab=NA,main='TRIS corrected pH with Durafet Temperature'))
lapply(unique(rawData$Deployment),function(x) lines(df$temperature[rawData$Deployment==x]~df$time[rawData$Deployment==x],col='gray'))
axis(4)
mtext(side = 4, line = 3, 'Temperature from Durafet (°C)')
par(new=T)
with(df, plot(pH~time,type='n', axes=F, xlab=NA, ylab=NA,col='blue'))
lapply(unique(rawData$Deployment),function(x) lines(df$pH[rawData$Deployment==x]~df$time[rawData$Deployment==x],col='blue'))
axis(2)
mtext(side = 2, line = 3, 'pH')
axis(1,at=as.POSIXct(strptime(seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),'%Y-%m-%d')),labels=seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),las=2)
legend('topleft',legend=as.vector(c('pH','Temperature')),col=c('blue','gray'),lty=c(1,1),pch=c(NA,NA),bty='n',lwd=c(1.5))
dev.off()

if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/open?id=1Yp3B-LtKJvuBvunakloUuKKjL4VOAjtM/view?usp=sharing"),media="TRIScorrectedPlotwithTemp.pdf")
}


pdf('TRIScorrectedPlotwithTempAndBadData.pdf',height=8.5,width=11)
par(mar=c(8,5,5,5))
with(df,plot(temperature~time,col='gray',type='n',xaxt='n',yaxt='n',ylab=NA,xlab=NA,main='TRIS corrected pH with Durafet Temperature',ylim=range(c(df$temperature,Retain$Temperature))))
lapply(unique(rawData$Deployment),function(x) lines(df$temperature[rawData$Deployment==x]~df$time[rawData$Deployment==x],col='gray'))
lapply(unique(Retain$Deployment),function(x) lines(Retain$Temperature[Retain$Deployment==x]~Retain$time[Retain$Deployment==x],col='gray50'))
axis(4)
mtext(side = 4, line = 3, 'Temperature from Durafet (°C)')
par(new=T)
with(df, plot(pH~time,type='n', axes=F, xlab=NA, ylab=NA,col='blue',ylim=range(c(df$pH,Retain$pH))))
lapply(unique(rawData$Deployment),function(x) lines(df$pH[rawData$Deployment==x]~df$time[rawData$Deployment==x],col='blue'))
lapply(unique(Retain$Deployment),function(x) lines(Retain$pH[Retain$Deployment==x]~Retain$time[Retain$Deployment==x],col='darkblue'))
axis(2)
mtext(side = 2, line = 3, 'pH')
axis(1,at=as.POSIXct(strptime(seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),'%Y-%m-%d')),labels=seq(as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[1]), as.Date(paste(substr(range(rawData$time),1,8),'01',sep='')[2]), by="months"),las=2)
legend('topleft',legend=as.vector(c('pH','Flagged pH','Temperature','Flagged Temperature')),col=c('blue','darkblue','gray','gray50'),lty=c(1,1,1,1),pch=c(NA,NA,NA,NA),bty='n',lwd=c(1.5))
dev.off()

if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/open?id=1LeWloeKLExtI2_X1Sg6sr7oY0v7Zv5rh/view?usp=sharing"),media="TRIScorrectedPlotwithTempAndBadData.pdf")
}

pdf('Eo25Values.pdf',height=8.5,width=11)
par(mar=c(8,5,5,5))
plot(EoStuff$Eo25C~EoStuff$time,col=as.factor(EoStuff$deployment),type='p',pch=20,ylab='Eo25 Value',xlab=NA,xaxt='n',main='Eo25 Values with different durafets')
for(i in 1:length(unique(EoStuff$deployment))){
  EoMean=EoStuff$Eo25C[EoStuff$deployment==unique(EoStuff$deployment)[i]]
  if(length(EoMean)==0){next}
  lines(x=range(EoStuff$time[EoStuff$deployment==unique(EoStuff$deployment)[i]]),y=rep(mean(EoMean),2))
}
legend('topleft',legend=as.vector(c('deployment number',unique(EoStuff$deployment))),col=c(NA,as.factor(unique(EoStuff$deployment))),pch=c(NA,rep(20,length(as.factor(unique(EoStuff$deployment))))),bty='n')
axis(1,at=as.POSIXct(substr(EoStuff$time,1,10)),labels=substr(EoStuff$time,1,10),las=2)
dev.off()
#

if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/open?id=1Nw9dp_zpacP18BgGjNpLGHZhs7HUq2vD/view?usp=sharing"),media="Eo25Values.pdf")
}


