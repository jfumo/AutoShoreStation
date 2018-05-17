rm(list=ls())


SaveToDrive=T

TrisCalTimes=data.frame(time=c("2/2/18 13:37:41","3/1/18 8:53:16","4/3/18 11:50:36"))
  




setwd("~/Desktop/stuff/Routine Scripts/Get Converted pH Data")
library(ncdf4)
rawData=data.frame(time=as.POSIXct(ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),"time"),origin='1970-01-01 00:00:00'),ph_counts=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph_counts'),ph_voltage=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph_voltage'),ph_raw=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph_raw'),temp_counts=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'temp_counts'),thermistor_voltage=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'thermistor_voltage'),thermistor_raw=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'thermistor_raw'),temperature=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'temperature'),ph=ncvar_get(nc_open("http://sccoos.org/thredds/dodsC/autoss/005_newport_pier-2018.nc"),'ph'))
rawData[,2:9]=lapply(rawData[,2:9],as.numeric)
#-------------------------------------------------------------------------------------------------------------
#read in google sheets coef and re-calculate ph voltage. 
library(googledrive)
file=drive_download(drive_get("SASS Inventory and Cleaning",id="1099lNMJ3XZQIFv7oSr0Q-5i4fihx7gnvoxMaVhiUhJE")[1,],type='xlsx',overwrite=T)
library(xlsx)
Coef=read.xlsx("SASS Inventory and Cleaning.xlsx",sheetName="pH_NBPier_Coef")
rawData$pH_slope=NaN
rawData$pH_intercept=NaN
rawData$temp_slope=NaN
rawData$temp_intercept=NaN
rawData$E0=NaN
rawData$Ts=NaN
rawData$Deployment=NaN
Coef=Coef[is.na(Coef$start_time)==F,]
Coef$start_time=as.POSIXct(strptime(as.character(Coef$start_time),'%Y-%m-%d %H:%M:%S'))
for(i in 1:length(Coef$start_time)){
  rawData$pH_slope[rawData$time>=Coef$start_time[i]]=Coef$pH_slope[i]
  rawData$pH_intercept[rawData$time>=Coef$start_time[i]]=Coef$pH_intercept[i]
  rawData$temp_slope[rawData$time>=Coef$start_time[i]]=Coef$temp_slope[i]
  rawData$temp_intercept[rawData$time>=Coef$start_time[i]]=Coef$temp_intercept[i]
  rawData$E0[rawData$time>=Coef$start_time[i]]=Coef$E0[i]
  rawData$Ts[rawData$time>=Coef$start_time[i]]=Coef$Ts[i]
  rawData$Deployment=i
}


rawData$ph_voltage=(rawData$ph_counts*rawData$pH_slope)+rawData$pH_intercept
rawData$temperature=(rawData$temp_counts*rawData$temp_slope)+rawData$pH_intercept
rawData$TK=rawData$temperature+273.15

#Also use voltage to calculate pH (non-TRIS corrected)
R=Coef$R[1]
Faraday=Coef$F[1]
rawData$ph=(rawData$ph_voltage-rawData$E0-0.001*(rawData$TK-rawData$Ts))/(R*rawData$TK*log(10)/Faraday)



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

#exptrapolate the Eo25C value from the TRIS cal to the all the Eo25C values within each respective deployment
##'deployments' are considered the length of time from one sensor deployment to the next (new coef)
###so all Eo25C values from 1 deployment are averaged and extrapolated across the whole deployment
for(i in 1:length(Coef$start_time)){
  rawData$Eo25C[rawData$Deployment==i]=mean(rawData$Eo25C[is.na(rawData$Eo25C[rawData$Deployment==i])==F])
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


#-------------------------------------------------------------------------------------------------------------
name='TRIScorrectedData.csv'
#plot up the data and save as a .pdf
pdf('TRIScorrectedData.pdf',height=8.5,width=11)
plot(x=range(rawData$time),y=range(c(rawData$pHint,rawData$ph)),type='n',main="TRIS Corrected pH data",ylab='pH',xlab='Time')
lines(rawData$ph~rawData$time,col='red',type='l')
lines(rawData$pHint~rawData$time,col='blue',type='l')
legend('bottomleft',legend=as.vector(c('pH from MBARI Coef','TRIS corrected pH')),col=c('red','blue'),lty=c(1,1),pch=c(NA,NA),bty='n',lwd=2)
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
dev.off()

df=data.frame(time=rawData$time,temperature=rawData$temperature,pH=rawData$pHint)



write.csv(df,name)

#And save that workbook to google drive
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1Fu8sXejMbbj92Rie52B6ajtT4VlPekF3/view?usp=sharing"),media=paste("~/Desktop/stuff/Routine Scripts/Get Converted pH Data/",name,sep=''))
}

#And update the plot
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1y_lmprMbP8rqH7nrXsO9sxd0-KaSdJV3/view?usp=sharing"),media="~/Desktop/stuff/Routine Scripts/Get Converted pH Data/TRIScorrectedData.pdf")
}

#plot just the TRIS data
pdf('TRIScorrectedPlot.pdf',height=8.5,width=11)
plot(rawData$pHint~rawData$time,type='l',col='blue',main="NB Pier pH Data",ylab='pH',xlab='Time')
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
dev.off()


#and put that on the server too
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/19G8OhfYsxuFZrTAIeZTEW6A2S6v5Nmo4/view?usp=sharing"),media="~/Desktop/stuff/Routine Scripts/Get Converted pH Data/TRIScorrectedPlot.pdf")
}

#And plot the last 48 hours too
pdf("Last48Hours.pdf",width=11,height=8.5)
plot(pHint~time,data=rawData[rawData$time>=tail(rawData$time,1)-(3600*48),],type='l',col='blue',main="NB Pier pH Data- Last 48 Hours",ylab="pH",xlab="Time",xaxt="n")
axis(1,at=as.POSIXct(strptime(paste(unique(substr(rawData$time[rawData$time>=tail(rawData$time,1)-(3600*48)],1,10)),'12:00:00'),'%Y-%m-%d %H:%M:%S')),labels= as.POSIXct(strptime(paste(unique(substr(rawData$time[rawData$time>=tail(rawData$time,1)-(3600*48)],1,10)),'12:00:00'),'%Y-%m-%d %H:%M:%S')))
legend('bottomright',legend=as.vector(c('SCCOOS Automated Shore Stations',"http://sccoos.org/data/autoss/")),text.col=c('black','blue'),bty='n')
dev.off()

#and put that on the server too
if(SaveToDrive==T){
  drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1TUGqf2aC8Ywv3JCPQB5IjPalJGYHteD9/view?usp=sharing"),media="~/Desktop/stuff/Routine Scripts/Get Converted pH Data/Last48Hours.pdf")
}



quit(save="no")
