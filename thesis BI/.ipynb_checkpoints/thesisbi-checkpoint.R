library(tidyverse)
library(readxl)
library(sandwich)
library(stats)
library(forecast)
library(zoo)
library(gridExtra)
library(ggpubr)

#download data, data was performed with eventstudytools

##data finance

setwd("~/Documents/GitHub/finalthesis/thesis BI")
findata<-read_csv('request_ticker_diffs.csv')

##AR
ar1<-read_excel('ar_results.xls')
ar2<-read_excel('ar_results.xls')
ar30<-read_excel("ar_results.xls", sheet = 2)


##CAR
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar1")
car1<-read_excel('car_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar2")
car2 <-read_excel('car_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar5")
car5 <- read_excel('car_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar30")
car30<-read_excel('car_results.xls')

#Visualisation

##line
ggplot(ar30, aes(y=caar, x= `Event ID`+2))+geom_line()+labs(x="Day from Event", y="", title = "CAAR")
 
##density
c1<-ggplot(car1, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='red')+labs(x="CAR 1", y="") + theme(legend.position = " ")  
c2<-ggplot(car2, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='green')+labs(x="CAR 2", y="") + theme(legend.position = " ")
c5<-ggplot(car5, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='blue')+labs(x="CAR 5", y="") + theme(legend.position = " ")
c30<-ggplot(car30, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='yellow')+labs(x="CAR 30", y="")+theme(legend.position = "bottom")

ggarrange(c1, c2, c5, c30, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

#Models

m1<-left_join(car1, findata, by=c("Event ID"="Event ID"))

md1<-lm(`CAR Value` ~ (log(`total debt`)+ log(ebitda)+eps+ROA+log(BV)), data=m1)
summary(md1)





