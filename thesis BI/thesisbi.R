library(tidyverse)
library(readxl)
library(sandwich)
library(stats)
library(forecast)
library(zoo)
library(gridExtra)
library(ggpubr)
library(corrplot)
library(mosaic)

#download data, data was performed with eventstudytools

##data finance

setwd("~/Documents/GitHub/finalthesis/thesis BI")
findata<-read_excel('request_ticker_bloomberg.xlsx')
glimpse(findata)
sector<-read_excel('request_ticker_bloomberg.xlsx', sheet=2)
age<-read_excel('request_ticker_bloomberg.xlsx', sheet=3)
sector<-full_join(sector, age, by=c("company"))
findata<-left_join(findata, sector, by=c("Event ID"="Event ID"))


##AR1
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar1/inv/")
inv_ar1<-read_excel('ar_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar1/cred/")
cred_ar1<-read_excel('ar_results.xls')

##`CAR Value`
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar1/inv/")
inv_car1<-read_excel('car_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar1/cred/")
cred_car1<-read_excel('car_results.xls')

#AR2
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar2/inv/")
inv_ar2<-read_excel('ar_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar2/cred/")
cred_ar2<-read_excel('ar_results.xls')

##CAR2
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar2/inv/")
inv_car2<-read_excel('car_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar2/cred/")
cred_car2<-read_excel('car_results.xls')


#AR5
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar5/inv/")
inv_ar5<-read_excel('ar_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar5/cred/")
cred_ar5<-read_excel('ar_results.xls')

##CAR5
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar5/inv/")
inv_car5<-read_excel('car_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar5/cred/")
cred_car5<-read_excel('car_results.xls')

##AR30
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar30/inv/")
inv_ar30<-read_excel('ar_results.xls')
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar30/cred/")
cred_ar30<-read_excel('ar_results-2.xls')

##CAR30
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar30/inv/")
inv_car30<-read_excel('car_results.xls')
inv_caar30<-read_excel('ar_results.xls', sheet = 2)
setwd("~/Documents/GitHub/finalthesis/thesis BI/ar30/cred/")
cred_car30<-read_excel('car_results-2.xls')
cred_caar30<-read_excel('ar_results-2.xls', sheet = 2)

#For models
lmm<-cbind(leverage=c(findata$DERatio), TAssets=c(findata$Asset), EBITDA=c(findata$ebitda), PB=c(findata$PB), ROA=c(findata$ROA),
           beta=c(findata$APPLIED_BETA), MCap=c(findata$MV), 
           age=c(findata$age), EPS=c(findata$eps), BV=c(findata$BV),event=c(findata$`Event ID`))%>%as.data.frame()

m11<-left_join(inv_car1, lmm, by=c("Event ID"="event"))%>%na.omit()
m11$sector<-as.factor(m11$sector)
glimpse(m11)

c11<-left_join(cred_car1, findata, by=c("Event ID"="event"))
c11$sector<-as.factor(c11$sector)
glimpse(c11)


#Visualisation

##line
ggplot(inv_caar30, aes(y=caar, x= `Event ID`+2))+geom_line()+labs(x="Day from Event", y="", title = "CAAR")
ggplot(cred_caar30[10:40,], aes(y=caar, x= date+6))+geom_line()+labs(x="Day from Event", y="", title = "CAAR")

##density
i1<-ggplot(inv_car1, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='red')+labs(x="CAR 1", y="") + theme(legend.position = " ")  
i2<-ggplot(inv_car2, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='green')+labs(x="CAR 2", y="") + theme(legend.position = " ")
i5<-ggplot(inv_car5, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='blue')+labs(x="CAR 5", y="") + theme(legend.position = " ")
i30<-ggplot(inv_car30, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='yellow')+labs(x="CAR 30", y="")+theme(legend.position = "bottom")
ggarrange(i1, i2, i5, i30, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

c1<-ggplot(cred_car1, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='red')+labs(x="CAR 1", y="") + theme(legend.position = " ")  
c2<-ggplot(cred_car2, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='green')+labs(x="CAR 2", y="") + theme(legend.position = " ")
c5<-ggplot(cred_car5, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='blue')+labs(x="CAR 5", y="") + theme(legend.position = " ")
c30<-ggplot(cred_car30, aes(x=`CAR Value`, col= sector))+geom_density(na.rm = T, alpha=0.1, fill='yellow')+labs(x="CAR 30", y="")+theme(legend.position = "bottom")
ggarrange(c1, c2, c5, c30, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

##Corrplot
lm<-lm%>%na.omit
corrplot(cor(lm), method = "number", order = "alphabet")
round(cor(lm), digits = 3)

#Interconnections plot
des1<-qplot(data=m11, leverage, `CAR Value`)+facet_wrap(~sector)
des2<-qplot(data=m11, log(TAssets), `CAR Value`)+facet_wrap(~sector)
des3<-qplot(data=m11, EBITDA, `CAR Value`)+facet_wrap(~sector)
des4<-qplot(data=m11, PB, `CAR Value`)+facet_wrap(~sector)
des5<-qplot(data=m11, beta, `CAR Value`)+facet_wrap(~sector)
des6 <- qplot(data=m11, EPS, `CAR Value`)+facet_wrap(~sector)
des7 <- qplot(data=m11, MCap, `CAR Value`)+facet_wrap(~sector)
des8 <- qplot(data=m11, age, `CAR Value`)+facet_wrap(~sector)
des9 <- qplot(data=m11, BV, `CAR Value`)+facet_wrap(~sector)
grid.arrange(des1, des2, des3, des4, des5, des6, des7, des8, des9)

des1<-qplot(data=c11, leverage, `CAR Value`)+facet_wrap(~sector)
des2<-qplot(data=c11, log(TAssets), `CAR Value`)+facet_wrap(~sector)
#des3<-qplot(data=c11, EBITDA, `CAR Value`)+facet_wrap(~sector)
des4<-qplot(data=c11, PB, `CAR Value`)+facet_wrap(~sector)
des5<-qplot(data=c11, beta, `CAR Value`)+facet_wrap(~sector)
des6 <- qplot(data=c11, EPS, `CAR Value`)+facet_wrap(~sector)
des7 <- qplot(data=c11, MCap, `CAR Value`)+facet_wrap(~sector)
des8 <- qplot(data=c11, age, `CAR Value`)+facet_wrap(~sector)
des9 <- qplot(data=c11, BV, `CAR Value`)+facet_wrap(~sector)
grid.arrange(des1, des2, des4, des5, des6, des7, des8, des9)

#casuality

cas<-right_join(inv_caar30, cred_caar30, by=c("`Event ID"="date"))

plot(inv_caar30$`Event ID`,inv_caar30$caar,type="l",col="red", xlab = "date", ylab="caar")
par(new=TRUE)
plot(cred_caar30$date,cred_caar30$caar,type="l",col="blue", xlab = "date", ylab="caar")

#Models
#My Model
m1<-left_join(inv_car1, findata, by=c("Event ID"="Event ID"))
glimpse(m1)
#m1$sector.y<-as.factor(m1$sector.y)
#m1$sector.y<-factor(m1$sector.y, levels = c("asset management","banking","insurance"))

md1<-lm(`CAR Value` ~ (log(ebitda) + eps + ROA + log(BV) + 
                         log(leverage) + APPLIED_BETA):sector.y, data = m1)
summary(md1)

#Model classic
md_inv11<-lm(`CAR Value`~ (age+beta+EBITDA+leverage+MCap+PB+ROA+TAssets):sector, data=m11)
summary(md11)

glimpse(c11)
md_cr11<-lm(`CAR Value`~ (age+beta+leverage+MCap+PB+ROA), data=c11)
summary(md_cr11)

#`CAR Value` ~ (log(ebitda) + eps + ROA + log(BV) + log(leverage) + APPLIED_BETA):sector.y, data = m1)






