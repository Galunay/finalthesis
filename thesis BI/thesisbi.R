library(tidyverse)
library(readxl)
library(sandwich)
library(stats)
library(forecast)
library(zoo)

#download data, data was performed with eventstudytools

ar1<-read_excel('ar_results.xls')
car1<-read_excel('car_results.xls')
caar1<-read_excel('caar_results.xls')
glimpse(ar1)

#visualisation
ggplot(ar1, aes(y=`AR(0)`, x=`Event ID`))+geom_line()
ggplot(ar1, aes(y=`AR(0)`, x=`Event ID`))+geom_line()