library(plyr)
library(quantmod)
library(reshape)
library (dplyr)
library (ggplot2)
df<- read.csv ("RBI.csv",header = T)
data <- tbl_df(df)
data

replacewithMean <- function (df) {
  
  df1<- df
 
  df1[]<-lapply(seq_len(ncol(df)),function(i) {x<-df[,i]; x[is.na(x)]<- 0;x}) 
    return (df1)
  
}

data<- tbl_df(replacewithMean(df))

d1<- data%>% select(Year,Months_words,Number_of_POS_On.line)%>% group_by (Year,Months_words)%>% summarise(sum=sum(Number_of_POS_On.line))%>%group_by(Year)%>% summarise(mean=mean (sum),sum=sum(sum),n=n())
library(tidyr)

d2<- d1 %>% mutate(Total_POS=sum/1000000)
longley.o <- arrange(d1,Year)

apply(longley.o,2,function(x){c(NA,diff(x))})
apply(longley.o,2,Delt)*100


gather (d1,"Year","n",2:3)
d2
longley.m <- melt(d1,id="Year")

g<- ggplot(d2,aes(x=Year,y=Total_POS))+geom_line()

library(plyr)
library(quantmod)
library(reshape)



