# Analyze the UDP data collected in order to search for problems 
# with the reliability of the UDP transmissions

library(tidyverse)
library(ggplot2)

udpData<-read.csv('UDPTiming.csv',sep=';')

ViewstatSummary<-summary(udpData$DeltaTms)
quantileSummary<-quantile(udpData$DeltaTms)
udp5num<-fivenum(udpData$DeltaTms)
udpHist<-hist(udpData$DeltaTms,plot=F,breaks=50)
udpHist<-hist(udpData$DeltaTms,plot=F)

#Summarizing
udpMax<-max(udpData$DeltaTms)
udpMin<-min(udpData$DeltaTms)
dtCount<-sum(udpData$DeltaTms>0)    # how many observations, to see how many were outside of 10 Hz datas
dtLT<-sum(udpData$DeltaTms<99)      # how many observations less than 99 ms
dtGT<-sum(udpData$DeltaTms>101)     # how many observations greater than 101 ms
dtLTPct<-dtLT/dtCount*100           # percentage of total count less than 99 ms
dtGTPct<-dtGT/dtCount*100           # percentage of total count greater than 101 ms

# Cummulative summary
udpCumSum<-cumsum(udpData$DeltaTms)

# Plot of cumm sum
time.q<-quantile(udpData$DeltaTms)
ggplot(NULL, aes(x=udpData$DeltaTms)) +
  geom_step(stat='ecdf',color='green') +
  labs(x='Instance', y='Count') + 
  geom_vline(aes(xintercept=time.q), linetype='dashed',color='darkgreen') + 
  coord_cartesian(xlim=c(99.925,100.075)) + 
  theme_dark()
  
# Full scale look at the data
ggplot(udpData) +
  geom_histogram(aes(x=DeltaTms), binwidth=0.005, color='darkgreen', fill='green') +
  coord_cartesian(xlim=c(99.85,100.15)) + 
  theme_dark()

# Outliers
ggplot(udpData) +
  geom_histogram(aes(x=DeltaTms), binwidth=0.005, color='darkgreen', fill='green') +
  coord_cartesian(xlim=c(99.5,100.5),ylim=c(0,100)) + 
  theme_dark()

