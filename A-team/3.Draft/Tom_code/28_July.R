#### 26 July 2018
#### Thomas Konigkramer

####----------------------set-up--------------------
setwd("E:/Project coding")
library(tseries)
library(dplyr)
library(quadprog)
library(NMOF)
library(limSolve)
library(cvar)
library(fPortfolio)
library(extrafont)

####--------------------------data---------------------------
equitiesData=read.csv("Top40_28Jun2018.csv", header= TRUE, sep=",")
head(equitiesData)

equitiesList=read.csv("TOP40_ConstituentsList.csv",header=TRUE,sep=",")
head(equitiesList)
equitiesNames = equitiesList[,1]

# for number of constituents - we know it's 91
# 1st column is dates, then each equity has 3 columns of data 
# (so 274 columns in totals)
nequities = 1 + (ncol(equitiesData)-4)/3

# create date, price, totalReturn (for dividends) and volume matrices
dates = equitiesData[1]
nsort = ncol(equitiesData) # to sort the data into seperate vectors
price=equitiesData[-c(1,seq(from=3,to=nsort,by=3),seq(from=4,to=nsort,by=3))]
totalReturn=equitiesData[-c(1,seq(from=2,to=nsort,by=3),seq(from=4,to=nsort,by=3))] # most important vector here
vol=equitiesData[-c(1,seq(from=2,to=nsort,by=3),seq(from=3,to=nsort,by=3))]    

days = nrow(dates) # no. of days we have data for

##won't be needing this
##dailyReturn=matrix(0,nrow=days,ncol=nequities)
##for (j in 1:nequities){
##  for (i in 1:days){
##    dailyReturn[i,j]=price[i+1,j]/price[i,j]-1
##  }
##}
#rename column names after equities
##colnames(dailyReturn)=equitiesNames
##dailyReturn

returnSample = totalReturn
returnSample[returnSample=="#N/A N/A"]=NA_character_

returnSample = as.matrix(as.data.frame(lapply(returnSample, as.numeric)))
str(returnSample)
returnSample=returnSample[ , apply(returnSample, 2, function(x) !any(is.na(x)))]

# renaming the columns to equities tickers (first three letters of tickers)
for (i in 1:ncol(returnSample)){
  colnames(returnSample)[i]=substr(colnames(returnSample)[i],1,3)
}
colnames(returnSample)

returnSample = as.timeSeries(returnSample)

#returnSample=returnSample[sapply(returnSample, function(returnSample) !any(is.na(returnSample)))] 
#str(returnSample)

####--------------------------mean-variance portfolio---------------------------

mvPortfolio = minvariancePortfolio(returnSample)
mvWeights = getWeights(mvPortfolio)
mvWeights=mvWeights[mvWeights>0] 
par(family="serif") # font to times new roman
barplot(mvWeights, main="Mean-Variance Portfolio Weights")


####-----------------------weighted expected shortfall-------------------

wes=function(alpha, xdata,lambda,investment){
  xalpha=quantile(xdata,alpha)
  step1 = mean(xdata[xdata<=xalpha])
  step1
}

wes(0.05,returnSample)


####----------------------------mini-max---------------------------------

minimax -> function(x_var){
  
}
       