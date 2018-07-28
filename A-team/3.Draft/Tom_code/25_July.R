#### 26 July 2018
#### Thomas Konigkramer

####----------------------set-up--------------------
setwd("E:/Project coding")


####--------------------------data---------------------------
equitiesData=read.csv("Top40_28Jun2018.csv", header= TRUE, sep=";")
head(equitiesData)

# for number of constituents - we know it's 91
# 1st column is dates, then each equity has 3 columns of data 
# (so 274 columns in totals)
nequities = 1 + (ncol(equitiesData)-4)/3

# create date, price, return and volume matrices
dates = equitiesData[1]
price=equitiesData[-c(1,seq(from=3,to=274,by=3),seq(from=4,to=274,by=3))]
totalReturn=equitiesData[-c(1,seq(from=2,to=274,by=3),seq(from=4,to=274,by=3))] 
vol=equitiesData[-c(1,seq(from=2,to=274,by=3),seq(from=3,to=274,by=3))]    

days = nrow(dates) # no. of days we have data for
       