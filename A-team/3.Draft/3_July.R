#### 3 July 2018
#### Tayla Radmore

####---------------------------------------------------set-up------------------------------------------#####
setwd("~/GitHub/a-team/A-team/3.Draft")
library(optimx)
 ### library(naniar)   ### installed better when you choose the no option ---->>> not currently in use
library(dplyr)


####---------------------------------------------------data-------------------------------------------######
share_data=read.csv("Top40_28Jun2018.csv", header= TRUE, sep=";")
summary(share_data)
str(share_data)
View(share_data)


cols=ncol(share_data)
cols

for (i in 1:cols)
{
  share_data[,i] <- recode_factor(share_data[,i], "#N/A N/A" = NA_character_)
}

View(share_data)
summary(share_data)
str(share_data)


share_data_1 = data.matrix(share_data)
View(share_data_1) #### drops decimals
summary(share_data_1)   #### makes it numbers
str(share_data_1)


share_data <- share_data_1   #######!!!!!!!!!!!

length=nrow(share_data)
length
in_sample_length=0.8*length
in_sample_length
in_sample_length <- floor(in_sample_length)
in_sample_length
out_sample_start=in_sample_length+1
out_sample_start

in_sample=share_data[1:in_sample_length,]
View(in_sample)
out_sample=share_data[out_sample_start:length,]
View(out_sample)

####-----------------------------------------Base case-----------------------------------------------#####

sequence_1=seq(from = 3, to = ncol(in_sample), by = 3)
sequence_1
var_data=in_sample[,sequence_1]
View(var_data)
summary(var_data)

number_of_shares=ncol(var_data)
number_of_shares

variance_vector = NULL
for (i in 1:91)
{
  new_variance=var(var_data[,i],na.rm=TRUE)
  variance_vector <- c(variance_vector, new_variance )
}

variance_vector

var(var_data[,2])  #### quick tests
var(var_data[,45]) 
  
summary(var_data[,45])


summary(variance_vector)


start=1/40
starting_values <- rep(start, number_of_shares)
starting_values

sum(t(variance_vector)* starting_values^2, na.rm=TRUE)




#### from here on doesn't work
optimx(starting_values, t(variance_vector)* starting_values, 
       lower=rep(0, number_of_shares), upper=rep(1, number_of_shares),
       method=c("Nelder-Mead","BFGS"))


starting_values_1 <- vector(mode="numeric", length=number_of_shares)
starting_values_1
function_to_optimise <- function(variance_vector, starting_values_1)
{
  sum(t(variance_vector)* starting_values_1)
}

optimx(starting_values_1, function_to_optimise, 
       lower=rep(0, number_of_shares), upper=rep(1, number_of_shares),
       method=c("Nelder-Mead","BFGS"))

####----------------------------------ignore---------------------------------------------------------###########

variance = function (returns_vector)
{
  number_obs = ncol(returns_vector)
  
}


FSR_return <- as.numeric(in_sample$FSR.Total.Return.Index..Gross.Dividends.) 
summary(FSR_return)
number_obs = NROW(FSR_return)
number_obs
number_obs-in_sample_length
sum(FSR_return)
average = sum(FSR_return)/number_obs
average 
min(FSR_return)


mean(FSR_return)
var(FSR_return) ##### 937680

## need to make these numbers not factors
### var_data_1 <- data.frame(var_data, rownames.force = NA)
### View(var_data_1)
### summary(var_data_1)

var_data_3 <- sub("#N/A N/A",NA,var_data)
View(var_data_3)


share_data_3 = data.frame(share_data)
View(share_data_3)  #### keeps decimals
summary(share_data_3)   ### still factors though
str(share_data_3)

####---------------------------------------------minimax------------------------------------------##########
sequence=seq(from = 2, to = length(in_sample), by = 3)
minimax_data=in_sample[,sequence]








start=1/40
starting_values <- rep(start, 40)

x=(minimax_data[1,])*matrix(starting_values)


t(minimax_data[1,])*starting_values

optimx(starting_values, "put your function in", lower=0, upper=1,
       method=c("Nelder-Mead","BFGS"))
       
       
       
       
       