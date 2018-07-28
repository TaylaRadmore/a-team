#### 3 July 2018
#### Tayla Radmore

####---------------------------------------------------set-up------------------------------------------#####
setwd("~/GitHub/a-team/A-team/3.Draft")
### library(optimx)
 ### library(naniar)   ### installed better when you choose the no option ---->>> not currently in use
library(dplyr)


####---------------------------------------------------data-------------------------------------------######
share_data=read.csv("Top40_28Jun2018_no_orange.csv", header= TRUE, sep=";")
summary(share_data)
str(share_data)
View(share_data)
#### there is NA data that isnt NA
#### the values are reading as factors not numbers

cols=ncol(share_data)
cols
(cols-1)/3

for (i in 1:cols)
{
  share_data[,i] <- recode_factor(share_data[,i], "#N/A N/A" = NA_character_)
}

View(share_data)
summary(share_data)
str(share_data) #### fixed NA



for (i in 2:cols) ### 2 because first column are dates
{
  x= share_data[,i]
  share_data[,i] <- as.numeric(as.character(sub("," , ".", x)))
}

View(share_data)
summary(share_data)
str(share_data)
#### factors to numbers, keeping the decimals



share_data[,1] <- as.Date(share_data[,1])
str(share_data)   #### coding dates as dates --> not sure I'll actually need this but just incase



numb_dif_Share=(ncol(share_data)-1)/3
numb_dif_Share   #### seeing how many shares there are
set.seed(2018)
select_shares = sample(0:(numb_dif_Share-1), 40)
select_shares

sequence_shares = c(1,(select_shares*3+2), (select_shares*3+3), (select_shares*3+4))
sequence_shares <- sort(sequence_shares, decreasing = FALSE)
sequence_shares

share_data_final <- share_data[,sequence_shares]
View(share_data_final)

share_data <- share_data_final #####!!!!!!!

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

####--------------------------------------------ignore----------------------------------------####
share_data_2 = mutate_all(share_data, function(x) as.numeric(as.character(x)))
View(share_data_2)

share_data_3 <- data.frame(sapply(share_data, function(x) as.numeric(as.character(x))))
View(share_data_3)

share_data_4 <- data.frame(apply(share_data, 2, function(x) as.numeric(as.character(x))))
View(share_data_4)

share_data_5 <- lapply(share_data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
View(share_data_5)

as.numeric(as.character(share_data$FSR.Last.Price))
as.double(as.character(share_data$FSR.Last.Price))

#### share_data_1 = data.matrix(share_data)
#### View(share_data_1) #### drops decimals
#### summary(share_data_1)   #### makes it numbers
#### str(share_data_1)
#### share_data_1[5,29]


### attempt <- as.numeric(as.character(sub("," , ".", share_data$Date)))
### summary(attempt)
### str(attempt)

### share_data <- share_data_1   #######!!!!!!!!!!!


####-----------------------------------------Base case-----------------------------------------------#####

sequence_1=seq(from = 3, to = ncol(in_sample), by = 3)
sequence_1
var_data=in_sample[,sequence_1]
View(var_data)
summary(var_data)

number_of_shares=ncol(var_data)
number_of_shares

number_obs = nrow(var_data)
number_obs



covarience_vector <- NULL
covarience_matrix <- NULL

for (i in 1:number_of_shares)
{
  
  for (j in 1:number_of_shares)
  {
    covariance <- cov(var_data[,i],var_data[,j],  use = "na.or.complete")
    covarience_vector <- c(covarience_vector, covariance)
  }
  covarience_matrix <- cbind (covarience_matrix, covarience_vector)
  covarience_vector <- NULL
}

View(covarience_matrix)
cov(var_data[,1],var_data[,2])
covarience_matrix[1,2]
covarience_matrix[1,1]   #### matches excel
covarience_matrix[2,2]   #### ditto 
covarience_matrix[3,3]   #### ditto



start=1/number_of_shares
starting_values <- rep(start, number_of_shares)
starting_values


starting_values%*%covarience_matrix%*%starting_values

mvp = optim(starting_values,
                function(starting_values) starting_values%*%covarience_matrix%*%starting_values)
mvp
mvp$value
starting_values%*%covarience_matrix%*%starting_values
mvp$par%*%covarience_matrix%*%mvp$par





####----------------------------------ignore---------------------------------------------------------###########

sum(t(variance_vector)* starting_values^2, na.rm=TRUE)


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


function_to_optimise <- function( weight)
{
  for (i in 1:number_of_shares)
  { for (j in 1:number_of_shares)
  {
    sum( covarience_matrix[i,j] * weight[i] * weight[j], na.rm = TRUE)
  }
  }
}



#### from here on doesn't work
optim( starting_values, function_to_optimise(starting_values))

optimx(starting_values, function_to_optimise(starting_values), 
       lower=rep(0, number_of_shares), upper=rep(1, number_of_shares),
       method=c("Nelder-Mead","BFGS"))


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



#### variance_vector = NULL
#### for (i in 1:number_of_shares)
#### {
####   new_variance=var(var_data[,i],na.rm=TRUE)
####   variance_vector <- c(variance_vector, new_variance )
#### }

#### variance_vector

#### var(var_data[,2])  #### quick tests
#### var(var_data[,39]) 

#### summary(var_data[,39])

#### summary(variance_vector)


####---------------------------------------------minimax------------------------------------------##########

mini_vector <- NULL
for (i in 1:number_of_shares)
{
  mini <- min(var_data[,i], na.rm = TRUE)
  mini_vector <- c(mini_vector, mini)
}

mini_vector

mini_vector[1]  
mini_vector[2]
mini_vector[3]
#### all match excel

start=1/number_of_shares
starting_values <- rep(start, number_of_shares)

mini_vector%*%starting_values

max_mini = optim(starting_values,function(starting_values) -1*mini_vector%*%starting_values)
mini_vector%*%max_mini$par  #### seems legit


#####--------------------------------------Ignore---------------------------------------------####
var_data[,2]
mini <- min(var_data[,1], na.rm = TRUE)
mini
str(var_data)

#### Doesn't work
x=(minimax_data[1,])*matrix(starting_values)


t(minimax_data[1,])*starting_values

optimx(starting_values, "put your function in", lower=0, upper=1,
       method=c("Nelder-Mead","BFGS"))

       
####----------------------------------------WES-----------------------------------------------####      
       