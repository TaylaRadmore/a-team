#### 24 July 2018
#### Tayla Radmore

####---------------------------------------------------set-up------------------------------------------#####
setwd("~/GitHub/a-team/A-team/3.Draft")
library(optimx)


####---------------------------------------------------data-------------------------------------------######
share_data=read.csv("Top40_28Jun2018.csv", header= TRUE, sep=";")
head(share_data)
length=nrow(share_data)
in_sample_length=0.8*length
out_sample_start=in_sample_length+1

in_sample=share_data[1:in_sample_length,]
out_sample=share_data[out_sample_start:length,]


####---------------------------------------------minimax------------------------------------------##########
sequence=seq(from = 2, to = length(in_sample), by = 3)
minimax_data=in_sample[,sequence]

start=1/40
starting_values <- rep(start, 40)

x=(minimax_data[1,])*matrix(starting_values)


t(minimax_data[1,])*starting_values

optimx(starting_values, "put your function in", lower=0, upper=1,
       method=c("Nelder-Mead","BFGS"))
       
       
       
       
       