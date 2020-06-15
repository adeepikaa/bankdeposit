

fileurl<-"https://raw.githubusercontent.com/adeepikaa/bankdeposit/master/bankdeposit.csv"
download.file(fileurl, "bank_data.csv")

bank_data<-read.csv("bank_data.csv")

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(gridExtra)
library(caret)
library(knitr)
library(gam)
library(randomForest)
library(MLmetrics)

######################################################################
#   Understand Data
######################################################################

names(bank_data)

# Data Dictionary:

#  "age"       : Age of the person 18-95
#  "job"       : Job, 22 types
#  "marital"   : Married, Single, Divorced
#  "education" : primary, secondary, tertiary, unknown
#  "default"   : has credit in default? yes/no
#  "balance"   : bank balance
#  "housing"   : has housing loan? yes/no
#  "loan"      : has personal load? yes/no
#  "contact"   : cell, telephone, unknown
#  "day"       : days of the month
#  "month"     : 12 months
#  "duration"  : duration of call, not to be used for predictions
#  "campaign"  : no. of contacts made to this person during this campaign
#  "pdays"     : number of days passed since last contact in previous campaign
#  "previous"  : number of contacts made before this campaign
#  "poutcome"  : outcome of previous marketing campaign
#  "deposit"   : Term Deposited, yes/no

str(bank_data)

summary(bank_data)


sum(is.na(bank_data))

unique(bank_data$job)
unique(bank_data$month)
unique(bank_data$poutcome)

unique(bank_data$previous)
unique(bank_data$campaign)
       

