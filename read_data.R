##########################################################
#
# HarvadX: Capstone CYO Project, Bank Marketing Dataset
#
# This file is the R code used for the analysis
#
##########################################################

# The data file has been downloaded from Kaggle website and uploaded to the GitHub page to download
# Kaggle link: https://www.kaggle.com/janiobachmann/bank-marketing-dataset
# GitHub Repsitory: https://github.com/adeepikaa/bankdeposit

# Download data from GitHub and load the CSV file as a dataframe

fileurl<-"https://raw.githubusercontent.com/adeepikaa/bankdeposit/master/bankdeposit.csv"
download.file(fileurl, "bank_data.csv")

bank_data<-read.csv("bank_data.csv")

# Installing all required packages and libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(gridExtra)
library(caret)
library(knitr)
library(randomForest)
library(MLmetrics)
library(ROCR)
library(fastAdaboost)

######################################################################
#   Understand Data
######################################################################

nrow(bank_data)
ncol(bank_data)
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


#Check for missing data
sum(is.na(bank_data))

#Check for duplicates
nrow(unique(bank_data))


summary(bank_data)


unique(bank_data$job)
unique(bank_data$month)
unique(bank_data$poutcome)

unique(bank_data$previous)
unique(bank_data$campaign)
       
# Checking for range of variables
table(bank_data$deposit)

table(bank_data$campaign)

table(bank_data$pdays)

table(bank_data$previous)

