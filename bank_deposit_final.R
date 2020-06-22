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



#############################################
#  Data Exploration
#############################################


# Exploration of the variable "Age"

median(bank_data$age)

min(bank_data$age)

max(bank_data$age)

dist<-bank_data%>%
  group_by(age)%>%
  summarize(n=n(), .groups='drop')%>%
  ggplot(aes(x=age, y=n))+
  geom_line()+
  xlab("Age")+
  ylab("Total number of customers")+
  ggtitle(" Total Distribution of Age")

dist_dep<-bank_data%>%
  group_by(age)%>%
  summarize(n=n(), deposit_pct=sum(deposit=="yes")/n, .groups="drop")%>%
  ggplot(aes(x=age, y=deposit_pct))+
  geom_bar(stat="identity")+
  xlab("Age")+
  ylab("Percent of customers deposited")+
  ggtitle(" Deposit percentage across Age")

grid.arrange(dist, dist_dep, ncol=1)

###########################################

# Explore Education and Marital Status 

e<-bank_data%>%
  ggplot(aes(x=education,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution")+
  xlab("Education")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

m<-bank_data%>%
  ggplot(aes(x=marital,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution")+
  xlab("Marital Status")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

grid.arrange(e,m, nrow=1)

###########################################

# Explore Job

bank_data%>%
  ggplot(aes(x=job,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with Job")+
  xlab("Job")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
bank_data%>%
  ggplot()+
  geom_boxplot(aes(job, age))+
  ggtitle("Distribution of Jobs across Age")+
  xlab("Job")+
  ylab("Age")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

###########################################

# Explore house loan, personal loan and default

h<-bank_data%>%
  ggplot(aes(x=housing,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  xlab("House Loans")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

l<-bank_data%>%
  ggplot(aes(x=loan,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  xlab("Personal Loans")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
d<-bank_data%>%
  ggplot(aes(x=default,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  xlab("Credit Default")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
grid.arrange(h,l,d, nrow=1)

###########################################
# Explore contact type

bank_data%>%
  ggplot(aes(x=contact,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with different types of contact")+
  xlab("Type of Contact")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
###########################################

# Explore this campaign contacts

mon<-bank_data%>%
  ggplot(aes(x=month,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Last contact month")+
  xlab("Months")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
day<-bank_data%>%
  ggplot(aes(x=day,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Last contact day")+
  xlab("Days")+
  ylab("Total number of customers")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
grid.arrange(mon, day, nrow=1)

bank_data%>%
  ggplot(aes(x=campaign,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  xlab("Number of contacts")+
  ylab("Total number of customers")+
  ggtitle("Number of contacts for current campaign")

###########################################
# Explore previous campaign contacts

bank_data%>%
  ggplot(aes(x=poutcome,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  xlab("Previous Campaign Outcome")+
  ylab("Total number of customers")+
  ggtitle(" Distribution of Deposits with previous outcomes")


bank_data%>%
  ggplot(aes(x=pdays,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Days since last contact")+
  xlab("Days since last contact")+
  ylab("Total number of customers")+
  xlim(c(0,400))

bank_data%>%
  ggplot(aes(x=previous,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  xlab("Number of contacts")+
  ylab("Total number of customers")+
  ggtitle(" Contacts before this campaign")




###########################################
# Explore bank balance
###########################################

median(bank_data$balance)

min(bank_data$balance)

max(bank_data$balance)

bal1<-bank_data%>%
  ggplot(aes(x=deposit, y=balance, col=deposit))+
  geom_boxplot()+
  ggtitle("Distribution of Bank balance")

bal2<-bank_data%>%
  ggplot(aes(x=deposit, y=balance, col=deposit))+
  geom_boxplot()+
  ggtitle("Balance between 0 & 2500")+
  ylim(0, 2500)

grid.arrange(bal1, bal2, nrow=1)

###################################
# Balance with other predictors.

bank_data%>%
  ggplot(aes(x=age, y=balance))+
  geom_point()+
  ggtitle(" Bank Balance for different ages")

bank_data%>%
  ggplot(aes(x=job,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance for different jobs")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

b1<-bank_data%>%
  ggplot(aes(x=education,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance Vs Education")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

b2<-bank_data%>%
  ggplot(aes(x=marital,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance Vs Marital Status")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
grid.arrange(b1, b2, nrow=1)

h_b<-bank_data%>%
  ggplot(aes(x=housing,  y=balance))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

l_b<-bank_data%>%
  ggplot(aes(x=loan,  y=balance))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
d_b<-bank_data%>%
  ggplot(aes(x=default,  y=balance))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

grid.arrange(h_b, l_b, d_b, nrow=1)



# Correlation of numeric predictors

cortable<-cor(bank_data[,c("age", "balance", "duration", "campaign", "pdays", "previous")])
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cortable, col=col, symm=TRUE)

# Corelation with duration as it is hevaily related to deposit column
cor(bank_data[,c("previous", "campaign","pdays", "age", "balance")], bank_data$duration)

################################################
#   Model Analysis and Results
################################################

# This code has all model analysis and results

# xvalues<-c("duration", "pdays")
# Remove the duration and pdays columns.
# removing pdays as more than half of the customers were not contacted before
# removing duration as it is heaily correlated to output and will be more 
# only after deciding to make deposit

bank_data_ml<-bank_data[, -c(12, 14)]

# Splitting data into train, test and eval sets with 80% 20% ratio
set.seed(123, sample.kind="Rounding")

# use 20% as evalset which will be used on final model to report performance 
eval_index <- createDataPartition(bank_data_ml$deposit, times = 1, p = 0.2, list = FALSE)
temp <- bank_data_ml[-eval_index,]
evalset <- bank_data_ml[eval_index,]


set.seed(123, sample.kind="Rounding")

# use above obtained 80% of data to make further splits to get trainset and testset 
# with same ratio of 80-20 split
test_index <- createDataPartition(temp$deposit, times = 1, p = 0.2, list = FALSE)
trainset <- temp [-test_index,]
testset <- temp [test_index,]


##################################################################################

# Function to create classification algorithem metrics
# measures confusion matrix, precision, recall and F score

get_result_stats<-function(x,y){
  cm<-table(Predict=x, Reference=y)
  acc<-(cm[1,1]+cm[2,2])/sum(cm)
  precision<-cm[2,2]/(cm[1,2]+cm[2,2])
  recall<-cm[2,2]/(cm[2,1]+cm[2,2])
  f1score<-2*precision*recall/(precision+recall)
  
  list(cm=cm, acc=acc, precision=precision, recall=recall, f1score=f1score)
}

##################################################################################
# Cross-validation with k-fold=5

ctrl <- trainControl(method = "cv",
                     number = 5,
                     allowParallel = TRUE)

##################################################################################

# KNN model 

# use caret package, with cross-validation and fine tune for k
set.seed(123, sample.kind="Rounding")
model_knn<-  train(deposit~., data=trainset, method = "knn", 
                   trControl = ctrl, tuneGrid = data.frame(k = seq(3, 50, 2)))

# best value of k
model_knn$bestTune
ggplot(model_knn)

# predict values for testset as factors
knn_dep<-predict(model_knn, testset)

# summarize metrics
knn_summary<-get_result_stats(knn_dep, testset$deposit)

# predict values for testset as probablities 
# used for Area under the curve calculation usinf different cutoff values
knn_y<- predict(model_knn, testset, type="prob")[,2]
pred.knn <- prediction(knn_y, testset$deposit)
knn_auc = performance(pred.knn, measure = "auc")

# plot ROC curve for different cutoff values
roc.perf.knn = performance(pred.knn, measure = "tpr", x.measure = "fpr")
plot(roc.perf.knn, main="KNN ROC Curve")
abline(a=0, b= 1)

# create a summary to add results after each model
all_results<-data.frame(method="Baseline: KNN", f1score=knn_summary$f1score,
                        accuracy=knn_summary$acc, precision=knn_summary$precision,
                        recall=knn_summary$recall, AUC=round(knn_auc@y.values[[1]],6))
all_results%>%knitr::kable()

##################################################################################

# Naive Bayes model 

# use caret package, with cross-validation 
set.seed(123, sample.kind="Rounding")
model_nb<-  train(deposit~., data=trainset, method = "naive_bayes", 
                  trControl = ctrl)

# predict values for testset as factors
nb_dep<-predict(model_nb, testset)

# summarize metrics
nb_summary<-get_result_stats(nb_dep, testset$deposit)

# predict values for testset as probablities 
# used for Area under the curve calculation usinf different cutoff values
nb_y<- predict(model_nb, testset, type="prob")[,2]
pred <- prediction(nb_y, testset$deposit)
nb_auc = performance(pred, measure = "auc")

# add results to summary
all_results<-rbind(all_results, data.frame(method="Naive Bayes", f1score=nb_summary$f1score,
                                           accuracy=nb_summary$acc, precision=nb_summary$precision,
                                           recall=nb_summary$recall, AUC=round(nb_auc@y.values[[1]],6)))
all_results%>%knitr::kable()

##################################################################################

# Logistic Regression model 

# use caret package, with cross-validation 
set.seed(123, sample.kind="Rounding")
model_glm<-  train(deposit~., data=trainset, method = "glm", 
                   trControl = ctrl)

model_glm$finalModel

# predict values for testset as factors
glm_dep<-predict(model_glm, testset)

# summarize metrics
glm_summary<-get_result_stats(glm_dep, testset$deposit)

# predict values for testset as probablities 
# used for Area under the curve calculation usinf different cutoff values
glm_y<- predict(model_glm, testset, type="prob")[,2]
pred <- prediction(glm_y, testset$deposit)
glm_auc = performance(pred, measure = "auc")

# add results to summary
all_results<-rbind(all_results, data.frame(method="Logistic Regression", f1score=glm_summary$f1score,
                                           accuracy=glm_summary$acc, precision=glm_summary$precision,
                                           recall=glm_summary$recall, AUC=round(glm_auc@y.values[[1]],6)))

all_results%>%knitr::kable()
##################################################################################

# CART: Tree model 

# use caret package, with cross-validation and fine tune for cp(complex parameter)
set.seed(123, sample.kind="Rounding")
model_tree<-  train(deposit~., data=trainset, method="rpart", tuneGrid= data.frame(cp=seq(0, 0.5, 0.05)),
                    trControl = ctrl)
# best value of cp
model_tree$bestTune
ggplot(model_tree)

# plot(model_tree$finalModel)
# text(model_tree$finalModel)

# predict values for testset as factors
tree_dep<-predict(model_tree, testset)

# summarize metrics
tree_summary<-get_result_stats(tree_dep, testset$deposit)

# predict values for testset as probablities 
# used for Area under the curve calculation usinf different cutoff values
tree_y<- predict(model_tree, testset, type="prob")[,2]
pred <- prediction(tree_y, testset$deposit)
tree_auc = performance(pred, measure = "auc")

# add results to summary
all_results<-rbind(all_results, data.frame(method="CART", f1score=tree_summary$f1score,
                                           accuracy=tree_summary$acc, precision=tree_summary$precision,
                                           recall=tree_summary$recall, AUC=round(tree_auc@y.values[[1]],6)))
all_results%>%knitr::kable()

##################################################################################

# Random Forest model 

# the randomForest package is used with cross-validation k-fold=5
set.seed(123, sample.kind="Rounding")
model_rf<-  randomForest(deposit~., data=trainset,  mtry=4,
                         trControl = ctrl)

# predict values for testset as factors
rf_dep<-predict(model_rf, testset)

# summarize metrics
rf_summary<-get_result_stats(rf_dep, testset$deposit)

# predict values for testset as probablities 
# used for Area under the curve calculation usinf different cutoff values
rf_y<- predict(model_rf, testset, type="prob")[,2]
pred <- prediction(rf_y, testset$deposit)
rf_auc = performance(pred, measure = "auc")

# add results to summary
all_results<-rbind(all_results, data.frame(method="Random Forest", f1score=rf_summary$f1score,
                                           accuracy=rf_summary$acc, precision=rf_summary$precision,
                                           recall=rf_summary$recall, AUC=round(rf_auc@y.values[[1]],6)))
all_results%>%knitr::kable()

# The variable importance plot shows which variables 
# have had an impact in the classification
varImpPlot(model_rf)


##################################################################################

# Gradient Boosting model 

# use caret package, with cross-validation and fine tune for interaction.depth & n.trees
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
set.seed(123, sample.kind="Rounding")
model_gbm<-  train(deposit~., data=trainset, method="gbm",
                   trControl = ctrl, tuneGrid = gbmGrid)

# best value of hyper-parameters
model_gbm$bestTune

# predict values for testset as factors
gbm_dep<-predict(model_gbm, testset)

# summarize metrics
gbm_summary<-get_result_stats(gbm_dep, testset$deposit)

# predict values for testset as probabilities 
# used for Area under the curve calculation using different cutoff values
gbm_y<- predict(model_gbm, testset, type="prob")[,2]
pred.gbm <- prediction(gbm_y, testset$deposit)
gbm_auc = performance(pred.gbm, measure = "auc")

# add results to summary
all_results<-rbind(all_results, data.frame(method="Gradient Boost", f1score=gbm_summary$f1score,
                                           accuracy=gbm_summary$acc, precision=gbm_summary$precision,
                                           recall=gbm_summary$recall, AUC=round(gbm_auc@y.values[[1]],6)))

all_results%>%knitr::kable()


# plot ROC curve for different cutoff values
roc.perf.gbm = performance(pred.gbm, measure = "tpr", x.measure = "fpr")
plot(roc.perf.gbm, main="Gradient Boosting ROC Curve")
abline(a=0, b= 1)

# plot of KNN ROC curve to compare performance
plot(roc.perf.gbm, main="GBM & KNN ROC Curves")
par(new=TRUE)
plot(roc.perf.knn)
abline(a=0, b= 1)


#####################################################################
# Cut off analysis: Analyze different cut-off levels to define
# the two class labels for best F score

# user defined function to sweep cutoff values for optimum Fscore
get_cutoff<-function(x){
  
  cutoff<-seq(0.1, 0.9, 0.025)
  f1s<-sapply(cutoff, function(z){
    y<-ifelse(x>z,1,0)
    scores<-get_result_stats(y, testset$deposit)
    scores$f1score
  })
  return(cutoff[which.max(f1s)])
}

# Perform cut-off analysis on the GBM output
final_cutoff<-get_cutoff(gbm_y) 

# predict outcomes based on fine tuned cutoff
final_y<-ifelse(gbm_y>final_cutoff,1,0)

# final summary for train/test data
final_stats<-get_result_stats(final_y, testset$deposit)


# add results to summary
all_results<-rbind(all_results, data.frame(method="Gradient Boost(Cut-off analysis)", f1score=final_stats$f1score,
                                           accuracy=final_stats$acc, precision=final_stats$precision,
                                           recall=final_stats$recall, AUC=round(gbm_auc@y.values[[1]],6)))

all_results%>%knitr::kable()

final_cutoff

#####################################################################
# Final Model on the evaluation dataset

#Predict probability outputs for the Evaluation dataset 
eval_y<- predict(model_gbm, evalset, type="prob")[,2]

# Use cutoff obtained from GBM to define class labels
evalset_deposit<-ifelse(eval_y>final_cutoff,1,0)

# Get final summary for evaluation set
final_summary<-get_result_stats(evalset_deposit, evalset$deposit)

# Final summary of the evaluation of the model put in easy to read format
data.frame(F1Score=final_summary$f1score, Accuracy=final_summary$acc, Precision=final_summary$precision, 
           Recall=final_summary$recall)
