# xvalues<-c("duration", "pdays")

# Remove the duration and pdays columns.
# removing pdays as more than half of the customers were not contacted before

bank_data_ml<-bank_data[, -c(12, 14)]

# Splitting data into train, test and eval sets with 80% 20% ratio
set.seed(123)

test_index <- createDataPartition(bank_data_ml$deposit, times = 1, p = 0.2, list = FALSE)
temp <- bank_data_ml[-test_index,]
evalset <- bank_data_ml[test_index,]

set.seed(123)

test_index <- createDataPartition(temp$deposit, times = 1, p = 0.2, list = FALSE)
trainset <- temp [-test_index,]
testset <- temp [test_index,]


##################################################################################

get_result_stats<-function(x,y){
  cm<-table(Predict=x, Reference=y)
  acc<-(cm[1,1]+cm[2,2])/sum(cm)
  precision<-cm[2,2]/(cm[1,2]+cm[2,2])
  recall<-cm[2,2]/(cm[2,1]+cm[2,2])
  f1score<-2*precision*recall/(precision+recall)
  
  list(cm=cm, acc=acc, precision=precision, recall=recall, f1score=f1score)
}

##################################################################################

ctrl <- trainControl(method = "cv",
                     number = 5,
                     allowParallel = TRUE)

##################################################################################

model_knn<-  train(deposit~., data=trainset, method = "knn", 
                   trControl = ctrl, tuneGrid = data.frame(k = seq(3, 50, 2)))
model_knn$bestTune
ggplot(model_knn)

knn_dep<-predict(model_knn, testset)
knn_summary<-get_result_stats(knn_dep, testset$deposit)

knn_y<- predict(model_knn, testset, type="prob")[,2]
pred <- prediction(knn_y, testset$deposit)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
knn_auc = performance(pred, measure = "auc")

plot(roc.perf, main="KNN ROC Curve")
abline(a=0, b= 1)

all_results<-data.frame(method="Baseline: KNN", f1score=knn_summary$f1score,
                                           acc=knn_summary$acc, precision=knn_summary$precision,
                                           recall=knn_summary$recall, AUC=round(knn_auc@y.values[[1]],6))


##################################################################################

model_nb<-  train(deposit~., data=trainset, method = "naive_bayes", 
                   trControl = ctrl)

nb_dep<-predict(model_nb, testset)

nb_summary<-get_result_stats(nb_dep, testset$deposit)

nb_y<- predict(model_nb, testset, type="prob")[,2]
pred <- prediction(nb_y, testset$deposit)
nb_auc = performance(pred, measure = "auc")

all_results<-rbind(all_results, data.frame(method="Naive Bayes", f1score=nb_summary$f1score,
                        acc=nb_summary$acc, precision=nb_summary$precision,
                        recall=nb_summary$recall, AUC=round(nb_auc@y.values[[1]],6)))


##################################################################################


model_glm<-  train(deposit~., data=trainset, method = "glm", 
                   trControl = ctrl)

glm_dep<-predict(model_glm, testset)
glm_summary<-get_result_stats(glm_dep, testset$deposit)

glm_y<- predict(model_glm, testset, type="prob")[,2]
pred <- prediction(glm_y, testset$deposit)
glm_auc = performance(pred, measure = "auc")

all_results<-rbind(all_results, data.frame(method="Logistic Regression", f1score=glm_summary$f1score,
                                           acc=glm_summary$acc, precision=glm_summary$precision,
                                           recall=glm_summary$recall, AUC=round(glm_auc@y.values[[1]],6)))


##################################################################################



model_tree<-  train(deposit~., data=trainset, method="rpart",
                         trControl = ctrl)
model_tree$bestTune
ggplot(model_tree)

tree_dep<-predict(model_tree, testset)
tree_summary<-get_result_stats(tree_dep, testset$deposit)

tree_y<- predict(model_tree, testset, type="prob")[,2]
pred <- prediction(tree_y, testset$deposit)
tree_auc = performance(pred, measure = "auc")

all_results<-rbind(all_results, data.frame(method="CART", f1score=tree_summary$f1score,
                                           acc=tree_summary$acc, precision=tree_summary$precision,
                                           recall=tree_summary$recall, AUC=round(tree_auc@y.values[[1]],6)))


##################################################################################


model_rf<-  randomForest(deposit~., data=trainset,  mtry=4,
                   trControl = ctrl)

rf_dep<-predict(model_rf, testset)
rf_summary<-get_result_stats(rf_dep, testset$deposit)

rf_y<- predict(model_rf, testset, type="prob")[,2]
pred <- prediction(rf_y, testset$deposit)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
rf_auc = performance(pred, measure = "auc")

all_results<-rbind(all_results, data.frame(method="Random Forest", f1score=rf_summary$f1score,
                                           acc=rf_summary$acc, precision=rf_summary$precision,
                                           recall=rf_summary$recall, AUC=round(rf_auc@y.values[[1]],6)))
all_results%>%knitr::kable()

varImpPlot(model_rf)

plot(roc.perf, main="RF ROC Curve")
abline(a=0, b= 1)
##################################################################################

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
model_gbm<-  train(deposit~., data=trainset, method="gbm",
                   trControl = ctrl, tuneGrid = gbmGrid)
model_gbm$bestTune
ggplot(model_gbm)

gbm_dep<-predict(model_gbm, testset)
gbm_summary<-get_result_stats(gbm_dep, testset$deposit)
gbm_y<- predict(model_gbm, testset, type="prob")[,2]
pred <- prediction(gbm_y, testset$deposit)
gbm_auc = performance(pred, measure = "auc")

all_results<-rbind(all_results, data.frame(method="Gradient Boost", f1score=gbm_summary$f1score,
                                           acc=gbm_summary$acc, precision=gbm_summary$precision,
                                           recall=gbm_summary$recall, AUC=round(gbm_auc@y.values[[1]],6)))

all_results%>%knitr::kable()

##################################################################################


gbm_summ<-get_result_stats(gbm_eval, evalset$deposit)
gbm_evalset<- predict(model_gbm, evalset, type="prob")[,2]
pred_evalset <- prediction(gbm_evalset, evalset$deposit)
gbm_auc_eval = performance(pred_evalset, measure = "auc")


data.frame(Accuracy=gbm_summ$acc, Precision=gbm_summ$precision, 
           Recall=gbm_summ$recall, F1Score=gbm_summ$f1score, AUC=round(gbm_auc_eval@y.values[[1]],6))
