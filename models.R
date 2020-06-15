# xvalues<-c("duration", "pdays")

bank_data_ml<-bank_data[, -c(12, 14)]

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(bank_data$deposit, times = 1, p = 0.2, list = FALSE)
trainset <- bank_data_ml %>% slice(-test_index)
testset <- bank_data_ml %>% slice(test_index)


get_result_stats<-function(x,y){
  cm<-table(Predict=x, Reference=y)
  acc<-(cm[1,1]+cm[2,2])/sum(cm)
  precision<-cm[2,2]/(cm[1,2]+cm[2,2])
  recall<-cm[2,2]/(cm[2,1]+cm[2,2])
  f1score<-2*precision*recall/(precision+recall)
  
  list(cm=cm, acc=acc, precision=precision, recall=recall, f1score=f1score)
}


ctrl <- trainControl(method = "cv",
                     number = 5,
                     allowParallel = TRUE)


model_nb<-  train(deposit~., data=trainset, method = "naive_bayes", 
                   trControl = ctrl)

nb_dep<-predict(model_nb, testset)

nb_summary<-get_result_stats(nb_dep, testset$deposit)

nb_y<- predict(model_nb, testset, type="prob")[,2]
pred <- prediction(nb_y, testset$deposit)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
nb_auc = performance(pred, measure = "auc")

all_results<-data.frame(method="Base: Naive Bayes", f1score=nb_summary$f1score,
                        acc=nb_summary$acc, precision=nb_summary$precision,
                        recall=nb_summary$recall, AUC=round(nb_auc@y.values[[1]],6))




model_glm<-  train(deposit~., data=trainset, method = "glm", 
                   trControl = ctrl)

glm_dep<-predict(model_glm, testset)
glm_summary<-get_result_stats(glm_dep, testset$deposit)

glm_y<- predict(model_glm, testset, type="prob")[,2]
pred <- prediction(glm_y, testset$deposit)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
glm_auc = performance(pred, measure = "auc")

all_results<-rbind(all_results, data.frame(method="Logistic Regression", f1score=glm_summary$f1score,
                                           acc=glm_summary$acc, precision=glm_summary$precision,
                                           recall=glm_summary$recall, AUC=round(glm_auc@y.values[[1]],6)))


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

varImpPlot(model_rf)
