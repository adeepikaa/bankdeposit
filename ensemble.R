vote_set<-NULL
vote_set<-as.data.frame(cbind(knn_dep, nb_dep, glm_dep, gbm_dep, rf_dep, deposit=testset$deposit))
vote_set$deposit<-as.factor(vote_set$deposit)

vote_set$votes<-ifelse(rowMeans(vote_set[,c(1:5)])>1.5, 2, 1)
vote_sum<-get_result_stats(vote_set$votes, testset$deposit)

####################################################################

stack_set<-NULL
stack_set<-as.data.frame(cbind(knn_dep, nb_dep, glm_dep, gbm_dep, rf_dep, deposit=testset$deposit))
stack_set$deposit<-as.factor(stack_set$deposit)

model_gbm2<-  train(deposit~., data=stack_set, method="gbm",
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10), tuneGrid = gbmGrid)

rf_eval<-predict(model_rf, evalset)
knn_eval<-predict(model_knn, evalset)
nb_eval<-predict(model_nb, evalset)
glm_eval<-predict(model_glm, evalset)
gbm_eval<-predict(model_gbm, evalset)

eval_set<-NULL
eval_set<-as.data.frame(cbind(knn_dep=knn_eval, nb_dep=nb_eval, glm_dep=glm_eval, gbm_dep=gbm_eval, rf_dep=rf_eval, deposit=evalset$deposit))

gbm_final<-predict(model_gbm2, eval_set)
gbm_sum<-get_result_stats(gbm_final, evalset$deposit)


gbm_sum$cm

data.frame(Accuracy=gbm_sum$acc, Precision=gbm_sum$precision, Recall=gbm_sum$recall, F1Score=gbm_sum$f1score)

