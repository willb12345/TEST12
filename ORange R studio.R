
############# MODLENG For Neural Net

library(rsample)
set.seed(501)

summary(dfOrange)

#orange

# convert factor to numeric 

str(dfOrange)

dfOrange$cust_gender_cd <- as.numeric(dfOrange$cust_gender_cd)
dfOrange$cust_language_cd <- as.numeric(dfOrange$cust_language_cd)
dfOrange$cust_mkt_segm_desc <- as.numeric(dfOrange$cust_mkt_segm_desc)
dfOrange$trf_mdl_phonedeal_cd <- as.numeric(dfOrange$trf_mdl_phonedeal_cd)
dfOrange$count_orange <- as.numeric(dfOrange$count_orange)
dfOrange$voice_oob_nat_mean <- as.numeric(dfOrange$voice_oob_nat_mean)
dfOrange$days_since_moving <- as.numeric(dfOrange$days_since_moving)
dfOrange$churned <- as.numeric(dfOrange$churned)

str(dfOrange)

#orange
split_orange <- rsample::initial_split(dfOrange, prop = 0.7, strata = churned)

orange.train <- rsample::training(split_orange)
orange.test <- rsample::testing(split_orange)

summary(orange.train)


orange_pp <- preProcess(orange.train[1:15], method = c("range"))
orange.train2 <- predict(orange_pp, orange.train[1:15])
orange.test2<- predict(orange_pp, orange.test[1:15])



#IF WE WANT TO DO A VALIDATION SET AS WELL WE CAN SPLIT THE TEST SET AGAIN 50/50
str(orange.train2)


#nnet

orange_nnet_model <- nnet(churned ~ ., data = orange.train2, size = 100, maxit = 10000,  MaxNWts = 10000)

train_predictions <- predict(orange_nnet_model, orange.train2)
mean(train_class_predictions == orange.train2$churn)


test_predictions <- predict(orange_nnet_model, orange.test2)
mean(test_predictions == orange.test2$churn)

library(caret)

train_class_predictions <- as.numeric(train_predictions > 0.035)
(confusion_matrix <- table(actual = orange.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.035)
(confusion_matrix <- table(actual = orange.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))
#-------------------------------------------------- Neural Net Testing


str(glass_train)

# hidden------------------------------------------------------------- 10
library(neuralnet)
orangenn10 <- neuralnet(churned ~ r_age_val + cust_gender_cd + cust_language_cd + cust_mkt_segm_desc + trf_mdl_phonedeal_cd + count_orange + cust_total_mobile_qty + voice_oob_mean + voice_oob_nat_mean + mean_bill_rev_vs_trf_plan + tenure_days + days_since_moving + nb_cont + avg_tp_churn, data=orange.train2, hidden=10, threshold=0.2)

orangenn$result.matrix
plot(orangenn)

train_predictions <- predict(orangenn10, orange.train2)
mean(train_class_predictions == orange.train2$churn)


test_predictions <- predict(orangenn10, orange.test2)
mean(test_class_predictions == orange.test2$churn)

library(caret)

train_class_predictions <- as.numeric(train_predictions > 0.035)
(confusion_matrix <- table(actual = orange.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.035)
(confusion_matrix <- table(actual = orange.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

#------------------------------------------------------------------- hidden 2

library(neuralnet)
orangenn2 <- neuralnet(churned ~ r_age_val + cust_gender_cd + cust_language_cd + cust_mkt_segm_desc + trf_mdl_phonedeal_cd + count_orange + cust_total_mobile_qty + voice_oob_mean + voice_oob_nat_mean + mean_bill_rev_vs_trf_plan + tenure_days + days_since_moving + nb_cont + avg_tp_churn, data=orange.train2, hidden=2, threshold=0.1)

orangenn2$result.matrix
plot(orangenn2)

train_predictions <- predict(orangenn2, orange.train2)
mean(train_class_predictions == orange.train2$churn)


test_predictions <- predict(orangenn2, orange.test2)
mean(test_class_predictions == orange.test2$churn)

library(caret)

train_class_predictions <- as.numeric(train_predictions > 0.035)
(confusion_matrix <- table(actual = orange.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.035)
(confusion_matrix <- table(actual = orange.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

#------------------------------------------------------------------- hidden 2,2,2

library(neuralnet)
orangenn222 <- neuralnet(churned ~ r_age_val + cust_gender_cd + cust_language_cd + cust_mkt_segm_desc + trf_mdl_phonedeal_cd + count_orange + cust_total_mobile_qty + voice_oob_mean + voice_oob_nat_mean + mean_bill_rev_vs_trf_plan + tenure_days + days_since_moving + nb_cont + avg_tp_churn, data=orange.train2, hidden=c(2,2,2), threshold=0.1)

orangenn222$result.matrix
plot(orangenn222)

train_predictions <- predict(orangenn222, orange.train2)
mean(train_class_predictions == orange.train2$churn)


test_predictions <- predict(orangenn222, orange.test2)
mean(test_class_predictions == orange.test2$churn)

library(caret)

train_class_predictions <- as.numeric(train_predictions > 0.035)
(confusion_matrix <- table(actual = orange.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.035)
(confusion_matrix <- table(actual = orange.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))
#----------------------Plot Orange

install.packages("ROCR")
library(ROCR)
orange_train.pred.r=prediction(train_predictions,orange.train2$churn)
Orange_train.ROC.r=performance(orange_train.pred.r,"tpr","fpr")
orange_test.pred.r=prediction(test_predictions,orange.test2$churn)
orange_test.ROC.r=performance(orange_train.pred.r,"tpr","fpr")
plot(Orange_train.ROC.r,lwd=5,lty=2,col='blue',main="Reduced dataset AUC for train and test data")
text(x=0.3,y=0.75,paste("Train AUC for hidden 2 is",round(as.numeric(performance(orange_train.pred.r, "auc")@y.values),4)),col='blue')
plot(orange_test.ROC.r,add=T,col='red',lty=1)
text(x=0.7,y=0.75,paste("Test AUC for hidden 2 is",round(as.numeric(performance(orange_test.pred.r, "auc")@y.values),4)),col='red')

#---------------------------------Internet

library(rsample)
set.seed(501)

summary(dfISChurnInfo)


#internet
split_internet <- rsample::initial_split(dfISChurnInfo, prop = 0.7, strata = churn)

internet.train <- rsample::training(split_internet)
internet.test <- rsample::testing(split_internet)

#IF WE WANT TO DO A VALIDATION SET AS WELL WE CAN SPLIT THE TEST SET AGAIN 50/50



#move churn to the last position 

internet.train <- internet.train %>% relocate(churn, .after = adjusted_contract_numeric)
internet.test <- internet.test %>% relocate(churn, .after = adjusted_contract_numeric)

summary(dfISChurnInfo)


internet_pp <- preProcess(internet.train[1:10], method = c("range"))
internet.train2 <- predict(internet_pp, internet.train[1:10])
internet.test2  <- predict(internet_pp, internet.test[1:10])


#IF WE WANT TO DO A VALIDATION SET AS WELL WE CAN SPLIT THE TEST SET AGAIN 50/50
str(internet.train2)

# Neural Net Testing

install.packages("neuralnet")
library("neuralnet")
# Prepare outputs

library("nnet")

### size 250

internet_nnet_model <- nnet(churn ~ ., data = internet.train2, size = 250, maxit = 10000,  MaxNWts = 10000)

train_predictions <- predict(internet_nnet_model,internet.train2)
mean(train_class_predictions == internet.train2$churn)




test_predictions <- predict(internet_nnet_model, internet.test2)
mean(test_class_predictions == internet.test2$churn)


library(caret)


library(ROCR)

train_class_predictions <- as.numeric(train_predictions > 0.5)
(confusion_matrix <- table(actual = internet.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.5)
(confusion_matrix <- table(actual = internet.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

#----------------------Plot internet

internet_train.pred.r=prediction(train_predictions,internet.train2$churn)
internet_train.ROC.r=performance(internet_train.pred.r,"tpr","fpr")
internet_test.pred.r=prediction(test_predictions,internet.test2$churn)
internet_test.ROC.r=performance(internet_train.pred.r,"tpr","fpr")
plot(internet_train.ROC.r,lwd=5,lty=2,col='blue',main="Reduced dataset AUC for train and test data")
text(x=0.3,y=0.75,paste("Train AUC is",round(as.numeric(performance(internet_train.pred.r, "auc")@y.values),4)),col='blue')
plot(internet_test.ROC.r,add=T,col='red',lty=1)
text(x=0.7,y=0.75,paste("Test AUC is",round(as.numeric(performance(internet_test.pred.r, "auc")@y.values),4)),col='red')


# other neural net

#----------------------------- Hidden 10

library(neuralnet)
internetnn10 <- neuralnet(churn ~ is_tv_subscriber + is_movie_package_subscriber + service_failure_count + subscription_age + bill_avg + download_avg + upload_avg + download_over_limit + adjusted_contract_numeric, data=internet.train2, hidden=10, threshold=0.3)

internetnn10$result.matrix
plot(internetnn10)

library(caret)
train_predictions <- predict(internetnn10, internet.train2)
mean(train_class_predictions == internet.train2$churn)


test_predictions <- predict(internetnn10, internet.test2)
mean(test_class_predictions == internet.test2$churn)

library(caret)

train_class_predictions <- as.numeric(train_predictions > 0.5)
(confusion_matrix <- table(actual = internet.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.5)
(confusion_matrix <- table(actual = internet.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

# Hidden 2

library(neuralnet)
internetnn2 <- neuralnet(churn ~ is_tv_subscriber + is_movie_package_subscriber + service_failure_count + subscription_age + bill_avg + download_avg + upload_avg + download_over_limit + adjusted_contract_numeric, data=internet.train2, hidden=2, threshold=0.3)

internetnn2$result.matrix
plot(internetnn2)

library(caret)
train_predictions <- predict(internetnn2, internet.train2)
mean(train_class_predictions == internet.train2$churn)


test_predictions <- predict(internetnn2, internet.test2)
mean(test_class_predictions == internet.test2$churn)

library(caret)

train_class_predictions <- as.numeric(train_predictions > 0.5)
(confusion_matrix <- table(actual = internet.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.5)
(confusion_matrix <- table(actual = internet.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

#-------------------------------------------- Hidden 2,2,2

library(neuralnet)
internetnn222 <- neuralnet(churn ~ is_tv_subscriber + is_movie_package_subscriber + service_failure_count + subscription_age + bill_avg + download_avg + upload_avg + download_over_limit + adjusted_contract_numeric, data=internet.train2, hidden=c(2,2,2), threshold=0.4)

internetnn222$result.matrix
plot(internetnn222)

library(caret)
train_predictions <- predict(internetnn222, internet.train2)
mean(train_class_predictions == internet.train2$churn)


test_predictions <- predict(internetnn222, internet.test2)
mean(test_class_predictions == internet.test2$churn)

library(caret)

train_class_predictions <- as.numeric(train_predictions > 0.5)
(confusion_matrix <- table(actual = internet.train2$churn, predicted = train_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))

test_class_predictions <- as.numeric(test_predictions > 0.5)
(confusion_matrix <- table(actual = internet.test2$churn, predicted = test_class_predictions))
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))
(f1 = 2 * precision * recall/(precision + recall))




