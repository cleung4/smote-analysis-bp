summary(a<-glm(y~.,family = "binomial",data=your_dataset_here))

df = your_dataset_here %>% mutate_at(c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14','x15','x16','x17','x18','x19','x20',
                                           'x21','x22','x23','x24','x25','x26','x27','x28','x29','x30','x31','x32','x33','x34','x35','x36','x37','x38',
                                           'x39','x40','x41','x42','x43','x44','x45','x46','x47','x48','x49','x50','x51','x52','x53','x54','x55','x56',
                                           'x57','x58','x59','x60','x61','x62','x63','x64','x65','x66','x67','x68','x69','x70','x71','x72','x73','x74'),~(scale(.) %>% as.vector))

ot = 3 # outlier threshold 

df_clean = subset(df,abs(x1)<ot & abs(x2)<ot & abs(x3)<ot & abs(x4)<ot & abs(x5)<ot & abs(x6)<ot & abs(x7)<ot & abs(x8)<ot & abs(x9)<ot & abs(x10)<ot & abs(x11)<ot
                  & abs(x12)<ot & abs(x13)<ot & abs(x14)<ot & abs(x15)<ot & abs(x16)<ot & abs(x17)<ot & abs(x18)<ot & abs(x19)<ot & abs(x20)<ot & abs(x21)<ot 
                  & abs(x22)<ot & abs(x23)<ot & abs(x24)<ot & abs(x25)<ot & abs(x26)<ot & abs(x27)<ot & abs(x28)<ot & abs(x29)<ot & abs(x30)<ot & abs(x31)<ot
                  & abs(x32)<ot & abs(x33)<ot & abs(x34)<ot & abs(x35)<ot & abs(x36)<ot & abs(x37)<ot & abs(x38)<ot & abs(x39)<ot & abs(x40)<ot & abs(x41)<ot
                  & abs(x42)<ot & abs(x43)<ot & abs(x44)<ot & abs(x45)<ot & abs(x46)<ot & abs(x47)<ot & abs(x48)<ot & abs(x49)<ot & abs(x50)<ot & abs(x51)<ot
                  & abs(x52)<ot & abs(x53)<ot & abs(x54)<ot & abs(x55)<ot & abs(x56)<ot & abs(x57)<ot & abs(x58)<ot & abs(x59)<ot & abs(x60)<ot & abs(x61)<ot
                  & abs(x62)<ot & abs(x63)<ot & abs(x64)<ot & abs(x65)<ot & abs(x66)<ot & abs(x67)<ot & abs(x68)<ot & abs(x69)<ot & abs(x70)<ot & abs(x71)<ot
                  & abs(x72)<ot & abs(x74)<ot)

df_clean$x73= NULL # constant
df_clean$x15= NULL # 
df_clean$x24= NULL
df_clean$x25= NULL
df_clean$x27= NULL
df_clean$x29= NULL
df_clean$x39= NULL
df_clean$x41= NULL
df_clean$x65= NULL

# At this point, we have 66 variables left
# This means 65 X variables
sum(cor(df_clean)>0.85) # this tells us there are 79 pairs of variables that have correlation larger than 0.85
# But correlation only calculate the relationship between two variables
# It can't consider relationship between more than 2 variables.
# try use a linear model 
# Use linear probability model to find colinearity
summary(lm(y~.,data=df_clean))
# there are 4 NA, remove them
df_clean$x21= NULL
df_clean$x46= NULL
df_clean$x47= NULL
df_clean$x56= NULL
# use vif
sum(vif(lm(y~.,data=df_clean))>10) # this tells us there are 32 variables have high vif

# According to the vif results, I decide
df_clean$x2= NULL
df_clean$x4= NULL
df_clean$x6= NULL
df_clean$x8= NULL
df_clean$x16= NULL
df_clean$x28= NULL
df_clean$x34= NULL
df_clean$x36= NULL
df_clean$x42= NULL
df_clean$x44= NULL
df_clean$x58= NULL
df_clean$x68= NULL
df_clean$x70= NULL

# use vif
sum(vif(lm(y~.,data=df_clean))>10) # this tells us there are 11 variables have high vif

df_clean$x10= NULL
df_clean$x13= NULL
df_clean$x17= NULL
df_clean$x60= NULL
df_clean$x62= NULL
df_clean$x66= NULL

# use vif
sum(vif(lm(y~.,data=df_clean))>10) # this tells us there are 11 variables have high vif

# At this step no vif is larger than 10


summary(glm(y~.,family = "binomial",data=df_clean))
# 1. based on heat map, try to remove highly correlated variables and see if the estimation results change a lot
# 2. calculate the average marginal effect of the variables
df_clean$y <- as.factor(df_clean$y)

set.seed(123) # For reproducibility
# 3. try different K and dup_size
smote_result <- SMOTE(X = df_clean[, -1], target = df_clean$y, 
                      K = 10, dup_size = 35)

# K = 2, 5, 10
# dup_size = 35, 30, 25, 20, 15, 10
df_smote <- data.frame(smote_result$data)
names(df_smote)[ncol(df_smote)] <- "y"
df_smote$y <- as.factor(df_smote$y)

summary(a<-glm(y~.,family = "binomial",data=df_smote))
# 4. calculate the average marginal effect of the variables, Is there important difference in the estimation compare to the results without SMOTE?

# Create training data and test data

# original data
set.seed(123)
training.samples <- createDataPartition(df_clean$y, p = 0.8, list = FALSE)
train.data.clean  <- df_clean[training.samples, ]
test.data.clean <- df_clean[-training.samples, ]

summary(logit1<-glm(y~.,family = "binomial",data=train.data.clean))

prediction_logit1 = predict(logit1,test.data.clean,type = "response")

prediction_logit1 = prediction_logit1>0.5

prediction_logit1 <- ifelse(prediction_logit1=="TRUE", 1, 0)

confusionMatrix(factor(test.data.clean$y),factor(prediction_logit1))

# smote data
set.seed(123)
training.samples <- createDataPartition(df_smote$y, p = 0.8, list = FALSE)
train.data.smote <- df_smote[training.samples, ]
test.data.smote <- df_smote[-training.samples, ]

summary(logit2<-glm(y~.,family = "binomial",data=train.data.smote))

prediction_logit2 = predict(logit2,test.data.smote,type = "response")

prediction_logit2 = prediction_logit2>0.5

prediction_logit2 <- ifelse(prediction_logit2=="TRUE", 1, 0)

confusionMatrix(factor(test.data.smote$y),factor(prediction_logit2))

# This indicates SMOTE does improve predictability of logistic regression

install.packages("glmnet")
install.packages("caret")
install.packages("tidyverse")
install.packages("faraway")
install.packages("MASS")
install.packages("randomForest")

library(glmnet)
library(caret)
library(tidyverse)
library(faraway)
library(MASS)
library(smotefamily)

# set.seed(123)
# elastic.model.clean <- train(
#   y ~., data = df_clean, method = "glmnet",
#   trControl = trainControl("cv", number = 10),
#   tuneLength = 10, family="binomial"
# )
# 
# set.seed(123)
# elastic.model.smote <- train(
#   y ~., data = df_smote, method = "glmnet",
#   trControl = trainControl("cv", number = 10),
#   tuneLength = 10, family="binomial"
# )

#elastic.model$bestTune

#coef(elastic.model$finalModel, elastic.model$bestTune$lambda)

# Predictability of elastic net with original data

# prediction_elastic.clean = predict(elastic.model.clean,test.data.clean,type = "prob")
# 
# prediction_elastic.clean = prediction_elastic.clean[2]>0.5
# 
# prediction_elastic.clean <- ifelse(prediction_elastic.clean=="TRUE", 1, 0)
# 
# confusionMatrix(factor(test.data.clean$y),factor(prediction_elastic.clean))

# Predictability of elastic net with smote

# prediction_elastic.smote = predict(elastic.model.smote,test.data.smote,type = "prob")
# 
# prediction_elastic.smote = prediction_elastic.smote[2]>0.5
# 
# prediction_elastic.smote <- ifelse(prediction_elastic.smote=="TRUE", 1, 0)
# 
# confusionMatrix(factor(test.data.clean$y),factor(prediction_elastic.smote))

# The goal is to see prediction performance in original data and smote data
# Try the estimate the model with 

library(dplyr)
library(ggplot2)

# Modeling packages
library(rpart)
library(caret)

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects

tree.model.clean <- rpart(
  formula = y ~ .,
  data    = df_clean,
  method  = "anova"
)

tree.model.clean

rpart.plot(tree.model.clean)

plotcp(tree.model.clean)

vip(tree.model.clean,num_features = 40)

library("randomForest")

r1<-randomForest(y~.,data = train.data.clean)

r2<-randomForest(y~.,data = train.data.smote)


prediction_randfor1 = predict(r1,test.data.clean,type = "response")

prediction_randfor2 = predict(r2,test.data.smote,type = "response")

confusionMatrix(factor(test.data.clean$y),factor(prediction_randfor1))

confusionMatrix(factor(test.data.smote$y),factor(prediction_randfor2))

library(e1071)
library(nnet)

balanced_acc <- function(truth, predicted) {
  # Ensure both are factors with consistent levels
  truth <- factor(truth, levels = c("0", "1"))
  predicted <- factor(predicted, levels = c("0", "1"))
  
  cm <- confusionMatrix(predicted, truth)
  return(cm$byClass["Balanced Accuracy"])
}

set.seed(123)
svm_model_clean <- train(
  y ~ ., data = train.data.clean, method = "svmRadial",
  trControl = trainControl(method = "none"),
  preProcess = c("center", "scale")
)

svm_pred_clean <- predict(svm_model_clean, test.data.clean)
acc_svm_clean <- balanced_acc(test.data.clean$y, svm_pred_clean)

print(acc_svm_clean)

# SMOTE data
set.seed(123)
svm_model_smote <- train(
  y ~ ., data = train.data.smote, method = "svmRadial",
  trControl = trainControl(method = "none"),
  preProcess = c("center", "scale")
)

svm_pred_smote <- predict(svm_model_smote, test.data.smote)
acc_svm_smote <- balanced_acc(test.data.smote$y, svm_pred_smote)

print(acc_svm_smote)
#Nnet
set.seed(123)
nn_model_clean <- train(
  y ~ ., data = train.data.clean, method = "nnet",
  trControl = trainControl(method = "none"),
  preProcess = c("center", "scale"),
  trace = FALSE,
  linout = FALSE
)

nn_pred_clean <- predict(nn_model_clean, test.data.clean)
acc_nn_clean <- balanced_acc(test.data.clean$y, nn_pred_clean)

print(acc_nn_clean)

# SMOTE data
set.seed(123)
nn_model_smote <- train(
  y ~ ., data = train.data.smote, method = "nnet",
  trControl = trainControl(method = "none"),
  preProcess = c("center", "scale"),
  trace = FALSE,
  linout = FALSE
)

nn_pred_smote <- predict(nn_model_smote, test.data.smote)
acc_nn_smote <- balanced_acc(test.data.smote$y, nn_pred_smote)

print(acc_nn_smote)
#results

acc_logit_clean <- balanced_acc(test.data.clean$y, prediction_logit1)
acc_logit_smote <- balanced_acc(test.data.smote$y, prediction_logit2)

acc_rf_clean <- balanced_acc(test.data.clean$y, prediction_randfor1)
acc_rf_smote <- balanced_acc(test.data.smote$y, prediction_randfor2)

results <- data.frame(
  Model = c("Logit", "Random Forest", "SVM", "Neural Net"),
  Clean = c(acc_logit_clean, acc_rf_clean, acc_svm_clean, acc_nn_clean),
  SMOTE = c(acc_logit_smote, acc_rf_smote, acc_svm_smote, acc_nn_smote)
)

print(results)
