#telecom customer churn
getwd()
setwd("F:/IMARTICUS/R")
library(caret)
library(broom)
library(dplyr)
library(dummy)
library(ggplot2)
library(ROCit)
library(purrr)
library(base)
library(CatEncoders)
library(Metrics)
library(tidyverse)
library(ggrepel)
library(GGally)
library(gridExtra)

library(ipred)


raw_data<-read.csv("churn.csv")
raw_data
dim(raw_data)
summary(raw_data)
str(raw_data)
length(raw_data)
ncol(raw_data)
data<-raw_data
factors<-names(which(sapply(data,is.factor)))
for(i in factors){
  encode<-LabelEncoder.fit(data[,i])
  data[,i]<-transform(encode,data[,i])
}
data
data_1<-data[-c(1)]
data_1
glimpse(data_1)

sum(is.na(data_1))
data_2<-data_1%>%mutate(TotalCharges=if_else(is.na(TotalCharges)==TRUE,median(TotalCharges,na.rm = TRUE),TotalCharges))
data_2
sum(is.na(data_2))
#removing outliers
remove_outliers<-function(x,na.rm=TRUE,...){
  qnt<-quantile(x,probs=c(.10,.90),na.rm=na.rm,...)
  H<-1.5*IQR(x,na.rm=na.rm)
  y<-x
  y[x<(qnt[1]-H)]<-NA
  y[x<-(qnt[3]+H)]<-NA
}
data_3<-data_2
data_3<-cbind(data_3[1],apply(data_3[2],2,remove_outliers),data_3[3:4],apply(data_3[5],2,remove_outliers),data_3[6:18],apply(data_3[19],2,remove_outliers),data_3[20])
data_3
sum(is.na(data_2))

df <- data_3
df %>% 
  group_by(Churn) %>% 
  count() %>%
  ggplot(aes(reorder(Churn, -n), n), fill = Churn)+
  geom_col(fill = c("seagreen", "steelblue"))+
  geom_text_repel(aes(label = n), size = 9)+
  coord_flip()+
  theme_minimal()+
  labs(x= NULL, y = "Frequency", title = "Dependent Variable - Churn")

ggplot(raw_data, aes(Partner, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Partner Status", 
       x = "Does the Customer have a Partner?", 
       y = "Count")
ggplot(raw_data, aes(Dependents, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Dependents Status", 
       x = "Does the Customer have Dependents?", 
       y = "Count")
ggplot(raw_data, aes(Dependents, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Dependents Status", 
       x = "Does the Customer have Dependents?", 
       y = "Count")
ggplot(raw_data, aes(InternetService, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Internet Service Status", 
       x = "Type of Internet Service Customer Has", 
       y = "Count")
ggplot(raw_data, aes(Contract, fill = Churn)) + 
  geom_bar() +
  labs(title = "Popularity of Contract Types", 
       x = "Type of Contract Customer Has", 
       y = "Count")
ggplot(raw_data, aes(PaymentMethod, fill = Churn)) + 
  geom_bar() +
  labs(title = "Payment Method", 
       x = "What Payment Method does the Customer Use?", 
       y = "Count")
ggplot(raw_data, aes(PaperlessBilling, fill = Churn)) + 
  geom_bar() +
  labs(title = "Paperless Billing Status", 
       x = "Does the Customer Use Paperless Billing?", 
       y = "Count")
ggplot(raw_data, aes(MonthlyCharges, fill = Churn)) + 
  geom_histogram() +
  labs(title = "Monthly Charges Histogram",
       x = "Monthly Charge to Customer", 
       y = "Count")
ggplot(telco, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")
df <- raw_data
grid.arrange(
  
  ggplot(df,aes(x = MonthlyCharges, color = Churn))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  
  ggplot(df,aes(x = TotalCharges, color = Churn))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  
  ggplot(df,aes(x = tenure, color = Churn))+ 
    geom_freqpoly(size=2)+
    theme_minimal()
  
)

data_numeric <- Filter(is.numeric, data_3)
data_numeric


library(reshape2)

cormat <- round(cor(data_numeric),2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value),method = "number" , iscorr = False) + 
  geom_tile() + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

library(corrplot)

cex.before <- par("cex")
par(cex = 0.7)

corrplot(cor(data_numeric), method = "color" , is.corr = FALSE ,addCoef.col="grey", order = "AOE",number.cex= 7/ncol(data_3))

par(cex = cex.before)

table(data_3$Churn)

data_3$Churn <- ifelse(data_3$Churn == 'Yes', 1, 0)
data_3

library(randomForest)
fit_rf = randomForest(Churn~., data=data_3)

# Create an importance based on mean decreasing gini
importance(fit_rf)

varImp(fit_rf)

# Create a plot of importance scores by random forest
varImpPlot(fit_rf)

**Removing feature with low importance**
  
  data_selected <- subset(data_3 , select = -c(PhoneService , Churn , StreamingTV))

data_selected

table(data_3$Contract)

data_3
data_3$gender <- ifelse(data_3$gender == "Male",1,0)
data_3$Partner <- ifelse(data_3$Partner == "Yes",1,0)
data_3$Dependents <- ifelse(data_3$Dependents == "Yes",1,0)
data_3$PhoneService <- ifelse(data_3$PhoneService == "Yes",1,0)

data_13 = select(data_3,select = -c('Churn'))
dmy <- dummyVars(" ~ .", data = data_13, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = data_13))

#select() function is used to choose a subset of variables or columns from a dataset
#dummyVars() function allows you to create dummy variables.

glimpse(dat_transformed)

data23 <- data_3
data_31 <- dat_transformed
data_31$Churn <- data_3$Churn
data_3 <- data_31

data3 <- subset(data_3,select=-c(Churn))
base_data <- data.frame(scale(data3))
base_data$Churn <- data_3$Churn
base_data

### 3.9 Standardizing Data

dmy <- dummyVars(" ~ .", data = data_selected, fullRank = T)
data_selected <- data.frame(predict(dmy, newdata = data_selected))

glimpse(data_selected)

data_standardized <- data.frame(scale(data_selected))
data_standardized$Churn <- data_3$Churn
data_standardized


### 3.10 Dimensionality Reduction

data_pca_base <- subset(data_standardized,select = -c(Churn))
data_pca <- prcomp(data_pca_base,
                   center = TRUE,
                   scale. = TRUE)

data_with_pca <- data.frame(as.matrix(data_pca_base) %*% as.matrix(data_pca$rotation[,0:5]))
data_with_pca$Churn <- data_standardized$Churn
head(data_with_pca)

## 4. Base Models

Before creating models let's split dataset into train and test

dt = sort(sample(nrow(base_data), nrow(base_data)*.8))
train<-base_data[dt,]
test<-base_data[-dt,]

actual <- test$Churn
actual <- if_else(actual == 1 , 0 , 1)
actual

### 4.1 Logistic Regression


##### 4.1.1 Build the model

table(train$Churn)

table(train$Churn)

lg_1 <- glm(factor(Churn) ~., data = train, family = "binomial")
summary(lg_1)

##### 4.1.2 Make predictions

prediction_lg_1 <- predict(lg_1,data.frame(test))
prediction_lg_1 <- if_else(prediction_lg_1 >= 0.5, 1, 0)
prediction_lg_1

##### 4.1.3 Performance Measure

table(prediction_lg_1)

factor(test$Churn)
factor(prediction_lg_1)


Get Confusion Matrix

confusionMatrix(data = factor(prediction_lg_1), 
                       reference = factor(test$Churn), 
                       positive = "1")

Accuracy and precision

accuracy_score_lg_1 = accuracy(actual , prediction_lg_1)
precision_score_lg_1 = precision(actual , prediction_lg_1)
accuracy_score_lg_1



**ROC Curve**

library(plyr)

ROCit_base <- rocit(score=prediction_lg_1,class=actual)
plot(ROCit_base)

###### 4.1.4 Tabulate Result 

accuracy_score_lg_1 = accuracy(actual , prediction_lg_1)
precision_score_lg_1 = precision(actual , prediction_lg_1)
recall_score_lg_1 = recall(actual , prediction_lg_1)
auc_score_lg_1 = auc(actual , prediction_lg_1)

Model <- c( "Accuracy","Precision","Recall","AUC Score")
Logistic_Regression <- c(accuracy_score_lg_1 , precision_score_lg_1 ,recall_score_lg_1  , auc_score_lg_1)

Base_Models = data.frame(Model,Logistic_Regression)
Base_Models

### 4.2 Decision Tree

##### 4.2.1 Build the model

library(rpart)
library(rpart.plot)
set.seed(1000)
model_tree_1 <- rpart(Churn ~ ., data = train, method = "class")
model_tree_1

##### 4.2.2 Make predictions

prediction_tree_1 <- predict(model_tree_1, test , type = "class")
#prediction_tree_1 <- if_else(prediction_tree_1 >= 0.5, 1, 0)
head(prediction_tree_1)

##### 4.2.3 Performance Measure

Get Confusion Matrix

confusionMatrix(data = factor(prediction_tree_1), 
                       reference = factor(test$Churn), 
                       positive = "1")

###### 4.2.4 Tabulate Result 

predicted <- as.numeric(prediction_tree_1)
predicted
predicted <- if_else(predicted == 2, 1, 0)
predicted

accuracy_score_tree_1 = accuracy(actual , predicted)
precision_score_tree_1 = precision(actual , predicted)
recall_score_tree_1 = recall(actual , predicted)
auc_score_tree_1 = auc(actual , predicted)

Base_Model_tree <- c(accuracy_score_tree_1 , precision_score_tree_1 ,recall_score_tree_1  , auc_score_tree_1)

Base_Models$Decision_Tree = Base_Model_tree
Base_Models

### 4.3 Random Forest


##### 4.3.1 Build the model

model_forest_1 <- randomForest(as.factor(Churn) ~ ., data = train)

# View the forest results.
print(model_forest_1) 

##### 4.3.2 Make predictions

prediction_forest_1 = predict(model_forest_1, test)
#prediction_forest_1 <- if_else(prediction_forest_1 >= 0.5, 1, 0)
prediction_forest_1

##### 4.3.3 Performance Measure

actual = test$Churn

Get Confusion Matrix

confusionMatrix(data = factor(prediction_forest_1), 
                       reference = factor(actual), 
                       positive = "1")

###### 4.3.4 Tabulate Result 

predicted <- as.numeric(prediction_forest_1)
predicted <- if_else(predicted == 2, 1, 0)
predicted

accuracy_score_forest_1 = accuracy(actual , predicted)
precision_score_forest_1 = precision(actual , predicted)
recall_score_forest_1 = recall(actual , predicted)
auc_score_forest_1 = auc(actual , predicted)

Base_Model_forest <- c(accuracy_score_forest_1 , precision_score_forest_1 ,recall_score_forest_1  , auc_score_forest_1)

Base_Models$Random_Forest = Base_Model_forest
Base_Models


### 4.4 KNN

##### 4.4.1 Build the model

library(class)
train_x <- subset(train,select = -c(Churn))
train_y <- train$Churn
test_x <- subset(test,select = -c(Churn))
test_y <- test$Churn

knn_model_1 = knn(train_x,test_x,train_y,k= 10)

##### 4.4.2 Make predictions

prediction_knn_1 = knn_model_1
prediction_knn_1

##### 4.4.3 Performance Measure

Get Confusion Matrix

confusionMatrix(data = as.factor(prediction_knn_1), 
                       reference = as.factor(actual), 
                       positive = "1")

###### 4.4.4 Tabulate Result 

predicted <- as.numeric(prediction_knn_1)
predicted <- if_else(predicted == 2, 1, 0)

table(actual)
table(predicted)

accuracy_score_knn_1 = accuracy(actual,predicted)
precision_score_knn_1 = precision(actual,predicted)
recall_score_knn_1 = recall(actual,predicted)
auc_score_knn_1 = auc(actual,predicted)

Base_Model_knn <- c(accuracy_score_knn_1 , precision_score_knn_1 ,recall_score_knn_1  , auc_score_knn_1)

Base_Models$KNN = Base_Model_knn
Base_Models

### 4.5 SVM


##### 4.5.1 Build the model

library(e1071)

base_model_svm_1 <- svm(as.factor(Churn) ~ ., data = train,formula = )
summary(base_model_svm_1)

##### 4.5.2 Make predictions

prediction_svm_1 = predict(base_model_svm_1, test)

##### 4.5.3 Performance Measure

Get Confusion Matrix

confusionMatrix(data = factor(prediction_svm_1), 
                       reference = factor(actual), 
                       positive = "1")

###### 4.5.4 Tabulate Result 

predicted <- as.numeric(prediction_svm_1)
predicted <- if_else(predicted == 2, 1, 0)

actual <- as.numeric(test$Churn)
actual <- if_else(actual == 2, 1, 0)

accuracy_score_svm_1 = accuracy(actual , predicted)
precision_score_svm_1 = precision(actual , predicted)
recall_score_svm_1 = recall(actual , predicted)
auc_score_svm_1 = auc(actual , predicted)

Base_Model_svm <- c(accuracy_score_svm_1 , precision_score_svm_1 ,recall_score_svm_1  , auc_score_svm_1)

Base_Models$SVM = Base_Model_svm
Base_Models

## 5. Models after feature selection

Before creating models let's split dataset into train and test

dt = sort(sample(nrow(data_standardized), nrow(data_standardized)*.8))
train<-data_standardized[dt,]
test<-data_standardized[-dt,]

For SVM we will use dataset on which pca is applied

dt1 = sort(sample(nrow(data_with_pca), nrow(data_with_pca)*.8))
train_svm<-data_with_pca[dt1,]
test_svm<-data_with_pca[-dt1,]

### 5.1 Logistic Regression


##### Build the model

lg_2 <- glm(factor(Churn) ~., data = train, family = "binomial")
summary(lg_2)

##### Make predictions

prediction_lg_2 <- predict(lg_2,data.frame(test))
prediction_lg_2 <- if_else(prediction_lg_2 >= 0.5, 1, 0)
prediction_lg_2

##### Performance Measure

table(prediction_lg_2)

factor(test$Churn)
factor(prediction_lg_2)

table(test$Churn)
table(prediction_lg_2)

Get Confusion Matrix

confusionMatrix(data = factor(prediction_lg_2), 
                reference = factor(test$Churn), 
                positive = "1")

###### Tabulate Result 

actual = test$Churn
accuracy_score_lg_2 = accuracy( actual , prediction_lg_2)
precision_score_lg_2 = precision(actual  , prediction_lg_2)
recall_score_lg_2 = recall(actual  , prediction_lg_2)
auc_score_lg_2 = auc(actual  , prediction_lg_2)

Model <- c( "Accuracy","Precision","Recall","AUC Score")
Logistic_Regression <- c(accuracy_score_lg_2 , precision_score_lg_2 ,recall_score_lg_2  , auc_score_lg_2)

Selected_Models = data.frame(Model,Logistic_Regression)
Selected_Models

### 5.2 Decision Tree


##### Build the model

library(rpart)
library(rpart.plot)
set.seed(1000)
model_tree_2 <- rpart(Churn ~ ., data = train, method = "class")
model_tree_2

##### Make predictions

prediction_tree_2 <- predict(model_tree_2, test , type = "class")
head(prediction_tree_2)


##### Performance Measure

actual = test$Churn
table(actual)
table(prediction_tree_2)

Get Confusion Matrix

confusionMatrix(data = factor(prediction_tree_2), 
                reference = factor(test$Churn), 
                positive = "1")

###### Tabulate Result 

predicted <- as.numeric(prediction_tree_2)
predicted <- if_else(predicted == 2, 1, 0)

actual <- as.numeric(test$Churn)
actual <- if_else(actual == 2, 1, 0)

table(actual)
table(predicted)

actual = test$Churn
accuracy_score_tree_2 = accuracy(actual , predicted )
precision_score_tree_2 = precision(actual , predicted )
recall_score_tree_2 = recall(actual , predicted )
auc_score_tree_2 = auc(actual , predicted )

Base_Model_tree <- c(accuracy_score_tree_2 , precision_score_tree_2 ,recall_score_tree_2  , auc_score_tree_2)

Selected_Models$Decision_Tree = Base_Model_tree
Selected_Models


### 5.3 Random Forest


##### Build the model

model_forest_2 <- randomForest(factor(Churn) ~ ., data = train)

# View the forest results.
print(model_forest_2) 

##### Make predictions

prediction_forest_2 = predict(model_forest_2, test)
#prediction_forest_2 <- if_else(prediction_forest_2 >= 0.5, 1, 0)
prediction_forest_2

##### Performance Measure

actual = test$Churn

Get Confusion Matrix

confusionMatrix(data = factor(prediction_forest_2), 
                reference = factor(test$Churn), 
                positive = "1")

###### Tabulate Result 

table(prediction_forest_2)

#predicted <- as.numeric(prediction_forest_2)
#predicted <- if_else(predicted == 2, 1, 0)
predicted <- prediction_forest_2
table(actual)
table(predicted)

accuracy_score_forest_2 = accuracy(actual , predicted)
precision_score_forest_2 = precision(actual , predicted)
recall_score_forest_2 = recall(as.numeric(actual) , as.numeric(predicted))
auc_score_forest_2 = auc(actual , predicted)

Base_Model_forest <- c(accuracy_score_forest_2 , precision_score_forest_2 ,recall_score_forest_2  , auc_score_forest_2)

Selected_Models$Random_Forest = Base_Model_forest
Selected_Models

### 5.4 KNN

##### Build the model

library(class)
train_x <- subset(train,select = -c(Churn))
train_y <- train$Churn
test_x <- subset(test,select = -c(Churn))
test_y <- test$Churn

knn_model_2 = knn(train_x,test_x,train_y,k= 10)


##### Make predictions

prediction_knn_2 = knn_model_2
prediction_knn_2


##### Performance Measure

actual = test$Churn

Get Confusion Matrix

confusionMatrix(data = as.factor(prediction_knn_2), 
                reference = as.factor(actual), 
                positive = "1")

###### Tabulate Result 
predicted <- as.numeric(prediction_knn_2)
predicted <- if_else(predicted == 2, 1, 0)

table(actual)
table(predicted)

accuracy_score_knn_2 = accuracy(actual , predicted)
precision_score_knn_2 = precision(actual , predicted)
recall_score_knn_2 = recall(actual , predicted)
auc_score_knn_2 = auc(actual , predicted)

Base_Model_knn <- c(accuracy_score_knn_2 , precision_score_knn_2 ,recall_score_knn_2  , auc_score_knn_2)

Selected_Models$KNN = Base_Model_knn
Selected_Models

### 5.5 SVM

##### Build the model

library(e1071)

base_model_svm_2 <- svm(as.factor(Churn) ~ ., data = train_svm)
summary(base_model_svm_2)

##### Make predictions

prediction_svm_2 = predict(base_model_svm_2, test_svm)


##### Performance Measure

actual = test_svm$Churn

Get Confusion Matrix

confusionMatrix(data = factor(prediction_svm_2), 
                reference = factor(actual), 
                positive = "1")

###### Tabulate Result 

predicted <- as.numeric(prediction_svm_2)
predicted <- if_else(predicted == 2, 1, 0)

table(actual)
table(predicted)

accuracy_score_svm_2 = accuracy(actual ,predicted)
precision_score_svm_2 = precision(actual ,predicted)
recall_score_svm_2 = recall(actual ,predicted)
auc_score_svm_2 = auc(actual ,predicted)

Base_Model_svm <- c(accuracy_score_svm_2 , precision_score_svm_2 ,recall_score_svm_2  , auc_score_svm_2)

Selected_Models$SVM = Base_Model_svm
Selected_Models

#6. Hyperparameter tuning
#Before creating models let's split dataset into train and test

dt = sort(sample(nrow(data_standardized), nrow(data_standardized)*.8))
train<-data_standardized[dt,]
test<-data_standardized[-dt,]

For SVM we will use dataset on which pca is applied

dt1 = sort(sample(nrow(data_with_pca), nrow(data_with_pca)*.8))
train_svm<-data_with_pca[dt1,]
test_svm<-data_with_pca[-dt1,]

### 6.1 Logistic Regression

actual <- test$Churn


##### Build the model

# Tunning logistic regression to find optimal parameter
set.seed(1)
lg_3 <- train(as.factor(Churn) ~., data=train, method='glm',
                    tuneGrid=expand.grid(parameter=c(0.001, 0.01, 0.1, 1,10,100, 1000)))

summary(lg_3)

##### Make predictions

prediction_lg_3 <- predict(lg_3,data.frame(test))
prediction_lg_3

##### Performance Measure

table(prediction_lg_3)

table(test$Churn)
table(prediction_lg_3)

Get Confusion Matrix

confusionMatrix(data = factor(prediction_lg_3), 
                       reference = factor(test$Churn), 
                       positive = "1")

Accuracy and precision

accuracy_score_lg_3 = accuracy(test$Churn , prediction_lg_3)
precision_score_lg_3 = precision(test$Churn , prediction_lg_3)
accuracy_score_lg_3

###### Tabulate Result 

predicted <- as.numeric(prediction_lg_3)
predicted <- if_else(predicted == 2, 1, 0)

table(actual)
table(predicted)

accuracy_score_lg_3 = accuracy(actual,predicted)
precision_score_lg_3 = precision(actual,predicted)
recall_score_lg_3 = recall(actual,predicted)
auc_score_lg_3 = auc(actual,predicted)

Model <- c( "Accuracy","Precision","Recall","AUC Score")
Logistic_Regression <- c(accuracy_score_lg_3 , precision_score_lg_3 ,recall_score_lg_3  , auc_score_lg_3)

Tunned_Models = data.frame(Model,Logistic_Regression)
Tunned_Models

### 6.2 Decision Tree

##### Build the model

# Creating a repeated cv object
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

library(rpart)
library(rpart.plot)
model_tree_3 <- train(factor(Churn) ~., data = train, method = "rpart",
                   trControl=trctrl,
                   tuneLength = 10, 
                   parms=list(split='information'))
model_tree_3

##### Make predictions

prediction_tree_3 <- predict(model_tree_3, test , type = "raw")
head(prediction_tree_3)

##### Performance Measure

Get Confusion Matrix

confusionMatrix(data = factor(prediction_tree_3), 
                       reference = factor(test$Churn), 
                       positive = "1")

###### Tabulate Result 


predicted <- as.numeric(prediction_tree_3)
predicted <- if_else(predicted == 2, 1, 0)

table(actual)
table(predicted)

accuracy_score_tree_3 = accuracy(actual , predicted)
precision_score_tree_3 = precision(actual , predicted)
recall_score_tree_3 = recall(actual , predicted)
auc_score_tree_3 = auc(actual , predicted)

Base_Model_tree <- c(accuracy_score_tree_3 , precision_score_tree_3 ,recall_score_tree_3  , auc_score_tree_3)

Tunned_Models$Decision_Tree = Base_Model_tree
Tunned_Models


### 6.3 Random Forest

##### Build the model

# Tunning random forest
library(randomForest)
control <- trainControl(method="repeatedcv", number=5, search="grid")
metric <- "Accuracy"

tunegrid <- expand.grid(.mtry=c(1:3))
model_forest_3 <- train(factor(Churn)~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

##### Make predictions

prediction_forest_3 = predict(model_forest_3, test)
#prediction_forest_3 <- if_else(prediction_forest_3 == 2, 1, 0)
prediction_forest_3

##### Performance Measure

actual = test$Churn

Get Confusion Matrix

confusionMatrix(data = factor(prediction_forest_3), 
                       reference = factor(actual), 
                       positive = "1")

###### Tabulate Result 

predicted <- as.numeric(prediction_forest_3)
predicted <- if_else(predicted == 2, 1, 0)

table(actual)
table(predicted)

accuracy_score_forest_3 = accuracy(actual,predicted)
precision_score_forest_3 = precision(actual,predicted)
recall_score_forest_3 = recall(actual,predicted)
auc_score_forest_3 = auc(actual,predicted)

Base_Model_forest <- c(accuracy_score_forest_3 , precision_score_forest_3 ,recall_score_forest_3  , auc_score_forest_3)

Tunned_Models$Random_Forest = Base_Model_forest
Tunned_Models


### 6.4 KNN

#Note : Tunning of KNN consumes lot of resources so skip the step. If you want to tune KNN the code is provided below**

### 6.5 SVM

##### Build the model

# Tunning SVM
library(e1071)

base_model_svm_3 <- tune.svm(factor(Churn)~., data =train_svm, 
                cost = 2^(2:8), 
                kernel = "linear") 

summary(base_model_svm_3)

##### Make predictions

base_model <- base_model_svm_3$best.model

prediction_svm_3 = predict(base_model, test_svm)

##### Performance Measure

actual = test_svm$Churn

Get Confusion Matrix

confusionMatrix(data = factor(prediction_svm_3), 
                       reference = factor(actual), 
                       positive = "1")

###### Tabulate Result 

predicted <- as.numeric(prediction_svm_3)
predicted <- if_else(predicted == 2, 1, 0)

table(actual)
table(predicted)

accuracy_score_svm_3 = accuracy(actual,predicted)
precision_score_svm_3 = precision(actual,predicted)
recall_score_svm_3 = recall(actual,predicted)
auc_score_svm_3 = auc(actual,predicted)

Base_Model_svm <- c(accuracy_score_svm_3 , precision_score_svm_3 ,recall_score_svm_3  , auc_score_svm_3)

Tunned_Models$SVM = Base_Model_svm
Tunned_Models

#### Model Comparision

Base_Models

#Conclusion base model of Random forest showed best result than othermodels**

Selected_Models

#Conclusion after performing feature selection on Decision tree showed best result than othermodels**

Tunned_Models

#Conclusion after performing hyperparameter tunning on Logistic Regression it showed best result than other models
