---
title: "Classification and Regression Model using Ranger"
author: "Aishwarya Iyer"
date: "25/10/2020"
output:
  pdf_document: default
  html_document: default
  #Description: The COVID symptoms dataset contains 23 predictor variables and 24th column containing four levels (No Severity, Mild Severity, Moderate Severity and Severe). These levels are treated as catergorical labels and Ranger (faster version of Rnadom Forest) classification is implemeted to predict the four levels. Additionally, the levels are converted into continous scale ( converting four categorical levels from range 1 to 4). This is done to mimick the contonuity between the classes. Since the levels are not independent of each other, which means an individual from No Severity can enter Mild Severity and an individual can be between two level of severity). In the end the resulst from classification and regression prediction using ranger are compared for the misclassfied labels in Ranger Classification Prediction.
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
```{r}
#Required packages
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(PRROC))
suppressPackageStartupMessages(library(ggplot2))
```

```{r}
#Set up the working directory where the imputed dataset is saved
DATA.DIR    <- "C:\\Users\\HP\\Desktop\\scientific programming"
RESULTS.DIR <-  "C:\\Users\\HP\\Desktop\\scientific programming"
setwd(DATA.DIR)

```

```{r}

#import imputed data 
imputed_data<-read.csv("imputed data 1.csv")

```

```{r}
##Merging classes into class column (No_severity, Mild_severity,Moderate_severity, Severity into one class)
##Multiplying Mild_Severity (with 2),Moderate_severity(with 3) and severity(with 4)
imputed_data$Mild_Severity<- 2*(imputed_data$Mild_Severity)
imputed_data$Moderate_Severity<-3*(imputed_data$Moderate_Severity)
imputed_data$Severe<-4*(imputed_data$Severe)


#combine column in covid symptoms data to one class column
imputed_data$class<-as.factor(imputed_data$Mild_Severity+imputed_data$Moderate_Severity+imputed_data$No_Severity+imputed_data$Severe)

#Remove the redundant columns from dataframe
imputed_data$Mild_Severity<-NULL
imputed_data$Moderate_Severity<-NULL
imputed_data$No_Severity<-NULL
imputed_data$Severe<-NULL


#Convert the levels of imputed data to categorical levels (for classification)
levels(imputed_data$class) <- c("No_Severity", "Mild_Severity", "Moderate_Severity","Severity")
```

```{r}
# summarize the class distribution in the imputed data
percentage <- prop.table(table(imputed_data$class)) * 100
cbind(freq=table(imputed_data$class), percentage=percentage) #the classes/levels are balanced
```

```{r}
#Split the imputed data into training data. validation data and test data (holdout)
library(caret)
set.seed(100)
index <- createDataPartition(imputed_data$class, p = 0.7, list = FALSE)
train_data <- imputed_data[index, ]
test_data  <- imputed_data[-index, ]

set.seed(101)
validation_index<-createDataPartition(train_data$class, p = 0.7, list = FALSE)
train_data <- train_data[validation_index, ]
validation_data  <- train_data[-validation_index, ]
```

```{r}

#summarise training dataset, validation data and test dataset class levels

percentage <- prop.table(table(train_data$class)) * 100
cbind(freq=table(train_data$class), percentage=percentage)

percentage <- prop.table(table(validation_data$class)) * 100
cbind(freq=table(validation_data$class), percentage=percentage)

percentage <- prop.table(table(test_data$class)) * 100
cbind(freq=table(test_data$class), percentage=percentage)

#The class levels for all datasets are balanced.
```


#################################
Classification using Ranger
#################################
##Model Optimisation: The Model optimisation was performed using 1000 trees.Given the computational difficulty in running the optimisation mode for number of trees a higher number of trees are selected. The code for optimisation is shown in this script but was not run. Further, 'caret' R package allows to optimise the following parameters for Ranger Model: mtry (Number of variables considered for tree split at each node), Minimal Node size (determines the depth of the tree, higher values-more depth) and splitrule. For classification 'gini' is default for ranger. For computational feasability given the large training dataset  the model was first optimised for mtry values and then optimised for minimal node size. 
The optimised model was then used to predict for validation dataset and the ROC curves for the prediction was plotted. The same was doen for Test dataset. The ROC plots of both Test and Vlaidation dataset were comparable showing consistency of model prediction. The accuracy of the model is around 24 % which is indicates poor performance of teh model. This low value of accuracy is due to the inherent quality of the dataset. This is beacuse this datatset has been synthetically generated and therefore it assigns equal weigth to each variable and categories. Normal population features are not represented by this daya and hence the prediction itself is quite poor. 

#Optimising model parameters with training dataset
```{r}
##Optimising number of trees  (was not run, took a lot of time to run)
#model_grid_trees <- expand.grid(
  num.trees=c(50,100,500,1000,2000)
  
  )

# total number of combinations
#nrow(model_grid_trees)
```

```{r}
#set.seed(34)
#for(i in 1:nrow(model_grid_trees)) {
# Fit a model for optimising trees
#model_opt_trees <- caret::train(as.factor(class) ~ ., data = train_data,
                     method = "ranger",
                     num.trees = model_grid_trees$num.trees[i],
                     importance = "impurity",
                     trControl = trainControl(method = "oob", number = 3))
#}
#save(model_opt_trees,file="model_opt_trees.RData")
```

```{r}

#Ploting to idenitfy optimal number of trees 
#plot(model_opt_trees)
```

```{r}
#optimising mtry (number of variables)

# hyperparameter grid search
model_grid_mtry <- expand.grid(
  mtry      = c(1,4,5,10,14,20),
  splitrule="gini",
  min.node.size  = c(1000)
  
  )

# total number of combinations
nrow(model_grid_mtry)
## [1] 96
```


```{r}
set.seed(34)
# Fit a model for mtry (number of variables for node split)
model_opt_mtry <- caret::train(class ~ ., data = train_data,
                     method = "ranger",
                     tuneGrid = model_grid_mtry,
                     num.trees = 1000,
                     importance = "impurity",
                     trControl = trainControl(method = "oob", number = 3))


#save the model as R.Data file
save(model_opt_mtry,file="model_opt_mtry.RData")

#Import the model 
lnames = load(file = "model_opt_mtry.RData");

```

```{r}
model_opt_mtry$bestTune#best tuning parameters for mtry

#ploting Out of bag Accuracy for mtry values in hyperparameter grid 
plot(model_opt_mtry,main="Optimising number of variables available for splitting at each tree node",cex.labels=2)

## mtry value of 1 was selected for further optimsing the minimal node split.
```


```{r}
#optimising minimum number of nodes
# hyperparameter grid search
model_grid_nodes <- expand.grid(
  mtry      = c(1),
  splitrule="gini",
  min.node.size  = c(500,1000,2000,5000,10000,20000,50000,100000,150000)
  
  )

# total number of combinations
nrow(model_grid_nodes)
```

```{r}
set.seed(34)
# Fit a model
model_opt_nodes <- caret::train(class~ ., data = train_data,
                     method = "ranger",
                     tuneGrid = model_grid_nodes,
                     num.trees = 1000,
                     importance = "impurity",
                     trControl = trainControl(method = "oob", number = 3))

#save the model as R.Data file
save(model_opt_nodes,file="model_opt_nodes.RData")

#Import the model 
lnames = load(file = "model_opt_nodes.RData");

```

```{r}
model_opt_nodes$bestTune#best tuning parameters for mtry

#ploting Out of bag Accuracy for minimal node size values in hyperparameter grid 
plot(model_opt_nodes,main="Optimising minimal node size for model")

## Minimal node split of 100000 was used for the optimised Ranger classification Model.
```

```{r}
#Optimized parameter grid 

# hyperparameter grid search
model_opt_parameters <- expand.grid(
  mtry      = c(1),
  splitrule="gini",
  min.node.size  = c(100000)
  
  )

# total number of combinations
nrow(model_opt_parameters)
```

```{r}
#Optimised model for ranger classification

set.seed(34)
# Fit a model
optimised_model <- caret::train(class ~ ., data = train_data,
                     method = "ranger",
                     tuneGrid = model_opt_parameters,
                     num.trees = 1000,
                     importance = "impurity",
                     metric="Accuracy",
                     trControl = trainControl(method = "cv", number = 3,savePredictions = T,classProbs = T))

#save the model as R.Data file
save(optimised_model,file="optimised_model.RData")

#Import the model 
lnames = load(file = "optimised_model.RData");

```

```{r}
#variable importance for classification
classification_importance<-varImp(optimised_model,scale = FALSE)

#Ploting the importance of varaiable.
plot(classification_importance,main="Variable importance for Classification Ranger Model")
```
##Checking the Sensitivity and Specicifity of prediction using validation dataset
```{r}
#predicting validation data

#calculating probabilties for each class 
predict_validation<-predict(optimised_model,validation_data,type="prob")

#predicting class for validation dataset
predict_valid<-predict(optimised_model,validation_data)

#Creating a dataframe of four class levels
validation_class<-data.frame(validation_data$class)
sep_validation <- dummyVars(~ ., validation_class, fullRank = FALSE)#seperating the class levels 
seperate_validation_class <- predict(sep_validation, validation_class) %>% as.data.frame() #dataframe with separate classes 
```



```{r}

#calculating True Postive rate and False Positive Rates for each class probability distribution for Validation dataset
PRROC_no_severity <- roc.curve(scores.class0 = predict_validation$No_Severity, weights.class0=seperate_validation_class$validation_data.class.No_Severity,
                       curve=TRUE)

PRROC_mild_severity <- roc.curve(scores.class0 = predict_validation$Mild_Severity, weights.class0=seperate_validation_class$validation_data.class.Mild_Severity,
                       curve=TRUE)

PRROC_moderate_severity <- roc.curve(scores.class0 = predict_validation$Moderate_Severity, weights.class0=seperate_validation_class$validation_data.class.Moderate_Severity,
                       curve=TRUE)

PRROC_severity <- roc.curve(scores.class0 = predict_validation$Severity, weights.class0=seperate_validation_class$validation_data.class.Severity,
                       curve=TRUE)

```


```{r}

#ploting ROC curves for each class 
par(mfrow=c(2,2))
plot(PRROC_no_severity,main="ROC curve for No Severity",xlab="1-Specificity")
plot(PRROC_mild_severity,main="ROC curve for Mild Severity",xlab="1-Specificity")
plot(PRROC_moderate_severity,main="ROC curve for Moderate Severity",xlab="1-Specificity")
plot(PRROC_severity,main="ROC curve for Severity",xlab="1-Specificity")
```




```{r}
#plotting the confusion matrix to compare the predicted and reference output for the validation data
table <- data.frame(confusionMatrix(table(predict_valid,validation_data[,24]))$table)
colnames(table)<-c("Observed","Prediction","Freq")
plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Observed, "good", "bad")) %>%
  group_by(Observed) %>%
  mutate(prop = Freq/sum(Freq))

#ploting the confusion matrix for validation dataset
Confusion_matrix<-ggplot(data = plotTable, mapping = aes(x = Observed, y = Prediction, fill = goodbad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "blue", bad = "yellow")) +
  theme_bw() +
  xlim(rev(levels(table$Observed))
  )
print(Confusion_matrix + ggtitle("Confusion Matrix for Validation dataset"))

```

```{r}
#plotting the confusion matrix to compare the predicted and reference output for the validation data
table <- data.frame(confusionMatrix(table(validation_data[,24],validation_data[,24]))$table)
colnames(table)<-c("Observed","Prediction","Freq")
plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Observed, "good", "bad")) %>%
  group_by(Observed) %>%
  mutate(prop = Freq/sum(Freq))

#Ploting confusionmatrix for ideal classification for validation dataset
Confusion_matrix<-ggplot(data = plotTable, mapping = aes(x = Observed, y = Prediction, fill = goodbad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "blue", bad = "yellow")) +
  theme_bw() +
  xlim(rev(levels(table$Observed)))
print(Confusion_matrix + ggtitle("Confusion Matrix for ideal classification in validation dataset"))

```
##Prediction using Test dataset and comparing the sensitivity and specicificity using ROC curve
```{r}
##Prediction for test dataset

#calculating the probabilities for test dataset
predict_test_prob<-predict(optimised_model,test_data,type="prob")

#Predict for Test dataset
predict_test<-predict(optimised_model,test_data)

#Creating a dataframe of four class levels
test_class<-data.frame(test_data$class)
sep_test <- dummyVars(~ ., test_class, fullRank = FALSE)#seperating the class levels
seperate_test_class <- predict(sep_test, test_class) %>% as.data.frame()#dataframe with separate classes 
```



```{r}
library(PRROC)
#calculating True Postive rate and False Positive Rates for each class probability distribution for Test dataset

PRROC_no_severity_test <- roc.curve(scores.class0 = predict_test_prob$No_Severity, weights.class0=seperate_test_class$test_data.class.No_Severity,
                       curve=TRUE)
PRROC_mild_severity_test <- roc.curve(scores.class0 = predict_test_prob$Mild_Severity, weights.class0=seperate_test_class$test_data.class.Mild_Severity,
                       curve=TRUE)
PRROC_moderate_severity_test <- roc.curve(scores.class0 = predict_test_prob$Moderate_Severity, weights.class0=seperate_test_class$test_data.class.Moderate_Severity,
                       curve=TRUE)
PRROC_severity_test <- roc.curve(scores.class0 = predict_test_prob$Severity, weights.class0=seperate_test_class$test_data.class.Severity,
                       curve=TRUE)
```

```{r}
##Ploting the ROC curve for Test dataset

par(mfrow=c(2,2))
plot(PRROC_no_severity_test,main="ROC curve for No Severity",xlab="1-Specificity")
plot(PRROC_mild_severity_test,main="ROC curve for Mild Severity",xlab="1-Specificity")
plot(PRROC_moderate_severity_test,main="ROC curve for Moderate Severity",xlab="1-Specificity")
plot(PRROC_severity_test,main="ROC curve for Severity",xlab="1-Specificity")
```



```{r}
#plotting the confusion matrix to compare the predicted and reference output for the test data
table <- data.frame(confusionMatrix(table(predict_test,test_data[,24]))$table)
colnames(table)<-c("Observed","Prediction","Freq")
plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Observed, "good", "bad")) %>%
  group_by(Observed) %>%
  mutate(prop = Freq/sum(Freq))

Confusion_matrix<-ggplot(data = plotTable, mapping = aes(x = Observed, y = Prediction, fill = goodbad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "blue", bad = "yellow")) +
  theme_bw() +
  xlim(rev(levels(table$Observed)))
print(Confusion_matrix + ggtitle("Confusion Matrix for classification in Test dataset"))


```
```{r}
#plotting the confusion matrix to compare the predicted and reference output for the test data
table <- data.frame(confusionMatrix(table(test_data[,24],test_data[,24]))$table)
colnames(table)<-c("Observed","Prediction","Freq")
plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Observed, "good", "bad")) %>%
  group_by(Observed) %>%
  mutate(prop = Freq/sum(Freq))

#Ploting confusionmatrix for ideal classification for validation dataset
Confusion_matrix<-ggplot(data = plotTable, mapping = aes(x = Observed, y = Prediction, fill = goodbad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "blue", bad = "yellow")) +
  theme_bw() +
  xlim(rev(levels(table$Observed)))
print(Confusion_matrix + ggtitle("Confusion Matrix for ideal classification in Test dataset"))

```





##################################
Regression using Ranger
##################################
#Regression was performed using Ranger by converting the categorical labels to continous scale ( 1,2,3 and 4). The optimisation was performed the same way as for classification model. Instead of focusing on the accuracy from this model, the prediction from this model was used to analyse the misclassified laels from classification model prediction. This was done to identify the intermediate ranges for the misclassified labels and if any trend in ranges were visisble.
```{r}
#For regression the class levels need to be of continous range.

#change levels in dataframe to continous scale
levels(train_data$class)<-c(1,2,3,4)
levels(validation_data$class)<-c(1,2,3,4)
levels(test_data$class)<-c(1,2,3,4)

```

```{r}

#summarise training dataset, validation data and test dataset class levels

percentage <- prop.table(table(train_data$class)) * 100
cbind(freq=table(train_data$class), percentage=percentage)

percentage <- prop.table(table(validation_data$class)) * 100
cbind(freq=table(validation_data$class), percentage=percentage)

percentage <- prop.table(table(test_data$class)) * 100
cbind(freq=table(test_data$class), percentage=percentage)

#The class levels for all datasets are balanced.
```

```{r}
#Optimising the model for mtry and minimal node size
set.seed(35)
# Fit a model
model_reg <- caret::train(as.numeric(class) ~ ., data = train_data,
                     method = "ranger",
                     num.trees = 1000,
                     importance = "impurity",
                     trControl = trainControl(method = "oob", number = 3))

#Save the R.Data file for the model
save(model_reg,file="model_reg.RData")

#Import the R.Data file

lnames = load(file = "model_reg.RData");

```

```{r}

model_reg$bestTune #Best tuning parameters for mtry 
plot(model_reg,main="Optimising the number of variables available for splitting at each tree node for regression model")
#mtry value of 1 and extratrees splitrule are selected for minimal node size optimisation
```

```{r}
#optimising mtry (number of variables) for regression
# hyperparameter grid search
reg_grid_mtry <- expand.grid(
  mtry      = c(1,2),
  splitrule="extratrees",
  min.node.size  = c(500,1000,2000,5000,10000,50000,100000)
  
  )

# total number of combinations
nrow(reg_grid_mtry)
## [1] 96
```

```{r}
set.seed(34)
# Fit a model
model_reg_mtry <- caret::train(as.numeric(class) ~ ., data = train_data,
                     method = "ranger",
                     tuneGrid = reg_grid_mtry,
                     num.trees = 1000,
                     importance = "impurity",
                     trControl = trainControl(method = "oob", number = 3))

#saving the model as R.Data file
save(model_reg_mtry,file="model_reg_mtry.RData")

#Import the R.Data file
lnames = load(file = "model_reg_mtry.RData");

```

```{r}
model_reg_mtry$bestTune #best tuning parameters for mtry and minimal node size
plot(model_reg_mtry,main="Optimising the minimal node size for regression model")


print("minimal node size of 100000 and extratrees splitrule are selected for optimised model for regression")
```

```{r}
#Optimal parameters for regression model
# hyperparameter grid search
opt_reg_grid <- expand.grid(
  mtry      = c(1),
  splitrule="extratrees",
  min.node.size  = c(100000)
  
  )

# total number of combinations
nrow(opt_reg_grid)
## [1] 96
```


```{r}
set.seed(34)
# Fit a model
opt_model_reg <- caret::train(as.numeric(class) ~ ., data = train_data,
                     method = "ranger",
                     tuneGrid = opt_reg_grid,
                     num.trees = 1000,
                     importance = "impurity",
                     trControl = trainControl(method = "oob", number = 3,savePredictions = T))
#save the optimised regression model as a R.Data file
save(opt_model_reg ,file="opt_model_reg .RData")

#load the R.Data file
lnames = load(file = "opt_model_reg .RData");

```

```{r}
#variable importance for regression
regression_importance<-varImp(opt_model_reg,scale = FALSE)

plot(regression_importance,main="Variable Importance for Regression Ranger Model")
```



```{r}
#predicting validation test for regression model
predict_reg_validation<-data.frame(predict(opt_model_reg,validation_data[,-24]))
predict_reg_validation$pred_class<-predict_valid #adding the prediction from the classification model
levels(predict_reg_validation$pred_class)<-c(1,2,3,4) #converting the classes in the classification column to continous class
predict_reg_validation$true_class<-validation_data$class #adding the true classes from the validation dataset
```


```{r}
#Calculate ROC curve for Regression model 

PRROC_reg_validation<- roc.curve(scores.class0 = predict_reg_validation$predict.opt_model_reg..validation_data....24.., weights.class0=as.numeric(validation_data$class),
                       curve=TRUE)

#Ploting the ROC curve for Regression model
plot(PRROC_reg_validation,main="ROC curve for Regression Ranger Model",xlab="1-Specificity")
```


```{r}
#Identify classes that are misclassified for classification model using ranger and subset the corresponding regression model prediction
#subset rows having same value
same_values<-subset(predict_reg_validation,pred_class==true_class)

#Subset rows having different values for prediction using classification model and true prediction class
diff_values<-subset(predict_reg_validation,pred_class!=true_class)
```

```{r}
#Subset and create dataframe for every class using the true class column
values_for_class_1<-subset(diff_values,true_class==1)#for class 1
values_for_class_2<-subset(diff_values,true_class==2)#for class 2
values_for_class_3<-subset(diff_values,true_class==3)#for class 3
values_for_class_4<-subset(diff_values,true_class==4)#for class 3

#Add the column names for each dataframe
colnames(values_for_class_1)<-c("Regression_prediction","Classification_prediction","true_class")
colnames(values_for_class_2)<-c("Regression_prediction","Classification_prediction","true_class")
colnames(values_for_class_3)<-c("Regression_prediction","Classification_prediction","true_class")
colnames(values_for_class_4)<-c("Regression_prediction","Classification_prediction","true_class")

#Combine them to form one dataframe
summary_covid<-data.frame(rbind(values_for_class_1,values_for_class_2,values_for_class_3,values_for_class_4))

#Conver the levels of true class to categorical classes
levels(summary_covid$true_class) <- c("No_Severity", "Mild_Severity", "Moderate_Severity","Severity")
levels(summary_covid$Classification_prediction) <- c("No_Severity", "Mild_Severity", "Moderate_Severity","Severity")


```

```{r}

#Ploting the prediction values from regression with the misclassified class prediction
summary_covid %>%
  ggplot(aes(x = Classification_prediction,
             y = Regression_prediction,
             fill = Classification_prediction))+
  geom_violin()+ stat_summary(fun.data=mean_sdl, mult=2, 
             geom="pointrange", color="yellow")+
  ggtitle("Violin plot indicating the predicted regression values for \n Misclassified labels for validation data")
ggsave("Violin plot indicating the predicted regression values for Misclassified labels for validation data.png")#save image

```

```{r}

#Ploting the prediction values from regression with the true class prediction
summary_covid %>%
  ggplot(aes(x = true_class,
             y = Regression_prediction,
             fill = true_class))+
  geom_violin()+ stat_summary(fun.data=mean_sdl, mult=2, 
             geom="pointrange", color="yellow")+
   ggtitle("Violin plot indicating the predicted regression values for \n True labels for validation data")
ggsave("Violin plot indicating the predicted regression values for True labels for validation data .png")#save image

```


```{r}
#predicting test dataset
predict_reg_test<-data.frame(predict(opt_model_reg,test_data[,-24]))
predict_reg_test$Classification_prediction<-predict_test
levels(predict_reg_test$Classification_prediction)<-c(1,2,3,4)

predict_reg_test$true_class<-test_data$class
```

```{r}
#Calculating TPR and FPR for regression prediction

PRROC_reg_test<- roc.curve(scores.class0 = predict_reg_test$predict.opt_model_reg..test_data....24.., weights.class0=as.numeric(test_data$class),
                       curve=TRUE)

#ploting the ROC curve for regression model

plot(PRROC_reg_test,main="ROC curve for Test dataset for Regression Ranger Model",xlab="1-Specificity")
```
```{r}
#Identify classes that are misclassified for classification model using ranger and subset the corresponding regression model prediction for test dataset
#subset rows having same value
same_values_test<-subset(predict_reg_test,Classification_prediction==true_class)
#subset rows having different values
diff_values_test<-subset(predict_reg_test,Classification_prediction!=true_class)
```

```{r}
#Subset and create dataframe for every class using the true class column
values_for_class_1<-subset(diff_values_test,true_class==1)#for class 1
values_for_class_2<-subset(diff_values_test,true_class==2)#for class 2
values_for_class_3<-subset(diff_values_test,true_class==3)#for class 3
values_for_class_4<-subset(diff_values_test,true_class==4)#for class 4

#adding column names to the dataframe
colnames(values_for_class_1)<-c("Regression_prediction","Classification_prediction","true_class")
colnames(values_for_class_2)<-c("Regression_prediction","Classification_prediction","true_class")
colnames(values_for_class_3)<-c("Regression_prediction","Classification_prediction","true_class")
colnames(values_for_class_4)<-c("Regression_prediction","Classification_prediction","true_class")

#Merge to form combined dataframe
summary_covid_test<-data.frame(rbind(values_for_class_1,values_for_class_2,values_for_class_3,values_for_class_4))
levels(summary_covid_test$true_class) <- c("No_Severity", "Mild_Severity", "Moderate_Severity","Severity")#convert the levels in true class column to continous classes
levels(summary_covid_test$Classification_prediction) <- c("No_Severity", "Mild_Severity", "Moderate_Severity","Severity")

```

```{r}

#Ploting the prediction values from regression with the misclassified class prediction
summary_covid_test %>%
  ggplot(aes(x = Classification_prediction,
             y = Regression_prediction,
             fill = Classification_prediction))+
  geom_violin()+ stat_summary(fun.data=mean_sdl, mult=2, 
             geom="pointrange", color="yellow")+
  ggtitle("Violin plot indicating the predicted regression values for \n  Misclassified labels for test data")
ggsave("Violin plot indicating the predicted regression values for Misclassified labels for test data.png")#save image

```

```{r}
summary_covid_test %>%
  ggplot(aes(x = true_class,
             y = Regression_prediction,
             fill = true_class))+
  geom_violin()+ stat_summary(fun.data=mean_sdl, mult=2, 
             geom="pointrange", color="yellow")+
   ggtitle("Violin plot indicating the predicted regression values for True labels for test data")
ggsave("Violin plot indicating the predicted regression values for True labels for validation data.png")#save image

```
##from observing the plot for misclassified labels vs the predicted regression values for validation and test dataset we see that the range of predicted regression values is around 2.5 for all classes. Additionally we observe and increase in trend across the severity classes. This indicates the range of values indicates the increasing in severity for predicted regression values ( although these values are between 2.5).
#We also observe that the model sensistivity and specificity is consistent (although the overall accuracy for both model is low~25%) for validation as well as test dataset ( shown by ROC curves) for both classification and regression model for ranger. This shows the reproducibility of the model for unseen datasets (datasets it was not trained on).  
