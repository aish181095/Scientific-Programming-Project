Predicting COVID Severity using Classification and Regression Random forest Model.

In this project COVID symptoms dataset (https://www.kaggle.com/iamhungundji/covid19-symptoms-checker) was used to predict the severity classes using Randomforest. This project aims to understand effect of considering the class labels as categorical vs continous labels on the accuracy of classification model. 
For this purpose, the dataset was preprocessed using Preprocessing.Rmd script. Missing values were detected and imputed in this script and exported. The imputed dataset is provided in this repository for running the Classifier_script.Rmd. 
The Classifier_script.Rmd was used implement classification and regression model using Ranger. The classified labels were then compared with the regression labels to identify the regression values for misclassified labels.

This project is part of Scientific Programming course for Masters in Systems biology program at Maastricht University,The Netherlands.
