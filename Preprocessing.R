---
title: "Pre-processing of Covid Symptoms data"
author: "Aishwarya Iyer"
date: "11/10/2020"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
```{r}
#Packages required
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(VIM))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(mice))
```


```{r}
#Set up the working directory
DATA.DIR    <- "C:\\Users\\HP\\Desktop\\scientific programming"
RESULTS.DIR <-  "C:\\Users\\HP\\Desktop\\scientific programming"
setwd(DATA.DIR)

#read the file 
covid_symptoms_data<-read.csv("Covid_symptoms.csv")
```

```{r}
#re-writing column names
column_names<-c('Fever','Tiredness','Dry_Cough','Difficulty_in_Breathing','Sore_Throat','No_Symptoms','Pains','Nasal_Congestion',
                'Runny_nose','Diarrhea','None_Experiencing','Age_0_9','Age_10_19','Age_20_24','Age_25_59','Age_60','Gender_Female','Gender_Male',
                'Genger_Transgender','Mild_Severity','Moderate_Severity','No_Severity','Severe','Contact_Dont_Know',
                'No_Contact','Yes_Contact','Country')

#Adding the column names to the data
colnames(covid_symptoms_data)<-column_names
```

```{r}
#Inspecting the data

## Overall look of the dataframe (consists of int-integer, dbl-double(numeric),fct-factor)
glimpse(covid_symptoms_data)

describe(covid_symptoms_data)#for each variable the number fo samples. missing values, distinct values, sum of values and mean of values.
##Negative values, Missing values and values higher than 1 are detected for columns. 
##The data is binary data with 316800 samples and 27 variables which contains one categorical variable of country.
```

```{r}
#Finding abnormal values in the data 
#Values below 0 (negative values)
data_zero<-filter_all(covid_symptoms_data[,1:26], any_vars(. < 0))
#Values above 1
data_positive<-filter_all(covid_symptoms_data[,1:26], any_vars(. > 1))
data_five<-filter_all(covid_symptoms_data[,1:26], any_vars(. ==0.5))
#NA values in the data
na_values<-colSums(is.na(covid_symptoms_data[,1:26]))
```

```{r}
##Replace abnormal values with NaN
covid_symptoms_data <- covid_symptoms_data %>% mutate(Diarrhea = replace(Diarrhea, which(Diarrhea<0), NaN),
                        None_Experiencing = replace(None_Experiencing, which(None_Experiencing<0), NaN),
                        `Age_0_9` = replace(`Age_0_9`, which(`Age_0_9`<0), NaN),
                        `Age_10_19` = replace(`Age_10_19`, which(`Age_10_19`<0), NaN),
                        `Age_20_24` = replace(`Age_20_24`, which(`Age_20_24`<0), NaN),
                        `Age_25_59` = replace(`Age_25_59`, which(`Age_25_59`<0), NaN),
                        Age_60 = replace(Age_60, which(Age_60<0), NaN),
                        Gender_Female= replace(Gender_Female, which(Gender_Female<0),NaN),
                        Gender_Male = replace(Gender_Male, which(Gender_Male<0), NaN),
                        Genger_Transgender = replace(Genger_Transgender, which(Genger_Transgender<0), NaN),
                        Mild_Severity = replace(Mild_Severity, which(Mild_Severity<0), NaN),
                        Moderate_Severity= replace(Moderate_Severity, which(Moderate_Severity<0), NaN),
                        No_Severity = replace(No_Severity, which(No_Severity<0), NaN),
                        Severe = replace(Severe, which(Severe<0), NaN),
                         Moderate_Severity = replace(Moderate_Severity, which(Moderate_Severity > 1), NaN))
```

```{r}
#Count number of missing values per column
colSums(is.na(covid_symptoms_data[,1:26]))


#Percentage of mnissing values
sapply(covid_symptoms_data[,1:26], function(df){
  sum(is.na(df) ==TRUE)/length(df);
})
```

```{r,fig.height=9,fig.width=20,dpi=100}
#visulaise the missing values

plot <- aggr(covid_symptoms_data[,1:26], col=c('navyblue','yellow'),
             numbers=TRUE, sortVars=TRUE,
             labels=names(covid_symptoms_data[,1:26]),font=2, cex.axis=0.5,
             gap=3, ylab=c("Missing data","Pattern"))
```

```{r}
#ploting the missing data visulaisation
gg_miss_var(covid_symptoms_data) + labs(y = "Look at all the missing ones",cex.labels=1.2)+
  theme(axis.text.x = element_text(size = "11"),
        axis.text.y = element_text(size = "11"),
        axis.title.x.bottom = element_text(size="11"))+
  ggtitle("Missing data visualisation")+
  theme(plot.title = element_text(hjust = 0.5,size=30))
ggsave("Missing data visualisation.png")#save the plot
```

```{r}
#Imputing the missing values
imputed_mice_pmm<-mice(covid_symptoms_data,method='pmm',maxit=5,m=1)#m is numbe rof imputation , maxit=no of iterations
summary(imputed_mice_pmm)
save(imputed_mice_pmm, file = "imputed_mice_pmm.RData")
```


```{r}
#Plot the distribution of values for observed and imputed values for each variable.
densityplot(imputed_mice_pmm)

#The distribution of imputed values fit well with observed values.
```

```{r}
#imputed dataset
imputed_data<-complete(imputed_mice_pmm,1)

```

```{r}
#export the imputed dataset as csv file
write.csv(imputed_data,"imputed data .csv",row.names=FALSE)


```
