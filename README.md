# UFNLP

This github repository contains the code to create a package called "UFNLP". This package was developed by a UF team working on a NLP project for Accenture Federal.
These functions help expedite certain aspects of an NLP project including initial data analysis, training a model, predicting with the model, and doing analysis on the model's predictions. These functions were made to work on a dataframe in R, so if you wish to use them most of your inputs should be in the form of vectors. 

To download this package into R, you want to make sure you have the devtools package installed and then run the two lines below: 

library(devtools)

devtools::install_github("JackOgozaly/UFNLP")

## text_clean 

This function takes a column of text and a dataframe, cleans the text, re-attaches it to the dataframe, and outputs a new dataframe called tidytext_df. 

## balanced_data 

This function takes a vector of labels and a dataframe, splits into train and test sets, upsamples or downsamples the train data, and outputs a new datadrame called balanced_df and the row to stop training off of. 

## svm_model 
This function takes a vector of text, a vector of labels, and the row at which to stop giving the model train data, and then generates an SVM (support vecotor machine) model, a confusion matrtix plot, and also more details confusion matrix statistics, and the accuracy of the model. 

## svm_predict_input
This function converts a vector of text into a document term matrix so that the model can make predictions off of it. If you are using the svm_confidence_table function to make predictions, you do not need to use this function. 

## svm_confidence_table 
If you don't have labels, this function takes a text input and an SVM model, and generates a dataframe with predictionss for the text input and also the confidence level in each prediction. If you have labels for this text input, this function will then analyze how well the model's confidence aligns with it being right. (Specifically, it will show a logistic distrubution over a histogram of the confidence level distribution.) 

## conf_interval_calculator
This function takes a vector of labels and the model's confidence in that label and calculates the weighted count for each label and the confidence interval for where the true count occurs. 

## vendor dataframe generator 
This function generates a dataframe that has the top X for each label class (where X is any other variable, such as vendor, location, etc). NOTE: This function generates data in a wide format and is therefore not reccomended in most instances. If you wish to achieve similar results in a tidy format please use: 

library(dplyr)

your_output <- your_dataframe %>%
  group_by(class) %>%
  count(VariableX)
  
## ngram 

This function takes a vector of text, cleans the text, and then creates ngrams for that text. You can specify if you want a unigram, bigram, or trigram by specificy what n=. Function will automatically display a plot of the most common ngrams. Below is an example bigram generated from tweets about airlines. 

![ngram example](https://user-images.githubusercontent.com/72467438/114043491-5410e400-9854-11eb-88f3-eab3dda3b4f7.jpg)
  
