# UFNLP

This github repository contains the code to create a package called "UFNLP". This package was developed by a UF team working on a NLP project for Accenture Federal.
These functions help expedite certain aspects of an NLP project including initial data analysis, training a model, predicting with the model, and doing analysis on the model's predictions. These functions were made to work on a dataframe in R, so if you wish to use them most of your inputs should be in the form of vectors. 

# text_clean 

This function takes a column of text and a dataframe, cleans the text, re-attaches it to the dataframe, and outputs a new dataframe called tidytext_df. 

# balanced_data 

This function takes a vector of labels and a dataframe, splits into train and test sets, upsamples or downsamples the train data, and outputs a new datadrame called balanced_df and the row to stop training off of. 

# svm_model 
