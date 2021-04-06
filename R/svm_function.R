#The files contain the various functions used to train SVM NLP Model


#Built on R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Package Versions:
#tidytext_0.3.0   tm_0.7-8         NLP_0.2-1        popbio_2.7       caret_6.0-86     ggplot2_3.3.3
#lattice_0.20-41  RTextTools_1.4.3 SparseM_1.78     e1071_1.7-4


#This function cleans our text in a standard way
text_clean <- function(text_input, # should be a column from a dataframe
                       dataframe #dataframe you want the clean text to be attached to
){
  #These lines clean the text input
  tidy_text <- text_input
  tidy_text <- tm::removePunctuation(tidy_text)
  tidy_text <- tolower(tidy_text)
  tidy_text <- tm::removeNumbers(tidy_text) #This line is optional
  stop_words <- as.data.frame(tm::stopwords())
  tidy_text <- tm::removeWords(tidy_text, stop_words$`tm::stopwords()`)
  #These lines create a new dataframe, attaches the clean text, and exports
  tidy_text_df <- as.data.frame(dataframe)
  tidy_text_df$tidy_text <- tidy_text
  assign("tidytext_df", tidy_text_df, envir = globalenv())
}


#This function upsamples or downsamples our data
balanced_data <- function(dataframe, #Dataframe
                          labels, #Labels to be balanced
                          train_percent = .75, #What percent of the data do you wish to train on?
                          upsample= T) #If true upsamples,if false downsamples
{
  set.seed(42) #Set seed for repeatability
  #Makes a datafrmae with our x and y variables, and then shuffles the rows
  df <- as.data.frame(dataframe)
  df$label <- as.factor(labels)
  rows <- sample(nrow(df))
  df <- df[rows, ]
  rownames(df) <- NULL
  #Calculates what the train and test data should be and splits them
  observations <- as.numeric(nrow(df))
  train_upper_limit <- train_percent * observations
  train_upper_limit <- ceiling(train_upper_limit)
  train_data <- df[1:train_upper_limit, ]
  test_data <- df[train_upper_limit:observations, ]

  if(upsample==T){ #If user selects Upsample=T this code runs
    #Upsample train data and then re-attach test data
    new.df <- as.data.frame(caret::upSample(train_data, train_data$label))
    number <- nrow(new.df)
    new.df <- new.df[,-ncol(new.df)]
    colnames(test_data) <- colnames(new.df)
    new.df <- rbind(new.df, test_data)
    assign("balanced_df", new.df, envir = globalenv())
    cat(sprintf("The row to stop training at is: %s\n", number))
  }

  else{
    #downsamples data and then re-attaches test data
    new.df <- as.data.frame(caret::downSample(train_data, train_data$label))
    number <- nrow(new.df)
    new.df <- new.df[,-ncol(new.df)]
    colnames(test_data) <- colnames(new.df)
    new.df <- rbind(new.df, test_data)
    assign("balanced_df", new.df, envir = globalenv())
    cat(sprintf("The row to stop training at is: %s\n", number))
  }
}

#This function trains and evaluates an SVM model

svm_model <- function(input, #column you want to predict off of
                      labels, #column you want to predict
                      train_row, #What row # does train data stop at?
                      plot=T,
                      cost= 1)
{
  #Before the model can run the data has to be transformed into a DTM in
  #a container
  data <- as.data.frame(labels)
  colnames(data) <- c("SVM_LABEL")
  dtMatrix <- RTextTools::create_matrix(input)
  container <-RTextTools::create_container(dtMatrix, data$SVM_LABEL,
                                trainSize=1:train_row, virgin=FALSE)
  #Creates the SVm model
  model <- RTextTools::train_model(container,
                       "SVM",
                       kernel="linear",
                       cost= cost)
  #Separates the test data and converts it into a DTM in a container
  lower <- as.numeric(train_row + 1)
  test_data <- data[lower:nrow(data),]
  predictionData <- input[lower:nrow(data)]
  predMatrix <- RTextTools::create_matrix(predictionData, originalMatrix=dtMatrix)
  predSize = length(predictionData);
  predictionContainer <- RTextTools::create_container(predMatrix, labels=rep(0,predSize),
                                          testSize=1:predSize, virgin=FALSE)
  #Runs the model on the transformed test data
  results <- RTextTools::classify_model(predictionContainer, model)
  SVM_results <- as.data.frame(results) #make the results into a dataframe
  #Create a column with the true labels
  SVM_results$actual <- labels[lower:nrow(data)]
  #Create a new column that is a boolean variable that assesses if the model is right
  SVM_results$is.match <- ifelse(SVM_results$SVM_LABEL==
                                   SVM_results$actual, 1 , 0)
  #Custom function which conerts a decimal to a percent
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
  }
  #The mean of the is.match is our accuracy, and we convert it to a percent
  accuracy <- percent(mean(SVM_results$is.match))
  #Print out a message with the model accuracy
  cat(sprintf("The model accuracy is: %s\n", accuracy))
  #create a confusion matrix
  SVM_results$SVM_LABEL <- as.factor(SVM_results$SVM_LABEL)
  SVM_results$actual <- as.factor(SVM_results$actual)
  cfm <- as.data.frame(table(SVM_results$SVM_LABEL, SVM_results$actual))
  cfm_stats <- caret::confusionMatrix(SVM_results$SVM_LABEL, SVM_results$actual,
                               positive = "pos")
  #Export the classifier, DTM, and confusion matrix stats
  assign("svm_classifier", model, envir=globalenv())
  assign("dtMatrix", dtMatrix, envir=globalenv())
  assign("confusion_matrix_stats", cfm_stats, envir=globalenv())
  #if the user wants a plot this code runs
  if(plot == TRUE){
    #Plots Predicted vs Actual labels from our test data
    print(ggplot2::ggplot(data = cfm,
                 mapping = ggplot2::aes(x = Var1,
                               y = Var2)) +
            ggplot2::geom_tile(ggplot2::aes(fill = Freq)) +
            ggplot2::geom_text(ggplot2::aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
            ggplot2::scale_fill_gradient(low = "blue",
                                high = "red",
                                trans = "log") +ggplot2::ylab("Actual Labels\n") +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge=3))+
            ggplot2::xlab("\nPredicted Labels"))
  }
}

#This function converts a vector of text in an object the model can predict off of
svm_predict_input <- function(x, #Should be a vector of text
                              original_matrix= dtMatrix)
{
  #Converts to a document term matrix
  dtMatrix <- dtMatrix
  predictionData <- x
  predMatrix <- RTextTools::create_matrix(predictionData, originalMatrix=dtMatrix)
  predSize = length(predictionData);
  predictionContainer <- RTextTools::create_container(predMatrix, labels=rep(0,predSize),
                                          testSize=1:predSize, virgin=FALSE)
  #Export the transofmred text input
  assign("predict.input", predictionContainer, envir=globalenv())
}

#This functions converts takes a standard vector of text and generates predictions
#And also other statistical measures.
svm_confidence_table <- function(x, #Should be a vector of text
                                 actual_label,
                                 model,
                                 original_matrix= dtMatrix,
                                 have_labels =T)
{ #First we need to convert the input into a form the model can predict off of

  svm_predict_input <- function(x, #Should be a vector of text
                                original_matrix= dtMatrix)
  {
    #Converts to a document term matrix
    dtMatrix <- dtMatrix
    predictionData <- x
    predMatrix <- RTextTools::create_matrix(predictionData, originalMatrix=dtMatrix)
    predSize = length(predictionData);
    predictionContainer <- RTextTools::create_container(predMatrix, labels=rep(0,predSize),
                                            testSize=1:predSize, virgin=FALSE)
    assign("predict.input", predictionContainer, envir=globalenv())
  }
  #Transform our text input
  svm_predict_input(x)
  #Run the model on this transofrmed input
  results <- RTextTools::classify_model(predict.input, model)

  if (have_labels==T){ #if we have the actual labels we can run some analyses on the model
    results$Actual_Label <- actual_label #attach actual label
    #create a column of a boolean variable to measure accuracy
    results$is.match <- ifelse(results$Actual_Label==results$SVM_LABEL, 1, 0)
    #Export the model predictions as a table
    assign("svm_confidence_table", results, envir=globalenv())
    #Makes a logistic regression that analyses the effect confidence has on
    #Whether a prediction is right (1) or wrong (0)
    popbio::logi.hist.plot(results$SVM_PROB, results$is.match,boxp=FALSE,type="hist",
                   col="navy blue", xlabel = "Confidence", ylabel = "1 = Correct, 0 = Wrong",
                   mainlabel = "Correct Label by Confidence Level- SVM Model")
    #Function that convert a decimal to a percent
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
    }
    #Creating an object that is the model's accuracy on this new data
    accuracy <- percent(mean(results$is.match))
    #Print out the accuracy score
    cat(sprintf("The model accuracy is: %s\n", accuracy))
  }
  else{
    #if we don't have the real labels the function just exports a table
    #with predictions and confidence level
    assign("svm_confidence_table", results, envir=globalenv())
  }
}


conf_interval_calculator<- function(probability = svm_confidence_table$SVM_PROB,
                                    prediction = svm_confidence_table$SVM_LABEL,
                                    confidence_interval= .95)
{ #Read in our data
  mydata <- as.data.frame(probability)
  colnames(mydata) <- c("PROB")
  mydata$Label <- prediction
  mydata$Label <- as.factor(mydata$Label)
  #Create an object with the levels of our labels.
  labels <- levels(mydata$Label)
  #This is used later to create the output sentences
  conf_interval_word <- confidence_interval
  #convert user input into the correct conf interval number for qt test in R
  confidence_interval <- confidence_interval+((1-confidence_interval)/2)
  #Read in a function that will add percents to our sentences later
  percent <- function(x, digits = 1, format = "f", ...) {
    paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
  }
  conf_interval_word <- percent(conf_interval_word)#Add percent to our CI
  i <- 1 #Set count
  while (i <= length(labels)) { #Goes through every label and runs the calculations.
    label_1 <- subset(mydata, mydata$Label == labels[i])
    Expected_Value <- as.character(round(sum(label_1$PROB)))
    error <- qt(confidence_interval,df= length(label_1$PROB)-1)*sd(label_1$PROB)/sqrt(label_1$PROB)
    average <- mean(label_1$PROB)
    left <- average-error
    right <- average+error
    left <- as.character(round(sum(left)))
    right <- as.character(round(sum(right)))
    cat(sprintf(" %s weighted Count: %s\n %s confidence interval between: %s and %s\n\n",
                labels[i], Expected_Value, conf_interval_word, left, right))
    i = i+1
  }
}


vendor_dataframe_generator <- function(labels, #vector of labels
                                       vendor) #vector of vendor names
{
  vendor_df <- as.data.frame(vendor) #convert inputs to dataframe
  colnames(vendor_df) <- c("Vendor_Name")
  vendor_df$Label <- labels
  vendor_df$Label <- as.factor(vendor_df$Label)
  #count how common each label class is, order them in descending
  level_order <- vendor_df %>%
    dplyr::group_by(Label) %>%
    dplyr::summarise(no_rows = length(Label))
  label_levels <- level_order$Label
  #Create a dataframe object data can be added too
  dataframe <- as.data.frame(c('dummy'))
  colnames(dataframe) <- c("location") #change name so we can remove this later
  i <- 1 #establish count
  while (i <= length(label_levels)) #loop will run for every label class
  {
    #Subset for label class, count vendors, attach vendor names in descending order
    label_1 <- subset(vendor_df, vendor_df$Label == label_levels[i])
    vendor_count <- plyr::count(label_1$Vendor_Name)
    vendor_count <- plyr::arrange(vendor_count, -vendor_count$freq)
    rownames(vendor_count) <- NULL
    colnames(vendor_count)[1] <- paste0("new", i)
    dataframe2 <- gdata::cbindX(dataframe, vendor_count[1])
    dataframe <- dataframe2
    i = i+1
  }
  n <- match("location",names(dataframe)) #find location of dummy column
  dataframe <- dataframe[,-n] #remove dummy column
  colnames(dataframe) <- label_levels #change column names
  assign("vendor_count_df", dataframe, envir = globalenv()) #export dataframe
}





ngram <- function(text_input, #vector of text
                  n = 2, #How many words together?
                  plot = TRUE, #return a plot? TRUE by defult
                  occurence_filter=8,#ngram must occur this many times
                  amount_to_graph=15) #number of ngrams to plot
{ #Text pre-processing
  column_one <- text_input %>%
    tolower() %>%
    tm::removePunctuation()%>%
    tm::removeNumbers()
  stop_words <- as.data.frame(tm::stopwords())
  column_one <- tm::removeWords(column_one, stop_words$`tm::stopwords()`)
  column_one <- gsub('\\b\\w{1,2}\\b','',column_one)
  column_one <- as.data.frame(column_one)
  colnames(column_one) <- c("column_one")
  text_df1 <- dplyr::mutate(column_one, text= column_one)
  #Creates object ngram which has the ngrams and stopwords
  ngram <- text_df1 %>%
    dplyr::mutate(line= dplyr::row_number()) %>%
    tidytext::unnest_tokens(word, `text`, token = "ngrams", n = n) %>%
    dplyr::count(word, sort = TRUE)%>%
    dplyr::filter(n> occurence_filter) %>%
    dplyr::mutate(word= reorder(word, n))
  ngram <- na.omit(ngram)
  #Now we have the code to plot the graph
  if(plot == TRUE){
    #Select the 15 most common unigrams
    ngram_plot <- ngram[0:amount_to_graph,]
    ggplot2::ggplot(data=ngram_plot, ggplot2::aes(word, n)) + ggplot2::geom_col(fill= "#5886a5") +
      ggplot2::coord_flip()+ ggplot2::xlab("Ngram\n") + ggplot2::ylab("\nFrequency") +
      ggplot2::ggtitle("Ngram Plot") +  ggplot2::theme_light() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     text = ggplot2::element_text(size=18))
  }else{
    # if the user does not request a plot, returns ngram dataframe
    colnames(ngram) <- c("N_gram", "Frequency")
    assign("n_gram_table", ngram, envir = globalenv())
  }
}


top_terms_by_topic_LDA <- function(text_input, # should be a columm from a dataframe
                                   plot = TRUE, # return a plot? TRUE by defult
                                   number_of_topics = 20) # number of topics (4 by default)
{
  # create a corpus (type of object expected by tm) and document term matrix
  filtered_text <- text_input
  filtered_text <- tm::removePunctuation(filtered_text)
  filtered_text <- tm::removeNumbers(filtered_text)
  filtered_text <- tolower(filtered_text)
  filtered_text <- tm::removeWords(filtered_text, stop_words$word)
  words_to_remove <- c("chlocation", "name", "location", "word", "address")
  filtered_text <- tm::removeWords(filtered_text, words_to_remove)
  Corpus <- tm::Corpus(VectorSource(filtered_text)) # make a corpus object
  DTM <- tm::DocumentTermMatrix(Corpus) # get the count of words/document
  # remove any empty rows in our document term matrix (if there are any
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  # preform LDA & get the words/topic in a tidy text format
  lda <- topicmodels::LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidytext::tidy(lda, matrix = "beta")
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    dplyr::group_by(topic) %>% # treat each topic as a different group
    dplyr::top_n(10, beta) %>% # get the top 10 most informative words
    dplyr::ungroup() %>% # ungroup
    dplyr::arrange(topic, -beta) # arrange words in descending informativeness
  # if the user asks for a plot (TRUE by default)
  if(plot == TRUE){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      dplyr::mutate(term = reorder(term, beta)) %>% # sort terms by beta value
      ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic))) + # plot beta by theme
      ggplot2::geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      ggplot2::labs(x = NULL, y = "Beta") + # no x label, change y label
      ggplot2::coord_flip() # turn bars sideways
  }else{
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}


