#' svm_model
#' @export
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
