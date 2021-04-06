#' svm_predict_input
#' @export
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
