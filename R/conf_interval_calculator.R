#' conf_interval_calculator
#' @export
conf_interval_calculator<- function(probability,
                                    prediction,
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
