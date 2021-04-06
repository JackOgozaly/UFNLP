#' balanced_data
#' @export
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
