#' text_clean
#' @export
text_clean <- function(text_input,
                       dataframe
){
  tidy_text <- text_input
  tidy_text <- tm::removePunctuation(tidy_text)
  tidy_text <- tolower(tidy_text)
  tidy_text <- tm::removeNumbers(tidy_text)
  stop_words <- as.data.frame(tm::stopwords())
  tidy_text <- tm::removeWords(tidy_text, stop_words$`tm::stopwords()`)
  tidy_text_df <- as.data.frame(dataframe)
  tidy_text_df$tidy_text <- tidy_text
  assign("tidytext_df", tidy_text_df, envir = globalenv())
}
