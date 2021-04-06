#' ngram
#' @importFrom magrittr %>%
#' @export
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

