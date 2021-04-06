#' top_terms_by_topic_LDA
#' @export
top_terms_by_topic_LDA <- function(text_input, # should be a columm from a dataframe
                                   plot = TRUE, # return a plot? TRUE by defult
                                   number_of_topics = 20) # number of topics (4 by default)
{
  # create a corpus (type of object expected by tm) and document term matrix
  filtered_text <- text_input
  filtered_text <- tm::removePunctuation(filtered_text)
  filtered_text <- tm::removeNumbers(filtered_text)
  filtered_text <- tolower(filtered_text)
  stop_words <- as.data.frame(tm::stopwords())
  filtered_text <- tm::removeWords(filtered_text, stop_words$`tm::stopwords()`)
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
