emotion <- function(frame) {

  require(dplyr)
  require(tidytext)
  require(ggplot2)

  frame <- data.frame(frame)
  names(frame) <- c("source", "text")
  frame$text <- as.character(frame$text)
  frame <- frame %>%
    group_by(source) %>%
    mutate(count = n())

  nrc <- sentiments %>%
    filter(lexicon == 'nrc') %>%
    select(word, sentiment)

  tokenFrame <- frame %>%
    unnest_tokens(word, text)

  tokenFrame <- tokenFrame %>%
    inner_join(nrc)

  tokenFrame <- tokenFrame %>%
    anti_join(stop_words)

  textSummary <- tokenFrame %>%
    group_by(source, sentiment, count) %>%
    count(source, sentiment)

  textSummary[is.na(textSummary)] <- 0

  textSummary <- textSummary %>%
    filter(sentiment != "positive", sentiment != "negative")

  print(textSummary)

  print(
    ggplot(textSummary, aes (x = sentiment, y = n, fill = source)) + geom_col() +
      coord_flip() + facet_wrap(~source) + labs(title = "Emotional Sentiment Analysis of Text",
                                                x = "Count of words",
                                                y = "Assigned Emotion",
                                                caption = "Using NRC lexicon")
        )

}
