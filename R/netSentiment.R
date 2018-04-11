netSentiment <- function(frame) {

  require(dplyr)
  require(tidytext)
  require(ggplot2)

  frame <- data.frame(frame)
  names(frame) <- c("source", "text")
  frame$text <- as.character(frame$text)
  frame <- frame %>%
    group_by(source) %>%
    mutate(count = n())

  bing <- sentiments %>%
    filter(lexicon == 'bing') %>%
    select(word, sentiment)

  tokenFrame <- frame %>%
    unnest_tokens(word, text)

  tokenFrame <- tokenFrame %>%
    inner_join(bing)

  tokenFrame <- tokenFrame %>%
    anti_join(stop_words)

  textSummary <- tokenFrame %>%
    group_by(source, count) %>%
    count(source, sentiment) %>%
    spread(sentiment, n)

  textSummary[is.na(textSummary)] <- 0

  textSummary <- textSummary %>%
    mutate(netSentiment = positive - negative)

  results <- textSummary %>%
    mutate(plusTive = ifelse(netSentiment > 0, TRUE, FALSE))

  output <- results %>%
    select(source, count, negative, positive, netSentiment)

  print(output)

  print(ggplot(results, aes(x = source, y = netSentiment, fill = plusTive)) +
    geom_col() + labs(x = "Text Source", y = "Net sentiment score",
                      title = "Results of Net Sentiment Analysis", caption = "Using Bing lexicon") +
    theme(legend.position="none"))

  bingCounts <- tokenFrame %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

  bingCounts %>%
    top_n(20) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top Words Contributing to Sentiment Score", x = "Word", y = "Net Sentiment Impact",
         caption = "Using Bing lexicon")

}
