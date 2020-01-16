 library(rvest)
 library(tidyverse)
 library(tm) #unsure if used due to tidytext fulfilling same role
 library(tidytext) #for stopwords
# library(gutenbergr) #for finding texts from gutenberg library

read_webpage <- function(urlsite, css="p", start_remove =0, end_remove=0){
    content <- urlsite %>%
        read_html() %>%
        html_nodes(css) %>% # select only the text that uses the specific css from the input
        html_text
    if (start_remove > 0){
        content <- tail(content,-start_remove)
    }
    if (end_remove > 0){
        content <- head(content, -end_remove)
    }
    return(content)
}

format_text <- function(content){
    tibble(line = seq_along(content), text=content) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) # some words are not useful for analysis
}

get_links <- function(urlsite, css="a"){
    links <- read_html(urlsite) %>%
    html_nodes(css) %>%
    html_attr("href")
    return(links)
}

cat_url <- function(urlsite, link_list){
    url_list <- ("")
    for (link in seq_along(link_list)){
        url_list[link] <- paste(urlsite, link_list[link], sep="")
    }
    return(url_list)
}

plot_sentiment <- function(text){
    content_sentiment <- text %>%
        inner_join(get_sentiments("bing")) %>%
        count(line, sentiment) %>%
        spread(sentiment, n, fill=0)%>%
        mutate(sentiment = positive - negative,
            end = cumsum(sentiment),
            start = dplyr::lag(end , default = 0),
            id = seq_along(line)) %>%
        ggplot(aes(x=id,fill=factor(sign(sentiment)))) +
        geom_rect(aes(xmin = id-0.45, xmax= id+0.45, ymin = start, ymax = end))+
        scale_fill_manual(values = c("red3","blue3","green3")) +
        theme(legend.position = "none") +
        labs(x="Paragraph",y="Sentiment", title="Sentiment changes by paragraph")
    return(content_sentiment)
}

