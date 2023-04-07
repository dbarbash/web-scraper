library(tidyverse)  
library(data.table) 
library(lsa)        
library(Rtsne)      
library(plotly)     
library(tidytext)   
library(wordcloud)  
library(lubridate)  
library(xml2)
library(rvest)      
library(glue)       
library(tm)
library(readxl)
library(ggplot2)
library(wordcloud2)
library(plotrix)
library(textstem)
library(stopwords)
library(hunspell)
library(gutenbergr)
library(dplyr)


max_page_no <- 1
links_all <- vector()

for(i in 1:max_page_no) {
  url <- glue("https://www.investing.com/news/cryptocurrency-news/{i}")
  page <- read_html(url)
  links <- page %>%
    html_node("section#leftColumn") %>%
    html_nodes("div.textDiv") %>%
    html_node("a") %>%
    html_attr("href") %>%
    paste0("https://www.investing.com", .)
  
  links_all <- c(links_all, links)
  
  print(i)
}



wypowiedzi <- tibble()

for(i in 1:length(links_all)) {
  
  page <- read_html(links_all[[i]])
  
  speech <- page %>%
    html_nodes("#leftColumn") %>% html_nodes("div.WYSIWYG.articlePage") %>%
    html_text() %>%
    str_replace_all("\\s+", " ")
  
  lead <- page %>%
    html_nodes("#leftColumn") %>% html_nodes("h1") %>%
    html_text()
  
  data <- page %>%
    html_nodes("#leftColumn") %>% html_nodes("div.contentSectionDetails") %>% html_nodes("span") %>%
    html_text() %>% str_replace(",", "") %>%
    str_sub(start = 17, end = 25) %>% as.Date("")
  
  wypowiedzi <- bind_rows(wypowiedzi,
                          tibble(text = speech,
                                 title = lead,
                                 date = data,           
                                 url = links_all[[i]])) 
  print(i)
}

wypowiedzi2 <- wypowiedzi

wypowiedzi2 <- wypowiedzi2 %>%
  arrange(date) %>%
  mutate(id = row_number(),
         text = trimws(text)) %>%
  drop_na()

dtm <- wypowiedzi2 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

dtm_z_datoi <- dtm %>% filter(!str_detect(word, '\\d')) %>%
  mutate(day = str_sub(date, start = 6, end = 10))

word_count <- dtm %>% filter(!str_detect(word, '\\d')) %>%
  count(word)

par(mar = c(0, 0, 0, 0))

wordcloud(word_count$word, 
          word_count$n, max.words = 120,min.freq=3,scale=c(4,.25), 
          random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(6, "Dark2"))

word_count2 <- dtm_z_datoi %>% 
  count(word, day)

par(mar = c(0, 0, 0, 0), mfrow = c(3, 5))
for (i in sort(unique(word_count2$day))){
  word_day <- filter(word_count2, day == i)
  wordcloud(word_day$word, word_day$n, max.words = 80, 
            rot.per = 0.35, colors = brewer.pal(6, "Dark2"))
  text(x = 0.5, y = 1, cex = 2, paste('Day:', i))
}

slowa_kluczowe <- dtm_z_datoi %>% 
  count(date, word, sort = T) %>% 
  complete(date, word, fill = list(n = 0)) %>%  
  group_by(date) %>% 
  mutate(minutes_total = sum(n)) %>% 
  filter(word %in% c("nft", "virtual", "usd", "finance", "bitcoin", "network")) 

ggplot(data = slowa_kluczowe, aes(date, n / minutes_total)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~ word) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  ylab("% frequency of word") 


word_top <- dtm_z_datoi %>% 
  count(day, word, sort = T) %>% 
  group_by(day) %>% 
  top_n(9, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))


ggplot(word_top, aes(x = reorder_within(word, n, day), y = n, fill = day)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~day, scales ="free_y" ) +
  scale_x_reordered()


nrc <- get_sentiments("nrc"); nrc
nrc_polarity <- nrc %>% 
  filter(sentiment %in% c('positive', 'negative'))

word_sent <- dtm %>% 
  inner_join(nrc_polarity, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T)

word_sent_top <- word_sent %>% 
  group_by(sentiment) %>% 
  top_n(20, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

ggplot(word_sent_top, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_grid(~sentiment, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
