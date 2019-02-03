library(tidytext)
library(wordcloud)
library(pluralize)
library(dplyr)

load("~/Apps/glassdoor/data/db/glassdoor_en.Rda")

custom_stop_words <- bind_rows(data_frame(word = c("cesar"),lexicon = c("custom")),
                               stop_words)

Pros <- glassdoor_en  %>% 
  transmute (Company = Company, id = id, text = Pros)

Pros_tokens <- Pros %>% filter(!is.na(text)) %>% 
  unnest_tokens(word,text) %>% mutate (word = singularize(word)) %>%
  anti_join(custom_stop_words)

Cons <- glassdoor_en %>% 
  transmute (Company = Company, id = id,text = Cons)

Cons_tokens <- Cons %>% filter(!is.na(text)) %>% 
  unnest_tokens(word,text) %>% mutate (word = singularize(word)) %>%
  anti_join(custom_stop_words)

Advs <- glassdoor_en %>% 
  transmute (Company = Company, id = id,text = AdviceMgmt)

Advs_tokens <- Advs %>% filter(!is.na(text)) %>% 
  unnest_tokens(word,text) %>% mutate (word = singularize(word)) %>%
  anti_join(custom_stop_words)

Pros_tokens %>% count(word) %>% with(wordcloud(word,n,scale = c(3,0.2)))
Cons_tokens %>% count(word) %>% with(wordcloud(word,n,scale = c(3,0.2)))
Advs_tokens %>% count(word) %>% with(wordcloud(word,n,scale = c(3,0.2)))

All_tokens <- bind_rows(Pros_tokens,Cons_tokens,Advs_tokens)
All_tokens %>% count(word) %>% 
  with(wordcloud(word,n,scale = c(4,0.2),vfont=c("serif","plain"),
  colors=c("hotpink4","firebrick4","darkslateblue","deepskyblue4")))
