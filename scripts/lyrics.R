library(tidyverse)
library(extrafont)
library(waffle)
library(ggplot2)

extrafont::font_import()

title_vec <- doctorwho %>% 
  unnest_tokens(word,Title) %>% 
  group_by(word) %>% 
  anti_join(stop_words) %>% 
  count(sort=T) %>% 
  ungroup() %>% 
  slice(1:10)

  
title_vec %>% 
  ggplot(aes(fct_reorder(word,n),n)) +
    geom_segment(aes(word,xend=word,
                     y=0,yend=n),color="skyblue")+
  geom_point(size=4)+
    coord_flip() +
    theme_clean() +
    labs(x="",y="",
         title="Most Used Words in Doctor Who Titles")
####################################################

# Total number of words on album
total_words <- aesop_ik %>% 
  unnest_tokens(word,lyric) %>% 
  count()
# Total number of unique words
total_unique <- aesop_ik %>% 
  unnest_tokens(word,lyric) %>% 
  distinct(word) %>% 
  count()
# Overall Sentiment
 aesop_ik %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(bing) %>% 
  group_by(sentiment) %>% 
  count()
# Sentiment Waffle
waffle(c('Negative=75%' = 70, 'Negative=25%' = 30), rows = 10, 
       colors = c("#FD6F6F", "#93FB98"),
       title = 'Sentiment of Aesop Rock - The Impossible Kid', 
       legend_pos="bottom")
# Sentiment by Track
# Colors for each sentiment
cols=c("positive"="#6BCAB6",
        "negative"="#C71E1D")
# Tidy Sentiment
sent_aes <- aesop_ik %>% 
   unnest_tokens(word,lyric) %>% 
   anti_join(stop_words) %>% 
   inner_join(bing) %>% 
   group_by(track_title,sentiment) %>% 
   count() %>% 
   mutate(n= ifelse(sentiment=="negative",-n,n)) 
# Tidy sentiment across album
by_line <- aesop_ik %>% 
  unnest_tokens(word,lyric) %>% 
  inner_join(bing) %>% 
  group_by(line,sentiment) %>% 
  count() %>% 
  mutate(n= ifelse(sentiment=="negative",-n,n)) %>% 
  count()
# Tidy plot across album
ggplot(aes(line,n,fill=sentiment))+
    geom_col()
# Sentiment Plot
sent_aes %>% 
  ggplot(aes(track_title,n,fill=sentiment)) +
    geom_bar(stat="identity") +
    coord_flip() +
    theme_clean() +
    scale_fill_manual(values=cols)+
    ylim(-45,45) +
    theme(legend.title = element_blank(),
          axis.ticks = element_blank())+
    labs(x="",y="Contribution to Sentiment",
       title="The Impossible Kid Sentiments",
       caption="(based on Genius Lyrics)")
