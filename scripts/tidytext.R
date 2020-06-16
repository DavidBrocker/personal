library(tidytext)
library(tidyverse)
library(stringr)
library(rvest)
library(genius)
library(ggthemes)

# URL from the website
site <- "https://www.timeout.com/newyork/music/best-pop-songs-of-all-time"
top_ten <- site %>% 
  # Read URL into R
  read_html() %>% 
  # Get text contained as header 3 elements
  html_nodes("header h3") %>% 
  # Trim the text
  html_text(trim = T)
top_ten <- top_ten %>% 
  # Remove any numbers and periods: ex. 10.
  str_remove("\\d{1,}.") %>% 
  # Replace "by" with comma for easy separating
  str_replace_all("\\bby\\b",",") %>% 
  str_replace_all("“",'"') %>% 
  str_replace_all("’","'") %>% 
  # Make all text Title
  str_to_title()
# While not an NA value, it won't add to our set
top_ten[12] <-NA
# Split by the comma
top_ten <- strsplit(top_ten,",")
# Formatting
top_ten[[7]][2] <- "Nsync"
# Get Lyrics For Each Song
num1 <- genius_lyrics(top_ten[[2]][2],top_ten[[2]][1])
num2 <- genius_lyrics(top_ten[[3]][2],top_ten[[3]][1])
num3 <- genius_lyrics(top_ten[[4]][2],top_ten[[4]][1])
num4 <- genius_lyrics(top_ten[[5]][2],top_ten[[5]][1])
num5 <- genius_lyrics(top_ten[[6]][2],top_ten[[6]][1])
num6 <- genius_lyrics(top_ten[[7]][2],top_ten[[7]][1])
# There is an odd difference between ' and " and ”
num7 <- genius_lyrics("Backstreet Boys","Everybody (Backstreet's Back)")
num8 <- genius_lyrics(top_ten[[9]][2],top_ten[[9]][1])
num9 <- genius_lyrics(top_ten[[10]][2],top_ten[[10]][1])
num10 <- genius_lyrics(top_ten[[11]][2],top_ten[[11]][1])
# Join into one dataframe
top_ten_df <- rbind(num1,num2,num3,
      num4,num5,num6,
      num7,num8,num9,num10)
# Number of Words in Song
num_words <- top_ten_df %>% 
  unnest_tokens(word,lyric) %>% 
  group_by(track_title) %>% 
  count(sort=T)
# Plot
num_words %>% 
  ggplot(aes(fct_reorder(track_title,n),n,fill=track_title)) +
    geom_bar(stat="identity",show.legend = F) +
    coord_flip()+
    labs(x="",y="",
         title="Top 10 Pop Songs: Word Count",
         caption="(based on timeout.com)")+
    theme_clean(base_size=10) +
    scale_fill_brewer(palette="Paired")
# Number of Unique Words
un_num_words <- top_ten_df %>% 
  unnest_tokens(word,lyric) %>% 
  group_by(track_title) %>% 
  distinct(word,.keep_all = T) %>% 
  count(sort=T)  
# Plot
un_num_words %>% 
  ggplot(aes(fct_reorder(track_title,n),n,fill=track_title)) +
  geom_bar(stat="identity",show.legend = F) +
  coord_flip()+
  labs(x="",y="",
       title="Top 10 Pop Songs: Unique Word Count",
       caption="(based on timeout.com)")+
  theme_clean(base_size=10) +
  scale_fill_brewer(palette="Paired")
# Get Sentiment
bing <- get_sentiments("bing")
# Sentiment df 
sent <- top_ten_df %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>%
  # Add Sentiment to the dataframe
  inner_join(bing) %>% 
  group_by(track_title,sentiment) %>% 
  count() %>% 
  # Make the n negative if the sentiment is negative
  mutate(n = ifelse(sentiment == "negative", -n, n)) 
# Colors for the Plot
cols=c("positive"="#6BCAB6",
       "negative"="#C71E1D")
# Sentiment Plot
sent %>% 
  ggplot(aes(fct_reorder(track_title,-n),n,fill=sentiment)) +
    geom_bar(stat="identity") +
    coord_flip()+
    labs(x="",y="Contribution to Sentiment",
         title="Top 10 Pop Songs Sentiments",
         caption="(based on timeout.com)")+
  theme_clean(base_size=10) +
  theme(axis.ticks = element_blank(),
        legend.title = element_blank())+
  scale_fill_manual(values=cols)

