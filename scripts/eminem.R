library(genius)
library(tidytext)
library(ggtext)

# Colors for later
cols=c("positive"="#6BCAB6",
       "negative"="#C71E1D")

# Get all sentiments
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
loughran <- get_sentiments("loughran")
nrc <- get_sentiments("nrc")
# Get all Eminem Albums
em_1 <- genius_album("Eminem","Infinite")
em_1 <- em_1 %>% mutate(album=rep("Infinite",length(track_title)))
em_2 <- genius_album("Eminem","The Slim Shady LP")
em_2 <- em_2 %>% mutate(album=rep("The Slim Shady LP",length(track_title)))
em_3 <- genius_album("Eminem","The Marshall Mathers LP")
em_3 <- em_3 %>% mutate(album=rep("The Marshall Mathers LP",length(track_title)))
em_4 <- genius_album("Eminem","The Eminem Show")
em_4 <- em_4 %>% mutate(album=rep("The Eminem Show",length(track_title)))
em_5 <- genius_album("Eminem","Encore")
em_5 <- em_5 %>% mutate(album=rep("Encore",length(track_title)))
em_6 <- genius_album("Eminem","Relapse")
em_6 <- em_6 %>% mutate(album=rep("Relapse",length(track_title)))
em_7 <- genius_album("Eminem","Recovery")
em_7 <- em_7 %>% mutate(album=rep("Recovery",length(track_title)))
em_8 <- genius_album("Eminem","The Marshall Mathers LP 2")
em_8 <- em_8 %>% mutate(album=rep("The Marshall Mathers LP 2",length(track_title)))
em_9 <- genius_album("Eminem","Revival")
em_9 <- em_9 %>% mutate(album=rep("Revival",length(track_title)))
em_10 <- genius_album("Eminem","Kamikaze")
em_10 <- em_10 %>% mutate(album=rep("Kamikaze",length(track_title)))
em_11 <- genius_album("Eminem","Music To Be Murdered By")
em_11 <- em_11 %>% mutate(album=rep("Music To Be Murdered By",length(track_title)))
# Join into one dataframe
all_em <- rbind(em_1,em_2,em_3,em_4,em_5,em_6,em_7,em_8,
      em_9,em_10,em_11)
# What is the most negative album?
all_em %>% 
  unnest_tokens(word,lyric) %>% 
  inner_join(bing) %>% 
  group_by(album,sentiment) %>% 
  count() %>% 
  mutate(n=ifelse(sentiment=="negative",-n,n)) %>% 
  ggplot(aes(fct_reorder(album,n),n,fill=sentiment)) +
    geom_bar(stat="identity",position = "dodge") +
    coord_flip() +
  labs(x="",y="",
       title="Eminem Discography by Sentiment",
       caption="(Based on Geniusr and bing lexicon)",
       tag="@DaveBrocker")+
    theme_clean() +
    scale_fill_manual(values = cols) +
    theme(axis.ticks = element_blank(),
          legend.position = "none",
          plot.title.position="plot",
          plot.tag = element_text(size=6,color="gold"),
          plot.tag.position = "bottomleft",
          plot.background = element_rect(fill = "#212121"),
          axis.text = element_text(color = "white"),
          title = element_text(color="white",size=10),
          plot.margin = margin(12,12,0,12))
    ylim(-1200,1200)
    
  
  
  
  
  
  
  
  
  
  


