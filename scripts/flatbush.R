library(ggplot2)
library(ggtext)
library(ggthemes)
library(tidytext)
library(genius)
library(tidyverse)

# Colors for later
cols=c("positive"="#6BCAB6",
       "negative"="#C71E1D")

# Get all sentiments
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
loughran <- get_sentiments("loughran")
nrc <- get_sentiments("nrc")

# Get album
fbz_1 <- genius_album("Flatbush Zombies","D.R.U.G.S")
# Add album column
fbz_1 <- fbz_1 %>% mutate(album=rep("D.R.U.G.S",length(track_title)))
# Get album
fbz_2 <- genius_album("Flatbush Zombies","BetterOffDEAD")
# Add album column
fbz_2 <- fbz_2 %>% mutate(album=rep("BetterOffDEAD",length(track_title)))
# Get album
fbz_3 <- genius_album("Flatbush Zombies","Day of the Dead EP")
# Add album column
fbz_3 <- fbz_3 %>% mutate(album=rep("Day of the Dead EP",length(track_title)))
# Get album
fbz_4 <- genius_album("Flatbush Zombies","3001: A Laced Odyssey")
# Add album column
fbz_4 <- fbz_4 %>% mutate(album=rep("3001: A Laced Odyssey",length(track_title)))
# Get album
fbz_5 <- genius_album("Flatbush Zombies","Vacation In Hell")
# Add album column
fbz_5 <- fbz_5 %>% mutate(album=rep("Vacation In Hell",length(track_title)))
# Get album
fbz_6 <- genius_album("Flatbush Zombies","now, more than ever")
# Add album column
fbz_6 <- fbz_6 %>% mutate(album=rep("now, more than ever",length(track_title)))
# Combine
fbz <- rbind(fbz_1,fbz_2,fbz_3,fbz_4,fbz_5,fbz_6)
# Top 10 Used Words
top_10 <- fbz %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  count(sort=T) 
# Sentiment (Negative/Positive)
fbz_sent <- fbz %>% 
  unnest_tokens(word,lyric) %>% 
  inner_join(bing) %>% 
   group_by(album,sentiment) %>% 
   count(sort=T) %>% 
  mutate(n=ifelse(sentiment=="negative",-n,n)) 
# Sentiment Plot
fbz_sent %>% 
  ggplot(aes(fct_reorder(album,n),n,fill=sentiment)) +
  geom_bar(stat="identity",position = "dodge") +
  coord_flip() +
  labs(x="",y="",
       title="Flatbush Zombies Discography by Sentiment",
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
        plot.margin = margin(12,12,0,12)) +
ylim(-1000,1000) +
  scale_x_discrete(name=NULL,
                   labels=labels)



labels <- c(
  now = "<img src='https://ssla.ulximg.com/image/750x750/cover/1591364677_44a4dc39edc13cb8fedd6ebcd7896f46.jpg/6c2be4305a5a20797172164f5e2dc4b1/1591364677_a4c91e838ffb4087ae3b0d479f09276c.jpg'
    width='60' /><br>*now, more than ever*",
  laced = "<img src='https://upload.wikimedia.org/wikipedia/en/thumb/0/04/3001ALaced_Odyssey.jpeg/220px-3001ALaced_Odyssey.jpeg'
    width='60' /><br>*3001: A Laced Odyssey*",
  day = "<img src='https://hw-img.datpiff.com/m5168bd2/Flatbush_Zombies_Day_Of_The_Dead-front-large.jpg'
    width='60' /><br>*Day of the Dead EP*",
  vacation = "<img src='https://upload.wikimedia.org/wikipedia/en/1/19/Vacation_in_Hell.jpg'
    width='60' /><br>*Vacation in Hell*",
  drugs="<img src='https://images.genius.com/467f11b4c1522a5c7721d62cd169f5f7.608x608x1.jpg'
    width='60' /<br>*D.R.U.G.S*",
  better = "<img src='https://cdn10.bigcommerce.com/s-99klc1qm/products/70218/images/112594/c0019732__34714.1576218331.500.750.jpg'
    width='60' /><br>*BetterOffDEAD*")

fbz_sentv2 <- fbz_sent

fbz_sentv2$album <- factor(fbz_sentv2$album)

fbz_sentv2$album <- 
  factor(fbz_sentv2$album,labels=c("now","laced","day","vacation","drugs","better"))
  
  
fbz_sentv2 %>% 
  ggplot(aes(album,n,fill=sentiment)) +
    geom_bar(stat="identity",position = "dodge")+
    labs(x="",y="Count",
         title=paste0("Flatbush Zombies Disocograhy by Sentiment Count","\n"),
         tag="@DaveBrocker",
         caption="(based on geniusr and bing") +
    scale_x_discrete(
      name=NULL,
      labels=labels)+
    theme_clean()+
    theme(
      axis.text.x = element_markdown(color = "white", size = 11),
      legend.position = "left",
      legend.background = element_rect(fill="#212121"),
      legend.title = element_blank(),
      plot.title.position="plot",
      plot.tag = element_text(size=6,color="gold"),
      plot.tag.position = "bottomleft",
      plot.background = element_rect(fill = "#212121"),
      axis.text = element_text(color = "white"),
      title = element_text(color="white",size=15),
      plot.margin = margin(12,12,0,12))+
  scale_fill_manual(values=cols)
  
nrc <- get_sentiments("nrc")
  
new <- rbind(fbz_5,fbz_6)









fbz %>% 
  unnest_tokens(word,lyric) %>% 
  inner_join(nrc) %>% 
  group_by(album,sentiment) %>% 
  filter(sentiment %in% c(
    "anger","disgust","fear",
    "sadness","joy","surprise",
    "trust","anticipation")) %>% 
  count() %>% 
  ggplot(aes(album,n,fill=sentiment)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_clean()+
  scale_fill_economist()+
  theme(
    axis.text.x = element_markdown(color = "white", size = 11),
    legend.position = "left",
    legend.background = element_rect(fill="#212121"),
    legend.title = element_blank(),
    legend.text = element_text(color="white"),
    plot.title.position="plot",
    plot.tag = element_text(size=6,color="gold"),
    plot.tag.position = "bottomleft",
    plot.background = element_rect(fill = "#212121"),
    axis.text = element_text(color = "white"),
    title = element_text(color="white",size=15),
    plot.margin = margin(12,12,0,12))

