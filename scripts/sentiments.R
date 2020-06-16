library(rvest)
library(ggtext)
dict <-  "https://www.merriam-webster.com/word-of-the-day"
word <- dict %>% 
  read_html() %>% 
  html_nodes("h1") %>% 
  html_text()
definitions <- dict %>% 
  read_html() %>% 
  html_nodes("p~ p+ p , .wod-definition-container h2+ p") %>% 
  html_text()
paste0("The Word of the day is: ",word, "\n", definitions)
#######################################################
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
loughran <- get_sentiments("loughran")
nrc <- get_sentiments("nrc")

# Bing Size
bing_size <- bing %>% 
  group_by(sentiment) %>% 
  count() 
# Afinn Size
afinn_size <- afinn %>% 
  group_by(value) %>% 
  count()
afinn %>% 
  filter(value==-5)
# Loughran Size
loughran_size <- loughran %>% 
  group_by(sentiment) %>% 
  count()
# NRC Size
nrc_size <- nrc %>% 
  group_by(sentiment) %>% 
  count()
# Plot
all_lex %>% 
  ggplot(aes(fct_reorder(lexicon,n),n,fill="gold")) +
    geom_bar(stat="identity") +
    theme_void() +
  geom_text(aes(label=lexicon,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=2)) +
  labs(title="Relative Size of Sentiments",
       subtitle="afinn | loughran | bing | nrc") +
  theme(text = element_text(color = "#ffffff"),
        legend.position = "none",
        plot.background = element_rect(fill = "#212121", 
                                       colour = NA),
        title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(colour = "gold", 
                                     face = "plain", size = 12),
        plot.margin = margin(12,12,0,12))+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") +
  ylim(0,15000)

a <- afinn %>% 
  count() %>% 
  add_column(lexicon="afinn")
b <- bing %>% 
  count() %>% 
  add_column(lexicon="bing")
l <- loughran %>% 
  count() %>% 
  add_column(lexicon="loughran")
n <- nrc %>% 
  count() %>% 
  add_column(lexicon="nrc")

all_lex <- rbind(a,b,l,n)


loughran_size%>% 
  ggplot(aes(fct_reorder(sentiment,n),n,fill="gold")) +
  geom_bar(stat="identity") +
  theme_void() +
  geom_text(aes(label=sentiment,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=1.7)) +
  labs(title="Size of Sentiments | loughran",
       subtitle=paste0("\n\n",
                       "amorphous | superfluous","\n",
                       "bound | constraining","\n",
                       "risks | uncertainty","\n",
                       "excelling | positive","\n",
                       "lawful | litigious","\n",
                       "deceptively | negative","\n")) +
  theme(text = element_text(color = "#ffffff"),
        legend.position = "none",
        plot.background = element_rect(fill = "#212121", 
                                       colour = NA),
        title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(colour = "gold", 
                                     face = "plain", size = 12),
        plot.margin = margin(12,12,0,12))+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") +
  ylim(0,3000)

nrc_size%>% 
  ggplot(aes(fct_reorder(sentiment,n),n,fill="gold")) +
  geom_bar(stat="identity",width = .75) +
  theme_void() +
  geom_text(aes(label=sentiment,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=2)) +
  labs(title="Size of Sentiments | nrc",
       subtitle=paste0("\n\n",
                       "bonus | surprise","\n",
                       "giggle | joy","\n",
                       "caution | anticipation","\n",
                       "collusion | disgust","\n",
                       "debt | sadness","\n",
                       "ordained | trust","\n",
                       "opress | anger","\n",
                       "incurable | fear","\n",
                       "flirt | positive","\n",
                       "scum | negative")) +
  theme(text = element_text(color = "#ffffff"),
        legend.position = "none",
        plot.background = element_rect(fill = "#212121", 
                                       colour = NA),
        title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(colour = "gold", 
                                     face = "plain", size = 12),
        plot.margin = margin(12,12,0,12))+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") +
  ylim(0,4000)

bing_size %>% 
  ggplot(aes(fct_reorder(sentiment,n),n,fill="gold")) +
  geom_bar(stat="identity") +
  theme_void() +
  geom_text(aes(label=sentiment,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=2)) +
  labs(title="Size of Sentiments | bing",
       subtitle=paste0("\n\n",
                       "guiltless | positive","\n",
                       "unavoidably | negative")) +
  theme(text = element_text(color = "#ffffff"),
        legend.position = "none",
        plot.background = element_rect(fill = "#212121", 
                                       colour = NA),
        title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(colour = "gold", 
                                     face = "plain", size = 12),
        plot.margin = margin(12,12,0,12))+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") +
  ylim(0,8000)

afinn_size%>% 
  ggplot(aes(value,n,n,fill="gold")) +
  geom_bar(stat="identity") +
  theme_void() +
  geom_text(aes(label=value,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=2.2)) +
  labs(title="Size of Sentiments | afinn",
       subtitle=paste0("\n\n",
                       "bastard | -5","\n",
                       "damned | -4","\n",
                       "fake |-3","\n",
                       "deniers | -2","\n",
                       "irrational | -1","\n",
                       "some kind | 0","\n",
                       "clearly | 1","\n",
                       "resolve | 2","\n",
                       "lovelies| 3","\n",
                       "lifesaver | 4","\n",
                       "breathtaking | 5","\n")) +
  theme(text = element_text(color = "#ffffff"),
        legend.position = "none",
        plot.background = element_rect(fill = "#212121", 
                                       colour = NA),
        title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(colour = "gold", 
                                     face = "plain", size = 12),
        plot.margin = margin(12,12,0,12))+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") +
  ylim(-500,1000)







########GGTEXT#############






# Better Bing
bing_size %>% 
  ggplot(aes(fct_reorder(sentiment,n),n,fill="gold")) +
  geom_bar(stat="identity") +
  theme_void() +
  geom_text(aes(label=sentiment,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=2)) +
  labs(title="<span style = 'color:white'><b>Sentiment Size | <span style = 'color:gold'<b>bing</b><br><br>
    <span style = 'font-size:12pt'> <span style = 'color:gold;'>guiltless | <span style = 'color:white;'>positive <br> 
    <span style = 'font-size:12pt'> <span style = 'color:gold;'>unavoidably | <span style = 'color:white;'>negative <br></span> <br>") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#212121"), 
        plot.title = element_textbox_simple(
          size = 24,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0)))+
  ylim(0,8000)+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") 

# Better loughran

loughran_size %>% 
  ggplot(aes(fct_reorder(sentiment,n),n,fill="gold")) +
  geom_bar(stat="identity") +
  theme_void() +
  geom_text(aes(label=sentiment,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=1.3)) +
  labs(title="<span style = 'color:white'><b>Sentiment Size | <span style = 'color:gold'<b>loughran</b><br><br>
<span style = 'font-size:12pt'> <span style = 'color:gold;'>amorphous | <span style = 'color:white;'>superfluous <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>bound | <span style = 'color:white;'>constraining <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>risks | <span style = 'color:white;'>uncertainty <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>excelling | <span style = 'color:white;'>positive <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>lawful | <span style = 'color:white;'>litigious   
<span style = 'font-size:12pt'> <span style = 'color:gold;'>deceptively | <span style = 'color:white;'> negative") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#212121"), 
        plot.title = element_textbox_simple(
          size = 24,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0)))+
  ylim(0,2500)+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") 

# Better NRC
nrc_size%>% 
  ggplot(aes(fct_reorder(sentiment,n),n,fill="gold")) +
  geom_bar(stat="identity",width = .75) +
  theme_void() +
  geom_text(aes(label=sentiment,
                vjust=-.6,colour="gold")) +
  geom_text(aes(label=n,vjust=2)) +
  labs(title="<span style = 'color:white'><b>Sentiment Size | <span style = 'color:gold'<b>nrc</b><br><br>
<span style = 'font-size:12pt'> <span style = 'color:gold;'>bonus | <span style = 'color:white;'>surprise <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>giggle | <span style = 'color:white;'>joy <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>caution | <span style = 'color:white;'>anticipation <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>collusion | <span style = 'color:white;'>disgust <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>debt | <span style = 'color:white;'>sadness   
<span style = 'font-size:12pt'> <span style = 'color:gold;'>ordianed | <span style = 'color:white;'> trust<br>
<span style = 'font-size:12pt'> <span style = 'color:gold;'>opress | <span style = 'color:white;'>anger <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>incurable | <span style = 'color:white;'>fear <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>flirt | <span style = 'color:white;'>positive <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>scum | <span style = 'color:white;'>negative") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#212121"), 
        plot.title = element_textbox_simple(
          size = 24,
          lineheight = .8,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0)))+
  ylim(0,3500)+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") 
  
  
# Better afinn

afinn_size%>% 
  ggplot(aes(value,n,n,fill="gold")) +
  geom_bar(stat="identity") +
  theme_void() +
  geom_text(aes(label=value,
                vjust=-.08,colour="gold")) +
  geom_text(aes(label=n,vjust=2.2)) +
  labs(title="<span style = 'color:white'><b>Sentiment Size | <span style = 'color:gold'<b>afinn</b></b><br>
<span style = 'font-size:12pt'> <span style = 'color:gold;'>bastard | <span style = 'color:white;'>-5 <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>damned | <span style = 'color:white;'>-4 <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>fake | <span style = 'color:white;'>-3 <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>deniers | <span style = 'color:white;'>-2 <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>irrational | <span style = 'color:white;'>-1 <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>some kind| <span style = 'color:white;'>0  
<span style = 'font-size:12pt'> <span style = 'color:gold;'>clearly | <span style = 'color:white;'> 1<br>
<span style = 'font-size:12pt'> <span style = 'color:gold;'>resolve | <span style = 'color:white;'>2 <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>lovelies | <span style = 'color:white;'>3<br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>lifesaver | <span style = 'color:white;'>4 <br> 
<span style = 'font-size:12pt'> <span style = 'color:gold;'>breathtaking | <span style = 'color:white;'>5") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#212121"), 
        plot.title = element_textbox_simple(
          size = 24,
          lineheight = .8,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0)))+
  ylim(-500,1000)+
  scale_colour_manual(values="#ffffff") +
  scale_fill_manual(values="gold") 
  
  





    