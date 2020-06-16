library(gganimate)
library(ggplot2)
library(ggthemes)
library(tidyverse)

# Read csv file
doctorwho <- read_csv("https://raw.githubusercontent.com/DavidBrocker/personal/master/files/DoctorWho_Ratings.csv")
# Imports a variable X1 so I remove it
dw <- doctorwho %>% 
  select(Season, EpisodeNumber,
         Doctor,Title,
         Ratings,Quality)
# Add a column for overall episode length. This is what enables
# gganimate to 'draw' over time
dw <- dw %>% 
  mutate(OverallEp=1:length(EpisodeNumber))
# Make sure that the Doctor factors are in order. In hindsight, I should have
# made them their numerical counterparts, (9,10,11,12,13)
dw$Doctor <- factor(dw$Doctor,levels = c("Ninth","Tenth","Eleventh","Twelfth","Thirteenth"),ordered = T)

dw %>% 
  # The group aesthetic is what gganimate will look for
  ggplot(aes(OverallEp,Ratings,group=Doctor,color=Doctor)) +
    geom_point() +
    geom_line() +
  # This is the line that creates the animation. Without it you would have
  # a regular line graph with points
    transition_reveal(OverallEp) +
    labs(x="Episode Number",y="User Rating",
         title=paste0("Doctor Who Episode Ratings by Doctor","\n"),
         caption="(*based on IMDb User Ratings*)",
         tag="@DaveBrocker")+
  # I just like this theme a lot!
    theme_clean() +
  # I am still figuring out what I like in my plots 
  # No axis ticks
    theme(axis.ticks = element_blank(),
          # Colorful background
          plot.background = element_rect(fill="#003D6B"),
          # Matching legend background with no border
          legend.background = element_rect(fill="#003D6B",linetype = "blank"),
          # Match background
          legend.text = element_text(color="white",size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          # Adjust plot elemets
          plot.title = element_text(color="white",hjust=1.8),
          axis.text = element_text(color='white'),
          axis.title.x = element_text(color="white",vjust = -3),
          axis.title.y = element_text(color="white",vjust=3),
          plot.title.position = "panel",
          plot.tag.position = "bottomleft",
          plot.tag = element_text(size=8,color="gold"),
          plot.caption.position = "plot",
          # Use markdown to stylize the caption
          plot.caption = element_markdown(
            color="white",size=8),
          )+
  # I could not find a suitable palette, maybe you can!
  scale_color_ptol() 
  # This saves the animation
anim_save("dw_ratings_10.gif")

############# HEATMAP ############

# This code draws a heatmap where each episode is a tile with 
# the IMDb user rating on top. 

cols=c("Bad"="red2",
       "Garbage" ="dodgerblue2",
       "Great" ="greenyellow",
       "Regular"="darkorange1", 
       "Good" ="gold1")

dw$EpisodeNumber <- factor(dw$EpisodeNumber)
 
ggplot(dw,aes(factor(Season),EpisodeNumber,fill=Quality)) +
  geom_tile(colour="black")+
  geom_text(aes(label=Ratings))+
  scale_fill_manual(values=cols)+
  labs(x="Season",y="Episode",
       title=paste0("Doctor Who (2005-2020) IMDb Ratings","\n"),
       caption="(*based on IMDb User Ratings*)",
       tag="@DaveBrocker")+
  theme_clean() +
  theme(axis.ticks = element_blank(),
        plot.background = element_rect(fill="#003D6B"),
        legend.background = element_rect(fill="#003D6B",linetype = "blank"),
        legend.text = element_text(color="white",size=8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(color="white",hjust=1.8),
        axis.text = element_text(color='white'),
        axis.title.x = element_text(color="white",vjust = -3),
        axis.title.y = element_text(color="white",vjust=3),
        #plot.title.position = "plot",
        plot.tag.position = "bottomleft",
        plot.tag = element_text(size=8,color="gold"),
        plot.caption.position = "plot",
        plot.caption = element_markdown(
          color="white",size=8))
ggsave("Better_Heat.jpeg")


















