library(ggthemes)
# Using an image as a reference, mark each space as a number
pacman = c(
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,
  1,1,1,0,2,2,0,1,1,1,0,2,2,0,1,1,
  1,1,0,2,2,2,0,1,1,1,0,2,2,2,0,1,
  1,1,0,2,2,2,2,0,1,0,2,2,2,2,0,1,
  1,0,2,2,2,2,2,0,1,0,2,2,2,2,2,0,
  1,0,2,2,2,2,2,0,1,0,2,2,2,2,2,0,
  1,0,2,2,2,2,2,2,0,2,2,2,2,2,2,0,
  1,0,2,2,2,2,2,2,0,2,2,2,2,2,2,0,
  1,0,2,2,2,2,2,2,0,2,2,2,2,2,2,0,
  1,1,0,2,2,2,2,2,2,2,2,2,2,2,0,1,
  1,1,0,2,2,2,2,2,2,2,2,2,2,2,0,1,
  1,1,1,0,2,2,2,2,2,2,2,2,2,0,1,1,
  1,1,1,1,0,0,2,2,2,2,2,0,0,1,1,1,
  1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# Most pixel art like this is 16x16
pcman <- data.frame(
# Formatted as such: 1,1,1,1...2,2,2,2,....
  r=rep(1:16,each=16),
# Formatted as such: 1,2,3,4...14,15,16
  cl=1:16,
  pacman)
pcman <- pcman %>% 
# Deserving of its own form: essentially `ifelse` x n
  mutate(pacman_color=case_when(
    pacman ==0 ~"black",
    pacman ==1 ~"white",
    pacman ==2 ~"yellow"
  ))
# Use these values with `scale_fill_manual`
pacman_colors=c("black"="black","white"="#212121","yellow"="yellow")
# `r` and `cl` will make the grid, `pacman_color` will fill it in
ggplot(pcman,aes(r,cl,fill=pacman_color)) +
# Create heatmap
    geom_tile() +
# Make the tiles the same size
    coord_equal()+
# Flip the plot
    scale_x_reverse()+
# Remove everything 'extra'
    theme_void() +
# Remove legend
    theme(legend.position = "none",
# Color the background
          plot.background = element_rect(fill="#212121")) +
# Use our colors from before
    scale_fill_manual(values=pacman_colors) 