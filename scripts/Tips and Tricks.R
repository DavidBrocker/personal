# Cool R-Stuff

#### Code Snippets

# Typing if and tab twice results in:

if (condition) {
  
}

# Typing fun and tab results in:

name <- function(variables) {
  
}

# Typing for and tab results in:

for (variable in vector) {
  
}

if (condition) {
  
}

# Typing lib and tab results in:

library("package")

# Multiline Editing: ctrl + alt + up/down
library("ggplot")
library("tidyverse")
library("stringr")
library("tidytext")

df <- tibble(
  species=c("crow","robin","owl","turkey"),
            count=c(10,7,6,8))
# vector of characters
assholes <- c("crow","turkey")

# the opposite of the %in% operator
`%!in%` <- function(x,y)!(`%in%`(x,y))

df_mutate <- df %>% 
  mutate(quality=case_when(
    species %in% assholes ~ "bad",
    species %!in% assholes ~ "good"
    ))

# Function shortcut: Control + Option + X

lengthz <- function(length, number) {
  length + number
}

# Multi-line comments: Command + Shift + C

ggplot(data,aes(x,y)) +
  geom_bar(stat="identity")+
  # scale_x_discrete(
  #   name=NULL,
  #   labels=labels) +
  theme_clean()
  )

# Skip formatting after the fact

library("datapasta")

# Addins window, paste as vector: automatically adds quotations

# Look into magnet.crowdcafe.com/

# tidyverse.org/blog/2017/12/workflow-vs-script

# Library for better file structures
library(here)

# Create script for libraries used often and then load one!




