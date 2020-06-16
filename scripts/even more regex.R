library(rvest)
library(tidyverse)
library(stringr)

str_view_all(words,"(.)(.)\\2\\1",match = T)

colorz <- c("orange","blue","yellow","red","green","black")
colorz <- str_c(colorz,collapse="|")

unlist(str_extract_all(sentences,colorz))

str_replace_all(sentences,"([^ ]+) ([^ ]+) ([^ ]+)","\\1 \\3 \\2")


  unlist(str_match_all(words,"^[aeiou]+"))


vowels <- "[aeiou]"
consonants <- "[^aeiou]"

o_test <- paste0(sample(words,1)," ",sample(words,1)," ",sample(words,1))

wrd <- sample(words,1)

str_replace_all(wrd,"^.","\\1")


str_match_all(setences,"")

str_replace_all(wrd,"(^.)(.*)(.$)","\\3\\2\\1")

                

scrambler <- function(x){
  str_replace_all(x,"(^.)(.*)(.$)","\\3\\2\\1")
}
scrambler(c("hey","poop","hello"))

forwords <- unlist(str_match_all(words,"\\b\\w{4}\\b"))

scrambler(forwords)

index <- regex("
  (^.)# Finds first character
  (.*)# Finds next character matching zero or more times
  (.$)# Finds the last character"
  ,comments = TRUE)
swap <- regex("
  \\3# Third group
  \\2# Second group
  \\1# First group"
  ,comments = TRUE)








