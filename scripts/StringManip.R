library(stringr)
library(stringi)

# Create a string
x <- "Here is a sentence, let us use it for this example."

############# Case Manipulation ####################
# Convert the string to lowercase, uppercase, and title case
str_to_lower(x)
str_to_upper(x)
str_to_title(x)

# Find the length of the string
str_length(x)
# It might be of interest to see how long the sentence is in terms of
# how many letters each word is

# First we need to split the string into words
# Note: strsplit converts the vector into a list
# Wrap it unist()
x_split <- unlist(strsplit(x, " "))

# Now the sentence is a string vector, so the str_length
# function will vectorize it

str_length(x_split)

############## String Manipulation ###############

# Append text to a string 

# Suffix
str_c(x_split,"hello",sep=" ")
str_c(x_split,"hello",sep="")
# Prefix
str_c("hello",x_split,sep=" ")
str_c("hello",x_split,sep="")
# Sort the sentence from A-Z
str_sort(x_split)

################## Regular Expressions ############

# What does each word start with
# Note: make sure to unlist
unlist(str_match_all(x_split,"^."))

# Replace a word with something else
# We will return the string into sentence format
x_sent <- str_flatten(x_split," ")

# Replace any three letter word with "cake"
str_replace(x_sent,"\\b\\w{2}\\b","cake")
# This only replaces one instance

str_replace_all(x_sent,"\\b\\w{2}\\b","cake")

###############Remove a pattern###############

# Remove any word that starts with a vowel
str_remove_all(x_sent,"\\b[aeiou][a-z]+")

# Remove any word greater than 5 letters
str_remove_all(x_sent,"\\b\\w{5,}\\b")

#############Detect ########################

# Does anything in your string match this pattern
# Returns the values that match
unlist(str_match_all(x_sent,"\\b[aeiou]\\w+"))

# Returns logical
str_detect(x_sent,"\\b[aeiou]\\w+")


Regular Expressions use special characters, so they sometimes
need to be escaped:
  
In other words 

"\\d" - Looks for numbers
"\\w" - Looks for word characters
"\\b" - Looks for word boundaries
"{n}" - Looks for pattern exactly n times
"{n,}"- Looks for pattern >= n times
"{n,k}" - Looks for pattern between n and k times




           
           