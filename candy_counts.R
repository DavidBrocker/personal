library(dplyr)
library(ggplot2)
library(forcats)
library(tibble)

candy_counts <- tribble(
  ~Candy, ~Count,
  "Peanut MM", 24,
  "Regular MM", 19,
  "Snickers", 19,
  "Almond Joy", 22,
  "Milky Way", 12,
  "Hershey's", 12,
  "KitKat", 16,
  "Butterfinger", 12,
  "Reese's Cups", 18
)


costco_2024 <- tribble(
  ~candy, ~count, ~year,
  "Reese’s Cup", 39, 2024,
  "Snickers", 27, 2024,
  "Regular MM", 18, 2024,
  "Twix", 17, 2024,
  "Kit Kat", 17, 2024,
  "Milky Ways", 15, 2024,
  "Hershey", 14, 2024,
  "Peanut MM", 9, 2024,
  "3 Musketeers", 9, 2024,
  "100 Grand", 1, 2024
)

costco_2024
