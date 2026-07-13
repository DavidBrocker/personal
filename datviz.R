library(tidyverse)
library(gtrendsR)
library(ggtext)

dat <-
  gtrends(keyword = c("stats", "plots", "psychology"))$interest_over_time |>
  filter(hits != "<1") |>
  mutate(hits = as.numeric(hits))

p1 <-
  dat |>
  ggplot(aes(date, hits, color = keyword)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Number of Searches (Hit)",
    title = "Search Term Popularity",
    subtitle = "For data-related words: plots/psychology/stats"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic")
  )

p2 <-
  dat |>
  mutate(
    colors = case_when(
      keyword == "plots" ~ "#E76F51",
      keyword == "psychology" ~ "#2A9D8F",
      keyword == "stats" ~ "#264653"
    )
  ) |>
  ggplot(aes(date, hits, color = colors)) +
  geom_line(linewidth = 1) +
  labs(
    x = "",
    y = "Number of Searches (Hits)\n",
    color = "Keyword",
    title = "Google Search Term Popularity from 2020-2025",
    subtitle = "Over the past five years, <span style='color:#2A9D8F'><strong>psychology</strong></span> and <span style='color:#264653'><strong>stats</strong></span> have steadily risen in search popularity <br> while <span style='color:#E76F51'><strong>plots</strong></span> remains surprisingly flat.",
    caption = "Data Source: {gtrendsR}"
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y"
  ) +
  scale_color_identity() +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = "cornsilk", color = NA),
    panel.background = element_rect(fill = "cornsilk", color = NA),
    panel.grid = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      face = "italic",
      linetype = 1,
      fill = "white"
    ),
    plot.caption = element_markdown(
      face = "italic",
      size = 10,
      hjust = .5
    ),
    plot.margin = margin(10, 20, 25, 20),
    panel.spacing = unit(0.4, "lines")
  )
