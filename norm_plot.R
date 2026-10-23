library(tidyverse)

# make dataset
set.seed(123)
x <- rnorm(100, 5, 2)

# Plot Normal Distribution
tibble(
  x = x,
  xd = dnorm(x, mean(x), sd(x))
) |>
  ggplot(aes(x, xd)) +
  geom_line() +
  theme_minimal(
    base_family = "Fraunces"
  ) +
  labs(
    x = "\nRaw Scores",
    y = "Probablity Density of Scores\n",
    title = "Normality of Variable X (N = 100)",
    subtitle = "\u00b14 4SD"
  ) +
  ylim(0, .30) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold")
  ) +
  scale_x_continuous(
    breaks = seq(from = 5 + (-2) * 4, to = 5 + (2) * 4, by = 2),
    sec.axis = sec_axis(
      transform = ~ (. - mean(x)) / sd(x),
      name = "Standardized Scores\n",
      breaks = -4:4
    )
  )


# Corrected Version ---------------------------------------------------------------------------

plot_normality <- function(data, var, base_family = "Fraunces") {
  v <- dplyr::pull(data, {{ var }})
  m <- mean(v, na.rm = TRUE)
  s <- sd(v, na.rm = TRUE)
  n <- sum(!is.na(v))

  tibble(x = v, xd = dnorm(v, m, s)) |>
    ggplot(aes(x, xd)) +
    geom_line() +
    theme_minimal(base_family = base_family) +
    labs(
      x = "\nRaw Scores",
      y = "Probability Density of Scores\n",
      title = glue::glue("Normality of {rlang::as_label(enquo(var))} (N = {n})"),
      subtitle = "\u00b14 SD"
    ) +
    ylim(0, 0.30) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold")
    ) +
    scale_x_continuous(
      breaks = seq(m - 4 * s, m + 4 * s, by = s),
      sec.axis = sec_axis(
        transform = ~ (. - m) / s,
        name = "Standardized Scores",
        breaks = -4:4
      )
    )
}

# usage
tibble(score = rnorm(100, 5, 2)) |> plot_normality(score)

# Test Sampling Distribution
replicate(
  n = 15,
  expr =
    tibble(
      m = rnorm(100, 5, 2),
    ) |>
      summarize_all(mean)
)
