# Load in Packages
library(shiny)
library(ggplot2)
library(broom)
library(huxtable)
library(dplyr)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Bivariate Correlations"),
  dashboardSidebar(
    sidebarMenu()
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Scatterplot",
        status = "primary",
        plotOutput("distPlot"),
        width = 6
      ),
      box(
        title = "Options",
        status = "success",
        sliderInput(
          "bins",
          "Number of Observations:",
          min = 1,
          max = 200,
          value = 50
        ),
        sliderInput(
          "diffx",
          "Maximum Value of X: ",
          min = 10,
          max = 100,
          value = 30
        ),
        sliderInput(
          "diffy",
          "Maximum Value of Y: ",
          min = 10,
          max = 100,
          value = 30
        ),
        checkboxInput(
          "reg",
          "Regression Line",
          value = FALSE
        ), width = 6
      ),
      box(
        title = "Summary",
        status = "success",
        uiOutput("summary")
      ),
      box(
        title = "Data Table",
        status = "warning",
        uiOutput("cor_info")
      )
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  # Shared simulation: x, y, their correlation test, and the fitted model
  # are computed once here and reused by all three outputs below, instead
  # of each output re-simulating its own copy.
  sim_data <- reactive({
    set.seed(100)
    x <- runif(input$bins, 10, input$diffx) |> round(0)
    y <- runif(input$bins, 10, input$diffy) |> round(0)

    list(
      x = x,
      y = y,
      ct = cor.test(x, y),
      mod = lm(y ~ x)
    )
  })

  output$distPlot <- renderPlot({
    d <- sim_data()

    p <-
      # Make X and Y a dataframe
      data.frame(
        x = d$x,
        y = d$y
      ) |>
      # Pass to ggplot
      ggplot(aes(x, y)) +
      # Make Condition Colors
      geom_point(color = ifelse(d$ct$p.value < .05, "darkgreen", "grey")) +
      labs(
        x = "X",
        y = "Y",
        title =
          paste0(
            "r = ",
            d$ct$estimate |> round(2),
            " | p = ",
            ifelse(d$ct$p.value < .05,
              "Significant ",
              "Nonsignificant "
            )
          )
      ) +
      theme_minimal() +
      theme(plot.title = element_text(
        size = 15,
        hjust = .5,
        face = "bold"
      ))

    if (input$reg) {
      p + geom_smooth(method = "lm")
    } else {
      p
    }
  })

  output$cor_info <- renderUI({
    sim_data()$ct |>
      tidy() |>
      select(estimate, p.value, parameter, conf.low, conf.high) |>
      rename(
        r = estimate,
        p = p.value,
        df = parameter
      ) |>
      mutate(r = r |> round(2)) |>
      huxtable() |>
      theme_article() |>
      to_html() |>
      HTML()
  })

  output$summary <- renderUI({
    huxreg(
      "Model" = summary(sim_data()$mod),
      error_pos = "right"
    ) |>
      to_html() |>
      HTML()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
