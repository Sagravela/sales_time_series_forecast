### SHINY TS

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)

# TS analysis
library(fabletools)
library(feasts)

# Load data
library(arrow)

# Visualization
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(DT)

## Load data
transactions_final <- read_parquet("transactions_final.parquet")
store <- read_parquet("store.parquet")
products <- read_parquet("products.parquet")
# Forecasting parquets
fc_train <- read_parquet("fc_train.parquet") |> mutate(.model = toupper(.model))
fc_arima <- read_parquet("fc_arima.parquet") |> mutate(.model = toupper(.model))
fc_stl <- read_parquet("fc_stl.parquet") |> mutate(.model = toupper(.model))
# Residuals
res <- read_parquet("residuals.parquet") |> mutate(.model = toupper(.model))

# Spinner type
options("spinner.type" = 8)

## Define UI
ui <- fluidPage(
  titlePanel("TS exploration"),
  fluidRow(
    column(
      3,
      selectInput(
        "upc_id", "UPC ID",
        c("", products$upc_id |> sort()),
        selected = ""
      )
    ),
    column(3, uiOutput("store_id"))
  ),
  tabsetPanel(
    tabPanel("Products Data", DT::dataTableOutput("products"), hr()),
    tabPanel("Store Data", DT::dataTableOutput("store"), hr()),
    tabPanel(
      "Correlations",
      fluidRow(hr(), column(10, withSpinner(plotOutput("corr", height = "800px")), offset = 1)),
      hr()
    ),
    tabPanel(
      "TS Plot",
      hr(),
      fluidRow(
        column(4, selectInput(
          "column",
          "Feature",
          colnames(
            transactions_final |>
              as_tibble() |>
              select(-upc_id, -store_id, -week, -week_end_date)
          ),
          selected = "units"
        ), offset = 1)
      ),
      withSpinner(plotlyOutput("plot")),
      # RMSE
      fluidRow(column(1), htmlOutput("rmse")),
      div(
        style = "text-align: right; margin-right: 10px;",
        em("Deselect model in legend to hide it in plot")
      ),
      hr()
    ),
    tabPanel(
      "Forecast",
      sidebarLayout(
        sidebarPanel(
          tags$h3("Settings", style = "font-weight:bold;"),
          # Fixed sidebar with checkboxes and select input
          checkboxInput("feature", label = "Feature", value = 0),
          checkboxInput("display", label = "Display", value = 0),
          checkboxInput("tpr_only", label = "TPR", value = 0),
          width = 2,
        ),
        mainPanel(
          # Main panel with the plot wrapped in a spinner
          withSpinner(plotlyOutput("forecast")),
          div(
            style = "text-align: right; margin-right: 10px;",
            em("Deselect model in legend to hide it in plot")
          ),
          width = 10
        )
      ),
      hr()
    ),
    tabPanel(
      "Residuals",
      fluidRow(column(10, withSpinner(plotlyOutput("res_acf")), offset = 1)),
      hr(),
      fluidRow(column(10, withSpinner(plotlyOutput("res_dist")), offset = 1)),
      hr()
    )
  )
)

## Server Logic
server <- function(input, output) {
  output$store_id <- renderUI({
    req(input$upc_id)
    selectInput(
      "store_id",
      "Store ID",
      transactions_final |>
        filter(upc_id == input$upc_id) |>
        distinct(store_id) |>
        dplyr::pull() |>
        sort()
    )
  })

  # Filter dataset based on input
  ts <- function(data) {
    reactive({
      req(input$upc_id, input$store_id)
      data |>
        filter(
          upc_id == as.numeric(input$upc_id),
          store_id == as.numeric(input$store_id)
        )
    })
  }
  rtransactions_final <- ts(transactions_final)
  rfc_train <- ts(fc_train)
  rfc_arima <- ts(fc_arima)
  rfc_stl <- ts(fc_stl)
  rres <- ts(res)

  # Render Products and Store data tables
  output$products <- DT::renderDataTable(products)
  output$store <- DT::renderDataTable(store)

  # Render TS plot with train/test forecasts
  output$plot <- renderPlotly({
    tryCatch(
      {
        if (input$column != "units") stop()
        p <- rtransactions_final() |>
          ggplot() +
          # Historic data plot
          geom_line(aes(week, units)) +
          # Train/test forecasts
          geom_line(aes(week, p_units, color = .model), data = rfc_train()) +
          # CI 95% and 80%
          geom_ribbon(
            aes(week, ymin = low95, ymax = up95, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_train()
          ) +
          geom_ribbon(
            aes(week, ymin = low80, ymax = up80, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_train()
          ) +
          # Scale, labels and theme
          scale_x_yearweek(breaks = "1 month") +
          labs(color = "Train/Test Forecast Model:", x = "Week", y = "Units sold") +
          theme(axis.text.x = element_text(angle = 45))
        plt <- ggplotly(p)

        # Modify legend manually
        plt$x$data[[2]]$name <- plt$x$data[[2]]$legendgroup <-
          plt$x$data[[4]]$name <- plt$x$data[[4]]$legendgroup <-
          plt$x$data[[6]]$name <- plt$x$data[[6]]$legendgroup <- "ARIMA"
        plt$x$data[[3]]$name <- plt$x$data[[3]]$legendgroup <-
          plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <-
          plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "STL"
        plt
      },
      error = function(e) {
        # If error is arised, render historic data plot
        p <- rtransactions_final() |>
          ggplot() +
          geom_line(aes(week, !!sym(input$column))) +
          scale_x_yearweek(breaks = "1 month") +
          labs(x = "Week", y = input$column) +
          theme(axis.text.x = element_text(angle = 45))
        ggplotly(p)
      }
    )
  })

  # RMSE display
  output$rmse <- renderUI({
    tryCatch(
      {
        rmse <- rfc_train() |>
          left_join(rtransactions_final(), by = c("week", "upc_id", "store_id")) |>
          as_tibble() |>
          group_by(.model) |>
          summarise(rmse = sqrt(mean((units - p_units)^2))) |>
          pivot_wider(names_from = .model, values_from = rmse)
        HTML(glue("<b>ARIMA RMSE</b>: {round(rmse$arima, 2)}&nbsp;&nbsp;&nbsp;<b>ETS RMSE</b>: {round(rmse$stl, 2)}"))
      },
      error = function(e) {
        # If error is arised, retrun nothing
        ""
      }
    )
  })

  # Render Correlation plot
  output$corr <- renderPlot({
    tryCatch(
      {
        rtransactions_final() |>
          as_tibble() |>
          mutate(
            feature = ifelse(feature == 1, "True", "False"),
            display = ifelse(display == 1, "True", "False"),
            tpr_only = ifelse(tpr_only == 1, "True", "False")
          ) |>
          select(
            -upc_id,
            -store_id,
            -week,
            -week_end_date
          ) |>
          GGally::ggpairs()
      },
      error = function(e) {
        # If error is arised, return void plot
        ggplot() +
          theme_void() +
          labs(title = "Not valid TS.")
      }
    )
  })

  # Render Forecast plot
  output$forecast <- renderPlotly({
    tryCatch(
      {
        v_feature <- as.numeric(input$feature)
        v_display <- as.numeric(input$display)
        v_tpr_only <- as.numeric(input$tpr_only)

        # Check if input is a valid combination
        if (v_tpr_only == 1 && (v_display == 1 || v_feature == 1)) {
          stop("Not valid combination.")
        }

        # Filter fable for input
        f_rfc_arima <- rfc_arima() |>
          filter(
            feature == v_feature,
            display == v_display,
            tpr_only == v_tpr_only
          )

        p <- rtransactions_final() |>
          ggplot() +
          # Historic data plot
          geom_line(aes(week, units)) +
          # ARIMA forecast plot
          geom_line(aes(week, p_units, color = .model), data = f_rfc_arima) +
          # CI 95% and 80%
          geom_ribbon(
            aes(week, ymin = low95, ymax = up95, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = f_rfc_arima
          ) +
          geom_ribbon(
            aes(week, ymin = low80, ymax = up80, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = f_rfc_arima
          ) +
          # STL forecast plot
          geom_line(aes(week, p_units, color = forcats::as_factor(.model)), data = rfc_stl()) +
          # CI 95% and 80%
          geom_ribbon(
            aes(week, ymin = low95, ymax = up95, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_stl()
          ) +
          geom_ribbon(
            aes(week, ymin = low80, ymax = up80, fill = .model),
            alpha = 0.3, show.legend = FALSE,
            data = rfc_stl()
          ) +
          # Scale, labs and theme
          scale_x_yearweek(breaks = "1 month") +
          labs(color = "Forecast Model:", x = "Week", y = "Units sold") +
          theme(axis.text.x = element_text(angle = 45))
        plt <- ggplotly(p)

        # Modify legend manually
        plt$x$data[[2]]$name <- plt$x$data[[2]]$legendgroup <-
          plt$x$data[[3]]$name <- plt$x$data[[3]]$legendgroup <-
          plt$x$data[[4]]$name <- plt$x$data[[4]]$legendgroup <- "ARIMA"
        plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <-
          plt$x$data[[6]]$name <- plt$x$data[[6]]$legendgroup <-
          plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "STL"
        plt
      },
      error = function(e) {
        # If error is arised, render historic data plot
        p <- rtransactions_final() |>
          autoplot(units) +
          scale_x_yearweek(breaks = "1 month") +
          labs(x = "Week", y = "Units sold") +
          theme(axis.text.x = element_text(angle = 45))
        ggplotly(p)
      }
    )
  })

  # Residuals autocorrelation plot
  output$res_acf <- renderPlotly({
    tryCatch(
      {
        rres() |>
          ACF(.innov, lag_max = 55) |>
          autoplot() +
          scale_x_continuous(breaks = c(13, 26, 39, 52)) +
          labs(x = "Lag [weeks]", y = "ACF")
      },
      error = function(e) {
        # If error is arised, return void plot
        ggplot() +
          theme_void() +
          labs(title = "Not valid TS.")
      }
    ) |>
      ggplotly()
  })

  # Residuals distribution plot
  output$res_dist <- renderPlotly({
    tryCatch(
      {
        rres() |>
          ggplot() +
          geom_histogram(aes(x = .innov, fill = .model)) +
          facet_wrap(~.model, scales = "free") +
          scale_x_continuous(breaks = scales::extended_breaks(10)) +
          labs(x = "Residuals", y = "Count") +
          theme(legend.position = "none")
      },
      error = function(e) {
        # If error is arised, return void plot
        ggplot() +
          theme_void() +
          labs(title = "Not valid TS.")
      }
    ) |>
      ggplotly()
  })
}

## Run App
shinyApp(ui, server)
