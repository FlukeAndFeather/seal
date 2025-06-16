app_ui <- function() {
  fluidPage(
    titlePanel("Human-in-the-loop Time Series Labeling & Training"),

    sidebarLayout(
      sidebarPanel(
        uiOutput("label_ui"),
        actionButton("submit_label", "Submit Label"),
        actionButton("train_model", "Train Model"),
        verbatimTextOutput("model_status")
      ),

      mainPanel(
        plotOutput("series_plot"),
        textOutput("prediction_output")
      )
    )
  )
}

app_server <- function(input, output, session) {
  rv <- reactiveValues(
    current_index = 1,
    labeled_data = labels,
    model = NULL
  )

  current_series <- reactive({
    all_series[[rv$current_index]]
  })

  output$series_plot <- renderPlot({
    df <- current_series()
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      ggtitle(paste("Time Series ID:", df$id[1]))
  })

  output$label_ui <- renderUI({
    selectInput("label_input", "Label this time series:", choices = c("Class A", "Class B", "Class C"))
  })

  observeEvent(input$submit_label, {
    id <- current_series()$id[1]
    label <- input$label_input
    rv$labeled_data <- rv$labeled_data %>%
      filter(id != !!id) %>%
      bind_rows(data.frame(id = id, label = label))

    # Advance to next unlabeled series
    unlabeled_ids <- setdiff(sapply(all_series, function(s) s$id[1]), rv$labeled_data$id)
    if (length(unlabeled_ids) > 0) {
      rv$current_index <- which(sapply(all_series, function(s) s$id[1]) == unlabeled_ids[1])
    } else {
      rv$current_index <- 1
    }
  })

  observeEvent(input$train_model, {
    if (nrow(rv$labeled_data) < 2) {
      output$model_status <- renderText("Need at least 2 labeled samples to train.")
      return()
    }

    # Extract features (simple example: mean and std dev)
    get_features <- function(df) {
      data.frame(
        id = df$id[1],
        mean = mean(df$value),
        sd = sd(df$value)
      )
    }

    labeled_features <- do.call(rbind, lapply(all_series, get_features)) %>%
      inner_join(rv$labeled_data, by = "id")

    rf <- randomForest(label ~ mean + sd, data = labeled_features)
    rv$model <- rf
    output$model_status <- renderText("Model trained.")
  })

  output$prediction_output <- renderText({
    if (!is.null(rv$model)) {
      current <- current_series()
      f <- data.frame(
        mean = mean(current$value),
        sd = sd(current$value)
      )
      pred <- predict(rv$model, f, type = "prob")
      pred_label <- names(pred)[which.max(pred)]
      prob <- round(max(pred), 3)
      paste("Predicted:", pred_label, "| Probability:", prob)
    } else {
      "No model trained yet."
    }
  })
}
