app_ui <- function() {
  fluidPage(
    titlePanel("SEAL Dive Typing"),

    sidebarLayout(
      sidebarPanel(
        tableOutput("label_summary"),
        br(),
        actionButton("train_btn", "Train!")
      ),

      mainPanel(
        plotOutput("dive_plot"),
        br(),
        fluidRow(
          column(2, actionButton("transit", "Transit")),
          column(2, actionButton("pelagic_forage", "Pelagic foraging")),
          column(2, actionButton("benthic_forage", "Benthic foraging")),
          column(2, actionButton("drift_pos", "Drift (positively buoyant)")),
          column(2, actionButton("drift_neg", "Drift (negatively buoyant)")),
          column(2, actionButton("benthic_rest", "Benthic resting"))
        )
      )
    )
  )
}

app_server <- function(dives) {
  dive_labels <- c(transit = "Transit",
                   pelagic_forage = "Pelagic foraging",
                   benthic_forage = "Benthic foraging",
                   drift_pos = "Drift (positively buoyant)",
                   drift_neg = "Drift (negatively buoyant)",
                   benthic_rest = "Benthic resting")

  function(input, output, session) {
    rv <- reactiveValues(
      current_dive = sample(unique(na.omit(dives$dive_id)), 1),
      # TODO: make model hyperparameters customizable
      model = tsf(min_interval = 3, n_estimators = 200, n_jobs = 1, random_state = 42),
      py_dives = preprocess_dives(dives),
      train_labels = tibble::tibble(dive_id = integer(),
                                    dive_label = character()),
      train_dives = NULL,
      predictions = NULL
    )

    current_dive <- reactive({
      dplyr::filter(dives, dive_id == rv$current_dive)
    })

    output$dive_plot <- renderPlot({
      pred_label <- NULL
      if(!is.null(rv$predictions)) {
        pred_proba <- rv$predictions[rv$current_dive, ]
        pred_label <- sprintf("%s (%.2f%%)",
                              dive_labels[which.max(pred_proba)],
                              max(pred_proba) * 100)
      }
      dive <- dplyr::filter(dives, dive_id == rv$current_dive)
      context_buffer <- 2 # 2 dives on either side
      context <- dplyr::filter(dives, between(dive_id, dive$dive_id[1] - 2, dive$dive_id[1] + 2))
      ggplot(context, aes(time, depth)) +
        geom_line() +
        geom_line(data = dive,
                  linewidth = 2) +
        scale_x_datetime(date_labels = "%Y-%m-%d %H:%M") +
        scale_y_reverse("Depth (m)") +
        labs(title = pred_label) +
        theme(axis.title.x = element_blank())
    })

    output$label_summary <- renderTable({
      dplyr::count(rv$train_labels, dive_label)
    })

    observeEvent(input$train_btn, {
      if (nrow(rv$train_labels) < 10) {
        output$model_status <- renderText("Need at least 10 labeled samples to train.")
        return()
      }

      rv$model <- tsf_fit(rv$model, rv$train_dives, rv$train_labels$dive_label)
      rv$predictions <- tsf_predict(rv$model, rv$py_dives)
    })

    # Observe dive labeling buttons
    # Gotta be a better way to do this!
    observe_label <- function(dive_label) {
      rv$train_labels <- rbind(
        rv$train_labels,
        tibble::tibble(dive_id = rv$current_dive,
                       dive_label = dive_label)
      )
      rv$train_dives <- pd$concat(list(rv$train_dives,
                                       slice_dive(py_dives, rv$current_dive)))
      rv$current_dive <- sample(unique(na.omit(dives$dive_id)), 1)
    }
    observeEvent(input$transit, observe_label("transit"))
    observeEvent(input$pelagic_forage, observe_label("pelagic_forage"))
    observeEvent(input$benthic_forage, observe_label("benthic_forage"))
    observeEvent(input$drift_pos, observe_label("drift_pos"))
    observeEvent(input$drift_neg, observe_label("drift_neg"))
    observeEvent(input$benthic_rest, observe_label("benthic_rest"))
  }
}

run_seal_app <- function(dives) {
  app <- shiny::shinyApp(ui = app_ui(), server = app_server(dives))
  shiny::runApp(app, launch.browser = TRUE)
}
