# inst/app/decisionRubricR-shiny/app.R

# --- dependency check (safe for servers) ---
need <- c("shiny", "ggplot2", "DT", "dplyr", "tibble", "pROC")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) {
  if (interactive()) {
    install.packages(miss, repos = "https://cloud.r-project.org")
  } else {
    stop("Missing packages: ", paste(miss, collapse = ", "),
         "\nPlease install them and restart the app.")
  }
}

if (!requireNamespace("decisionRubricR", quietly = TRUE)) {
  stop("Package 'decisionRubricR' not found. Install from your source/GitHub and retry.")
}

library(shiny)
library(decisionRubricR)
library(DT)
library(ggplot2)

`%||%` <- function(a, b) if (!is.null(a)) a else b

ui <- fluidPage(
  # Add custom CSS for better styling
  tags$head(
    tags$style(HTML("
      .status-box { padding: 10px; margin: 10px 0; border-radius: 5px; }
      .status-error { background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }
      .status-success { background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb; }
      .status-warning { background-color: #fff3cd; color: #856404; border: 1px solid #ffeaa7; }
      .composite-display { text-align: center; padding: 20px; margin: 10px 0; border-radius: 8px; }
      .weights-display { font-family: monospace; font-size: 12px; }
    "))
  ),
  
  # -------- build tag in the title --------
  titlePanel(
    div("Decision-Quality Rubric",
        span(style = "font-size:12px;color:#888;margin-left:8px;",
             paste("build:", format(Sys.time(), "%Y-%m-%d %H:%M"))))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # File upload with better validation
      fileInput("file", "Upload CSV with columns 'truth' and 'prob'", 
                accept = c(".csv", "text/csv")),
      
      # Sample data option
      actionButton("loadSample", "Load Sample Data", class = "btn-info btn-sm"),
      
      tags$hr(),
      
      # Threshold input with validation
      numericInput("tau", "Decision threshold (τ)", 
                   value = 0.30, min = 0.01, max = 0.99, step = 0.01),
      helpText("Threshold for converting probabilities to binary decisions"),
      
      tags$hr(),
      
      h5("Composite weights"),
      helpText("Adjust weights for each component. Enable normalization to ensure they sum to 1."),
      
      # Weight sliders with better labels
      sliderInput("w1", "Calibration (Cal)", min = 0, max = 1, value = 0.25, step = 0.01),
      sliderInput("w2", "Discrimination (AUC)", min = 0, max = 1, value = 0.20, step = 0.01),
      sliderInput("w3", "Balanced Accuracy (MCC)", min = 0, max = 1, value = 0.15, step = 0.01),
      sliderInput("w4", "Skill Score (BSS)", min = 0, max = 1, value = 0.20, step = 0.01),
      sliderInput("w5", "Net Benefit (NB)", min = 0, max = 1, value = 0.20, step = 0.01),
      
      checkboxInput("normW", "Normalize weights to sum to 1", TRUE),
      
      fluidRow(
        column(6, actionButton("resetW", "Reset Weights", class = "btn-secondary btn-sm")),
        column(6, actionButton("equalW", "Equal Weights", class = "btn-secondary btn-sm"))
      ),
      
      tags$hr(),
      
      actionButton("go", "Calculate Rubric Score", class = "btn-primary"),
      
      tags$hr(),
      
      # Status and weights info
      uiOutput("status"),
      uiOutput("weightsInfo"),
      uiOutput("dataInfo")
    ),
    
    mainPanel(
      width = 8,
      # Error/warning display
      uiOutput("errorDisplay"),
      
      fluidRow(
        column(6, 
               h4("Composite Score"),
               uiOutput("compBox"),
               uiOutput("compText")
        ),
        column(6, 
               h4("Component Metrics"), 
               dataTableOutput("table")
        )
      ),
      
      tags$hr(),
      
      h4("Component Visualization"),
      plotOutput("barPlot", height = 300),
      
      # Additional insights
      conditionalPanel(
        condition = "input.go > 0",
        tags$hr(),
        h4("Interpretation Guide"),
        uiOutput("interpretation")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values for error handling
  values <- reactiveValues(
    error_msg = NULL,
    warning_msg = NULL,
    data_loaded = FALSE
  )

  # Reset to default weights
  observeEvent(input$resetW, {
    updateSliderInput(session, "w1", value = 0.25)
    updateSliderInput(session, "w2", value = 0.20)
    updateSliderInput(session, "w3", value = 0.15)
    updateSliderInput(session, "w4", value = 0.20)
    updateSliderInput(session, "w5", value = 0.20)
    updateCheckboxInput(session, "normW", value = TRUE)
  })
  
  # Equal weights
  observeEvent(input$equalW, {
    updateSliderInput(session, "w1", value = 0.20)
    updateSliderInput(session, "w2", value = 0.20)
    updateSliderInput(session, "w3", value = 0.20)
    updateSliderInput(session, "w4", value = 0.20)
    updateSliderInput(session, "w5", value = 0.20)
    updateCheckboxInput(session, "normW", value = TRUE)
  })
  
  # Load sample data
  observeEvent(input$loadSample, {
    # Create sample data
    set.seed(42)
    n <- 200
    prob <- runif(n, 0, 1)
    truth <- rbinom(n, 1, prob^1.5)  # Slightly miscalibrated
    
    sample_data <- data.frame(truth = truth, prob = prob)
    temp_file <- tempfile(fileext = ".csv")
    write.csv(sample_data, temp_file, row.names = FALSE)
    
    # Update file input (this is a workaround since we can't directly set fileInput)
    values$sample_data <- sample_data
    values$data_loaded <- TRUE
    values$error_msg <- NULL
    values$warning_msg <- "Sample data loaded (200 observations with slight miscalibration)"
  })

  # Raw + effective weights
  weights_raw <- reactive({
    c(Cal = input$w1, AUC = input$w2, MCC = input$w3, BSS = input$w4, NB = input$w5)
  })

  weights_eff <- reactive({
    w <- pmax(0, weights_raw())
    if (isTRUE(input$normW)) {
      s <- sum(w)
      if (s > 0) w / s else rep(1/5, 5)
    } else {
      w
    }
  })

  # Data loading with better error handling
  loaded_data <- reactive({
    if (values$data_loaded && !is.null(values$sample_data)) {
      return(values$sample_data)
    }
    
    req(input$file)
    
    tryCatch({
      # Read CSV with better error handling
      dat <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # Validate required columns
      if (!all(c("truth", "prob") %in% names(dat))) {
        stop("CSV must contain columns named 'truth' and 'prob'")
      }
      
      # Convert and validate truth column
      if (!all(dat$truth %in% c(0, 1, TRUE, FALSE))) {
        stop("'truth' column must contain only 0/1 or TRUE/FALSE values")
      }
      dat$truth <- as.numeric(as.logical(dat$truth))
      
      # Validate prob column
      if (!is.numeric(dat$prob)) {
        stop("'prob' column must be numeric")
      }
      
      if (any(is.na(dat$prob))) {
        stop("'prob' column contains missing values")
      }
      
      if (!all(dat$prob >= 0 & dat$prob <= 1)) {
        stop("'prob' values must be between 0 and 1")
      }
      
      # Check for minimum sample size
      if (nrow(dat) < 10) {
        stop("Dataset must contain at least 10 observations")
      }
      
      # Check for both classes
      if (sum(dat$truth) == 0) {
        values$warning_msg <- "Warning: No positive cases (truth=1) in dataset"
      } else if (sum(dat$truth) == nrow(dat)) {
        values$warning_msg <- "Warning: No negative cases (truth=0) in dataset"
      } else {
        values$warning_msg <- NULL
      }
      
      values$error_msg <- NULL
      dat
      
    }, error = function(e) {
      values$error_msg <- paste("Error loading data:", e$message)
      NULL
    })
  })

  # Base scoring when 'Score' is clicked
  baseScores <- eventReactive(input$go, {
    dat <- loaded_data()
    req(dat)
    
    tryCatch({
      # Validate threshold
      if (input$tau <= 0 || input$tau >= 1) {
        stop("Threshold must be between 0 and 1 (exclusive)")
      }
      
      rub <- score_rubric(
        truth = dat$truth, 
        prob = dat$prob, 
        tau = input$tau,
        weights = weights_eff(),
        normalize = FALSE  # We handle normalization above
      )
      
      values$error_msg <- NULL
      
      # Return named vector of components
      c(Cal = rub$cal, AUC = rub$auc, MCC = rub$mcc, BSS = rub$bss, NB = rub$nb)
      
    }, error = function(e) {
      values$error_msg <- paste("Error calculating rubric:", e$message)
      NULL
    })
  })

  # Composite score
  cs <- reactive({
    scores <- baseScores()
    req(scores)
    sum(weights_eff() * scores)
  })

  # Error display
  output$errorDisplay <- renderUI({
    if (!is.null(values$error_msg)) {
      div(class = "status-box status-error", 
          tags$strong("Error: "), values$error_msg)
    } else if (!is.null(values$warning_msg)) {
      div(class = "status-box status-warning", 
          tags$strong("Warning: "), values$warning_msg)
    }
  })

  # Weights info display
  output$weightsInfo <- renderUI({
    w <- round(weights_eff(), 3)
    div(class = "weights-display",
        HTML(sprintf("Effective weights: Cal=%.3f, AUC=%.3f, MCC=%.3f, BSS=%.3f, NB=%.3f<br>Sum=%.3f",
                     w[1], w[2], w[3], w[4], w[5], sum(w))))
  })
  
  # Data info display
  output$dataInfo <- renderUI({
    dat <- loaded_data()
    if (!is.null(dat)) {
      prev <- mean(dat$truth)
      div(
        tags$strong("Dataset info:"),
        tags$br(),
        sprintf("Observations: %d", nrow(dat)),
        tags$br(),
        sprintf("Prevalence: %.3f", prev),
        tags$br(),
        sprintf("Prob range: [%.3f, %.3f]", min(dat$prob), max(dat$prob))
      )
    }
  })

  # Composite score display
  output$compBox <- renderUI({
    if (is.null(baseScores())) return()
    
    s <- cs()
    col <- if      (s >= 0.80) "#4CAF50"
           else if (s >= 0.65) "#FFC107" 
           else if (s >= 0.50) "#FF9800"
           else               "#F44336"
    
    div(class = "composite-display",
        style = sprintf("background-color: %s; color: white;", col),
        div(style = "font-size: 3em; font-weight: bold;", sprintf("%.3f", s)),
        div(style = "font-size: 1.2em; margin-top: 10px;", "Composite Score")
    )
  })

  # Composite explanation
  output$compText <- renderUI({
    if (is.null(baseScores())) return()
    
    s <- cs()
    band <- cut(s, c(-Inf, 0.50, 0.65, 0.80, Inf),
                labels = c("Poor", "Fair", "Good", "Excellent"),
                right = FALSE)
    
    interpretation <- switch(as.character(band),
      "Poor" = "Consider reviewing model calibration and threshold selection.",
      "Fair" = "Reasonable performance, but there's room for improvement.",
      "Good" = "Solid performance across multiple decision quality dimensions.",
      "Excellent" = "Outstanding decision quality across all measured dimensions."
    )
    
    HTML(sprintf(
      "<p><strong>Performance:</strong> %s (%.3f)</p><p><em>%s</em></p>",
      as.character(band), s, interpretation
    ))
  })

  # Sub-metrics table
  output$table <- renderDataTable({
    scores <- baseScores()
    if (is.null(scores)) return()
    
    df <- data.frame(
      Component = c("Calibration", "Discrimination", "Balanced Accuracy", "Skill Score", "Net Benefit"),
      Metric = names(scores),
      Value = round(as.numeric(scores), 3),
      Weight = round(weights_eff(), 3),
      Weighted = round(as.numeric(scores) * weights_eff(), 3)
    )
    
    datatable(df, 
              options = list(dom = "t", pageLength = 10, scrollX = TRUE), 
              rownames = FALSE) %>%
      formatRound(columns = c("Value", "Weight", "Weighted"), digits = 3)
  })

  # Bar plot with improvements
  output$barPlot <- renderPlot({
    scores <- baseScores()
    if (is.null(scores)) return()
    
    df <- data.frame(
      Metric = factor(names(scores), levels = names(scores)),
      Value = as.numeric(scores),
      Weight = weights_eff()
    )
    df$Weighted <- df$Value * df$Weight
    
    p1 <- ggplot(df, aes(x = Metric, y = Value, fill = Metric)) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      geom_hline(yintercept = c(0.5, 0.65, 0.8), linetype = "dashed", alpha = 0.5) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      labs(title = "Component Scores", y = "Score", x = "") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p1)
  })

  # Interpretation guide
  output$interpretation <- renderUI({
    scores <- baseScores()
    if (is.null(scores)) return()
    
    div(
      h5("Component Interpretations:"),
      tags$ul(
        tags$li(strong("Calibration (Cal):"), "How well probabilities match actual frequencies (1.0 = perfect)"),
        tags$li(strong("Discrimination (AUC):"), "Ability to rank order cases by risk (0.5 = random, 1.0 = perfect)"),
        tags$li(strong("Balanced Accuracy (MCC):"), "Performance accounting for class imbalance (0.0 = random, 1.0 = perfect)"),
        tags$li(strong("Skill Score (BSS):"), "Improvement over baseline climatological forecast"),
        tags$li(strong("Net Benefit (NB):"), "Clinical/practical utility at the chosen threshold")
      ),
      
      h5("Improvement Suggestions:"),
      if (scores["Cal"] < 0.7) {
        div(class = "status-box status-warning", 
            "• Consider calibration techniques (Platt scaling, isotonic regression)")
      },
      if (scores["AUC"] < 0.7) {
        div(class = "status-box status-warning", 
            "• Focus on feature engineering and model selection for better discrimination")
      },
      if (scores["NB"] < 0.5) {
        div(class = "status-box status-warning", 
            "• Adjust decision threshold based on cost-benefit analysis")
      }
    )
  })

  # Status display
  output$status <- renderUI({
    if (is.null(input$go) || input$go == 0) {
      div(class = "status-box", 
          tags$em("Upload CSV data and click 'Calculate Rubric Score' to begin"))
    } else if (!is.null(values$error_msg)) {
      div(class = "status-box status-error", "Calculation failed - see error above")
    } else {
      div(class = "status-box status-success", 
          "Last calculated: ", format(Sys.time(), "%H:%M:%S"))
    }
  })
}

shinyApp(ui, server)
