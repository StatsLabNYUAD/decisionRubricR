# inst/app/decisionRubricR-shiny/app.R

# --- dependency check (safe for servers) ---
need <- c("shiny","ggplot2","DT")
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
  # -------- build tag in the title (helps verify a fresh deploy) --------
  titlePanel(
    div("Decision-Quality Rubric",
        span(style = "font-size:12px;color:#888;margin-left:8px;",
             paste("build:", format(Sys.time(), "%Y-%m-%d %H:%M"))))
  ),
  # ----------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV with columns 'truth' and 'prob'", accept = ".csv"),
      numericInput("tau", "Decision threshold (τ)", value = 0.30, min = 0, max = 1, step = 0.01),
      tags$hr(),
      h5("Composite weights"),
      helpText("Adjust and (optionally) normalise to sum to 1."),
      # sliders (use numericInput if you prefer)
      sliderInput("w1", "Cal (m1)", min = 0, max = 1, value = 0.25, step = 0.01),
      sliderInput("w2", "AUC (m2)", min = 0, max = 1, value = 0.20, step = 0.01),
      sliderInput("w3", "MCC (m3)", min = 0, max = 1, value = 0.15, step = 0.01),
      sliderInput("w4", "BSS (m4)", min = 0, max = 1, value = 0.20, step = 0.01),
      sliderInput("w5", "NB  (m5)", min = 0, max = 1, value = 0.20, step = 0.01),
      checkboxInput("normW", "Normalise weights to sum to 1", TRUE),
      actionButton("resetW", "Reset to defaults"),
      tags$hr(),
      actionButton("go", "Score"),
      tags$hr(),
      uiOutput("status"),
      uiOutput("weightsInfo")
    ),
    mainPanel(
      fluidRow(
        column(6, h4("Composite score"),
               uiOutput("compBox"),
               uiOutput("compText")),
        column(6, h4("Sub-metrics"), dataTableOutput("table"))
      ),
      plotOutput("barPlot", height = 260)
    )
  )
)

server <- function(input, output, session) {

  # Reset to default weights
  observeEvent(input$resetW, {
    updateSliderInput(session, "w1", value = 0.25)
    updateSliderInput(session, "w2", value = 0.20)
    updateSliderInput(session, "w3", value = 0.15)
    updateSliderInput(session, "w4", value = 0.20)
    updateSliderInput(session, "w5", value = 0.20)
    updateCheckboxInput(session, "normW", value = TRUE)
  })

  # Raw + effective (possibly normalised) weights ----------------
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

  output$weightsInfo <- renderUI({
    w <- round(weights_eff(), 3)
    HTML(sprintf("Effective weights: Cal=%0.3f, AUC=%0.3f, MCC=%0.3f, BSS=%0.3f, NB=%0.3f (sum=%0.3f)",
                 w[1], w[2], w[3], w[4], w[5], sum(w)))
  })

  # Base scoring (components only) when 'Score' is clicked -------
  baseScores <- eventReactive(input$go, {
    req(input$file)
    dat <- read.csv(input$file$datapath)
    validate(
      need(all(c("truth","prob") %in% names(dat)), "CSV must have 'truth' and 'prob' columns"),
      need(is.numeric(dat$prob), "'prob' must be numeric"),
      need(all(dat$prob >= 0 & dat$prob <= 1, na.rm = TRUE), "'prob' must be in [0,1]")
    )
    rub <- score_rubric(truth = dat$truth, prob = dat$prob, tau = input$tau)
    # Named vector of components
    c(Cal = rub$cal, AUC = rub$auc, MCC = rub$mcc, BSS = rub$bss, NB = rub$nb %||% rub$net_benefit)
  })

  # Composite from current weights + last scored components -------
  cs <- reactive({
    req(baseScores())
    sum(weights_eff() * baseScores())
  })

  # Composite box ------------------------------------------------
  output$compBox <- renderUI({
    s <- cs()
    col <- if      (s >= .80) "#4CAF50"
           else if (s >= .65) "#FFC107"
           else if (s >= .50) "#FF9800"
           else               "#F44336"
    tags$div(style = sprintf("font-size:3em;font-weight:bold;color:%s;", col),
             sprintf("%.3f", s))
  })

  # Composite explanation line ----------------------------------
  output$compText <- renderUI({
    s <- cs()
    band <- cut(s, c(-Inf, 0.50, 0.65, 0.80, Inf),
                labels = c("poor", "fair", "acceptable", "strong"),
                right = FALSE)
    HTML(sprintf(
      "CS ranges from 0 to 1 (higher is better). Your CS = <b>%.3f</b>, which falls in the <b>%s</b> band (thresholds: [0,0.50), [0.50,0.65), [0.65,0.80), [0.80,1]).",
      s, as.character(band)
    ))
  })

  # Sub-metrics table -------------------------------------------
  output$table <- renderDataTable({
    comps <- baseScores()
    datatable(
      data.frame(Metric = names(comps), Value = round(unname(comps), 3)),
      options = list(dom = "t", pageLength = 5), rownames = FALSE
    )
  })

  # Bar plot ----------------------------------------------------
  output$barPlot <- renderPlot({
    comps <- baseScores()
    df <- data.frame(Metric = names(comps), Value = unname(comps))
    ggplot(df, aes(Metric, Value, fill = Metric)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal(base_size = 13)
  })

  # Status line -------------------------------------------------
  output$status <- renderUI({
    if (is.null(input$go) || input$go == 0) tags$em("Waiting for CSV…")
    else tags$span("Scored at ", format(Sys.time(), "%H:%M:%S"))
  })
}

shinyApp(ui, server)
