# inst/app/decisionRubricR-shiny/app.R

# --- dependency check (safe for servers) ---
need <- c("shiny","ggplot2","DT")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) {
  if (interactive()) {
    install.packages(miss, repos = "https://cloud.r-project.org")
  } else {
    stop(
      "Missing packages: ", paste(miss, collapse = ", "),
      "\nPlease install them and restart the app."
    )
  }
}

# decisionRubricR is your package; don't auto-install here
if (!requireNamespace("decisionRubricR", quietly = TRUE)) {
  stop("Package 'decisionRubricR' not found. Install from your source/GitHub and retry.")
}


# call libraries
library(shiny)
library(decisionRubricR)
library(DT)
library(ggplot2)

`%||%` <- function(a, b) if (!is.null(a)) a else b

ui <- fluidPage(
  titlePanel("Decision-Quality Rubric"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV with columns 'truth' and 'prob'", accept = ".csv"),
      numericInput("tau", "Decision threshold (τ)", value = 0.30, min = 0, max = 1, step = 0.01),
      actionButton("go", "Score"),
      tags$hr(),
      uiOutput("status")
    ),
    mainPanel(
      fluidRow(
        column(6, h4("Composite score"), uiOutput("compBox")),
        column(6, h4("Sub-metrics"), dataTableOutput("table"))
      ),
      plotOutput("barPlot", height = 260)
    )
  )
)

server <- function(input, output, session) {

  # Read & score when 'Score' is clicked -----------------------
  scored <- eventReactive(input$go, {
    req(input$file)
    dat <- read.csv(input$file$datapath)

    # basic validation
    validate(
      need(all(c("truth","prob") %in% names(dat)), "CSV must have 'truth' and 'prob' columns"),
      need(is.numeric(dat$prob), "'prob' must be numeric"),
      need(all(dat$prob >= 0 & dat$prob <= 1, na.rm = TRUE), "'prob' must be in [0,1]")
    )

    # run the rubric
    score_rubric(truth = dat$truth, prob = dat$prob, tau = input$tau)
  })

  # Composite box ----------------------------------------------
  output$compBox <- renderUI({
    s <- (scored()$composite %||% scored()$comp)
    validate(need(is.numeric(s) && length(s) == 1, "Composite not found"))
    col <- if      (s >= .80) "#4CAF50" else if (s >= .65) "#FFC107" else if (s >= .50) "#FF9800" else "#F44336"
    tags$div(style = sprintf("font-size:3em;font-weight:bold;color:%s;", col), sprintf("%.3f", s))
  })

  # Sub-metrics table ------------------------------------------
  output$table <- renderDataTable({
    s <- scored()
    comps <- c(Cal = s$cal, AUC = s$auc, MCC = s$mcc, BSS = s$bss, NB = s$nb %||% s$net_benefit)
    datatable(data.frame(Metric = names(comps), Value = round(unname(comps), 3)),
              options = list(dom = "t", pageLength = 5), rownames = FALSE)
  })

  # Bar plot ----------------------------------------------------
  output$barPlot <- renderPlot({
    s <- scored()
    df <- data.frame(Metric = c("Cal","AUC","MCC","BSS","NB"),
                     Value  = c(s$cal, s$auc, s$mcc, s$bss, (s$nb %||% s$net_benefit)))
    ggplot(df, aes(Metric, Value, fill = Metric)) +
      geom_col(show.legend = FALSE) +
      ylim(0, 1) +
      theme_minimal(base_size = 13)
  })

  # Status line -------------------------------------------------
  output$status <- renderUI({
    if (is.null(input$go) || input$go == 0) tags$em("Waiting for CSV…")
    else tags$span("Scored at ", format(Sys.time(), "%H:%M:%S"))
  })
}

shinyApp(ui, server)
