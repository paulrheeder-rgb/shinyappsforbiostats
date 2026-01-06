# ============================================================
# RMS Regression Shiny App — FINAL & CORRECTED
# Coefficient table uses modelsummary_rms correctly:
# - Automatic ORs for lrm, Betas for ols
# - No unsupported arguments
# ============================================================

library(shiny)
library(rms)
library(rmsMD)
library(readxl)
library(haven)
library(DT)
library(officer)
library(flextable)
library(ggplot2)
library(cowplot)

SAVE_PATH <- "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/Results"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

ui <- fluidPage(
  titlePanel("RMS Regression App with rmsMD Plots"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Load Data"),
      fileInput("file", "Upload Excel, Stata or RData",
                accept = c(".xlsx",".xls",".dta",".RData",".rda",".rds")),
      selectInput("env_dataset", "Or select from environment:", choices = "None"),
      actionButton("load_env", "Load"),
      hr(),
      
      h4("2. Model Setup"),
      radioButtons("model_type", "Model type:",
                   choices = c("Linear (ols)" = "ols", "Logistic (lrm)" = "lrm")),
      uiOutput("response_ui"),
      uiOutput("predictors_ui"),
      hr(),
      
      h4("3. Splines"),
      uiOutput("spline_vars_ui"),
      uiOutput("knots_ui"),
      tags$p("Note: Default = 3 knots (safer, reduces singularity risk). Increase only if variable has many unique values.",
             style = "font-size: 90%; color: gray;"),
      hr(),
      
      actionButton("fit", "Fit Model"),
      hr(),
      
      h4("4. Plot Customisation"),
      checkboxInput("lrm_prob", "Logistic: Plot probability (not OR)", FALSE),
      checkboxInput("shade_higher", "Shade higher values as inferior (red)", TRUE),
      textInput("global_ylab", "Global Y-label (optional)", ""),
      textInput("global_title", "Global title prefix (optional)", ""),
      
      fluidRow(
        column(6, numericInput("ylim_min", "Y-min", value = NA)),
        column(6, numericInput("ylim_max", "Y-max", value = NA))
      ),
      hr(),
      
      h4("5. Save Results"),
      actionButton("save", "Save Table (Word) + Plots (PNG)"),
      verbatimTextOutput("save_msg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("First 15 rows"),
                 DTOutput("data_table"),
                 br(),
                 h4("Data Summary"),
                 verbatimTextOutput("data_summary")),
        tabPanel("Model Summary",
                 h4("Coefficient Table"),
                 tableOutput("coef_table")),
        tabPanel("RCS Plots",
                 plotOutput("rcs_plots", height = "800px"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  model <- reactiveVal(NULL)
  
  # Update environment dropdown
  observe({
    all_objs <- ls(envir = .GlobalEnv)
    df_names <- character(0)
    for (obj in all_objs) {
      item <- try(get(obj, envir = .GlobalEnv), silent = TRUE)
      if (inherits(item, "data.frame")) {
        df_names <- c(df_names, obj)
      }
    }
    updateSelectInput(session, "env_dataset", choices = c("None", df_names))
  })
  
  # Load from file
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    path <- input$file$datapath
    
    df <- switch(tolower(ext),
                 xlsx = read_excel(path),
                 xls  = read_excel(path),
                 dta  = as.data.frame(read_dta(path)),
                 rdata = {e <- new.env(); load(path, envir = e); get(ls(e)[1], e)},
                 rda   = {e <- new.env(); load(path, envir = e); get(ls(e)[1], e)},
                 rds   = readRDS(path),
                 NULL)
    
    if (is.data.frame(df)) data(df)
  })
  
  # Load from environment
  observeEvent(input$load_env, {
    req(input$env_dataset != "None")
    data(get(input$env_dataset, envir = .GlobalEnv))
  })
  
  # Data preview
  output$data_table <- renderDT({
    req(data())
    datatable(head(data(), 15), options = list(scrollX = TRUE))
  })
  
  output$data_summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Dynamic UIs
  output$response_ui <- renderUI({
    req(data())
    selectInput("response", "Response variable:", choices = names(data()))
  })
  
  output$predictors_ui <- renderUI({
    req(data(), input$response)
    avail <- setdiff(names(data()), input$response)
    selectInput("predictors", "Predictors:",
                choices = avail, multiple = TRUE, selected = avail)
  })
  
  output$spline_vars_ui <- renderUI({
    req(data(), input$predictors)
    numeric_preds <- input$predictors[sapply(data()[, input$predictors, drop = FALSE], is.numeric)]
    
    if (length(numeric_preds) == 0) {
      tagList(h5("No numeric predictors available for splines.", style = "color: gray;"))
    } else {
      checkboxGroupInput("spline_vars", "Apply restricted cubic spline (rcs) to:",
                         choices = numeric_preds)
    }
  })
  
  output$knots_ui <- renderUI({
    req(input$spline_vars)
    if (length(input$spline_vars) == 0) return(NULL)
    
    lapply(input$spline_vars, function(v) {
      numericInput(paste0("knots_", v),
                   label = paste("Number of knots for", v),
                   value = 3, min = 3, max = 7, step = 1)
    })
  })
  
  # Reactive datadist
  dd_current <- reactive({
    req(data())
    datadist(data())
  })
  
  # Fit model
  observeEvent(input$fit, {
    req(data(), input$response, input$predictors, input$model_type)
    
    d <- data()
    
    dd <<- dd_current()
    options(datadist = "dd")
    
    preds <- input$predictors
    if (length(input$spline_vars) > 0) {
      for (v in input$spline_vars) {
        k <- input[[paste0("knots_", v)]] %||% 3
        preds[preds == v] <- paste0("rcs(", v, ", ", k, ")")
      }
    }
    
    form <- as.formula(paste(input$response, "~", paste(preds, collapse = " + ")))
    
    fitted <- if (input$model_type == "ols") {
      ols(form, data = d, x = TRUE, y = TRUE)
    } else {
      lrm(form, data = d, x = TRUE, y = TRUE)
    }
    
    model(fitted)
  })
  
  # Coefficient Table — Correct usage of modelsummary_rms
  output$coef_table <- renderTable({
    req(model())
    modelsummary_rms(model())
  }, rownames = TRUE)
  
  # RCS Plots
  output$rcs_plots <- renderPlot({
    req(model())
    
    f <- model()
    d <- data()
    dd <- dd_current()
    options(datadist = "dd")
    
    shade <- if (input$shade_higher) "higher" else "none"
    
    plots_list <- ggrmsMD(f, d, datadist = dd, combined = FALSE,
                          ylab = if (nzchar(input$global_ylab)) input$global_ylab else NULL,
                          lrm_prob = input$lrm_prob,
                          shade_inferior = shade)
    
    if (inherits(plots_list, "ggplot")) plots_list <- list(plots_list)
    
    custom_plots <- lapply(plots_list, function(p) {
      if (nzchar(input$global_title)) {
        p <- p + ggtitle(paste(input$global_title, p$labels$title %||% "", sep = " "))
      }
      if (!is.na(input$ylim_min) || !is.na(input$ylim_max)) {
        ylim_vals <- c(input$ylim_min, input$ylim_max)
        p <- p + coord_cartesian(ylim = ylim_vals[!is.na(ylim_vals)])
      }
      p
    })
    
    if (length(custom_plots) > 0) {
      print(plot_grid(plotlist = custom_plots, ncol = 1, align = "v"))
    } else {
      print(ggplot() + theme_void() +
              annotate("text", x = 0.5, y = 0.5,
                       label = "No spline terms in model", size = 8, colour = "gray50"))
    }
  })
  
  # Save results — Same correct table
  observeEvent(input$save, {
    req(model())
    if (!dir.exists(SAVE_PATH)) dir.create(SAVE_PATH, recursive = TRUE)
    
    prefix <- paste0("rms_", format(Sys.time(), "%Y%m%d_%H%M"))
    
    # Save coefficient table
    tab_df <- modelsummary_rms(model())
    
    ft <- flextable(tab_df) %>% theme_booktabs() %>% autofit()
    doc <- read_docx() %>% body_add_flextable(ft)
    word_path <- file.path(SAVE_PATH, paste0(prefix, "_coefficients.docx"))
    print(doc, target = word_path)
    
    # Save plots (unchanged)
    saved_plots <- 0
    model_formula <- deparse(formula(model()))
    has_splines <- grepl("rcs\\(", model_formula)
    
    if (has_splines) {
      dd <- dd_current()
      options(datadist = "dd")
      
      plots_raw <- ggrmsMD(model(), data(), datadist = dd, combined = FALSE,
                           lrm_prob = input$lrm_prob,
                           shade_inferior = if (input$shade_higher) "higher" else "none")
      
      if (inherits(plots_raw, "ggplot")) plots_raw <- list(plots_raw)
      
      if (length(plots_raw) > 0) {
        for (i in seq_along(plots_raw)) {
          p <- plots_raw[[i]]
          if (inherits(p, "ggplot")) {
            if (!is.na(input$ylim_min) || !is.na(input$ylim_max)) {
              ylim_vals <- c(input$ylim_min, input$ylim_max)
              p <- p + coord_cartesian(ylim = ylim_vals[!is.na(ylim_vals)])
            }
            if (nzchar(input$global_title)) {
              title <- p$labels$title %||% ""
              p <- p + ggtitle(paste(input$global_title, title, sep = " "))
            }
            
            p_name <- names(plots_raw)[i] %||% paste0("rcs_", i)
            ggsave(file.path(SAVE_PATH, paste0(prefix, "_", p_name, ".png")),
                   plot = p, width = 9, height = 6, dpi = 300)
            saved_plots <- saved_plots + 1
          }
        }
      }
    }
    
    plot_msg <- if (saved_plots > 0) {
      paste(saved_plots, "plot(s) saved.")
    } else {
      "No plots saved (no spline terms in model)."
    }
    
    output$save_msg <- renderText({
      paste("Outputs saved successfully!\n",
            "• Table: ", basename(word_path), "\n",
            "• ", plot_msg, "\n",
            "Folder: ", SAVE_PATH)
    })
  })
}

shinyApp(ui, server)
