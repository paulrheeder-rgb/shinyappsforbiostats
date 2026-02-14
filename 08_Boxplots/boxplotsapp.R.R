# app6.R Boxplots
# load libraries
library(shiny)
library(shinyFiles)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(rstatix)
library(ggpubr)
library(forcats)
library(DT)
library(RColorBrewer)
library(officer)
library(fs)
library(haven)

# Example dataset
example_data <- ToothGrowth %>% mutate(dose = factor(dose))

ui <- fluidPage(
  titlePanel("Boxplots with Multiple-Comparison p-values"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("datasrc", "Data source",
                   c("Use example: ToothGrowth" = "ex",
                     "Upload CSV/Excel/Stata" = "up",
                     "Upload RData" = "rdata",
                     "Select from environment" = "env"),
                   inline = TRUE),
      
      # --- Upload CSV/Excel/Stata ---
      conditionalPanel(
        "input.datasrc == 'up'",
        fileInput("file_upload", "Upload file",
                  accept = c(".csv", ".xlsx", ".xls", ".dta")),
        checkboxInput("header", "Header", TRUE),
        radioButtons("sep", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
        radioButtons("quote", "Quote", c(None = "", "Double Quote" = "\"", "Single Quote" = "'"), selected = "\"")
      ),
      
      # --- Upload RData ---
      conditionalPanel(
        "input.datasrc == 'rdata'",
        fileInput("file_rdata", "Upload RData (.RData/.rda)")
      ),
      
      # --- Load from environment ---
      conditionalPanel(
        "input.datasrc == 'env'",
        selectInput("env_dataset", "Select dataset from R environment", choices = ls(envir = .GlobalEnv)),
        actionButton("load_env", "Load")
      ),
      
      hr(),
      uiOutput("varpickers"),
      hr(),
      selectInput("test", "Pairwise test",
                  choices = c("t-test (Welch)" = "t", "Wilcoxon/Mann–Whitney" = "w")),
      selectInput("padj", "Multiple-testing adjustment",
                  choices = c("Holm" = "holm", "Bonferroni" = "bonferroni", "Benjamini-Hochberg" = "BH", "None" = "none"),
                  selected = "holm"),
      checkboxInput("show_points", "Show points", TRUE),
      checkboxInput("violin", "Add violin underlay", FALSE),
      checkboxInput("flip", "Flip coordinates", FALSE),
      checkboxInput("logy", "Log10 Y-axis", FALSE),
      checkboxInput("show_pbars", "Show pairwise p-value bars", TRUE),
      hr(),
      shinyDirButton("save_dir", "Choose folder", "Select folder to save plots", multiple = FALSE),
      verbatimTextOutput("folder_path"),
      actionButton("save_plots", "Save plots")
    ),
    
    mainPanel(
      plotOutput("plt", height = "700px"),
      hr(),
      DTOutput("pairwise_tbl")
    )
  )
)

server <- function(input, output, session){
  # Define local and Google Drive folders
  roots <- c(
    Home = normalizePath("~"),
    Results = "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/Results"
  )
  shinyDirChoose(input, "save_dir", roots = roots, session = session)
  
  save_dir <- reactiveVal(NULL)
  
  observeEvent(input$save_dir, {
    req(input$save_dir)
    path <- parseDirPath(roots, input$save_dir)
    save_dir(path)
  })
  
  output$folder_path <- renderText({
    req(save_dir())
    paste("Selected folder:", save_dir())
  })
  
  dat <- reactiveVal(NULL)
  
  observe({ if (input$datasrc == "ex") dat(example_data) })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$file_upload$datapath, header = input$header, sep = input$sep, quote = input$quote)
      } else if (ext %in% c("xls", "xlsx")) {
        readxl::read_excel(input$file_upload$datapath) %>% as.data.frame()
      } else if (ext %in% c("dta", "sav", "sas7bdat")) {
        haven::read_dta(input$file_upload$datapath) %>% as.data.frame()
      } else stop("Unsupported file type")
    }, error = function(e){
      showNotification(e$message, type = "error")
      return(NULL)
    })
    
    # ✅ Automatically convert labelled variables to factors
    if (!is.null(df)) {
      df <- dplyr::mutate(df, dplyr::across(where(haven::is.labelled), haven::as_factor))
      dat(df)
    }
  })
  
  observeEvent(input$file_rdata, {
    req(input$file_rdata)
    e <- new.env()
    load(input$file_rdata$datapath, envir = e)
    dat(as.data.frame(e[[ls(e)[1]]]))
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset)
    obj <- get(input$env_dataset, envir = .GlobalEnv)
    if (!is.data.frame(obj)) showNotification("Selected object is not a data frame", type = "error")
    else dat(as.data.frame(obj))
  })
  
  output$varpickers <- renderUI({
    df <- dat()
    req(df)
    num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    tagList(
      selectInput("yvar", "Numeric response (Y)", choices = num_vars, selected = num_vars[1]),
      selectInput("xvar", "Grouping (X; categorical)", choices = cat_vars, selected = cat_vars[1]),
      selectInput("facet", "Facet (optional)", choices = c("None" = "", cat_vars), selected = "")
    )
  })
  
  dsel <- reactive({
    req(dat(), input$yvar, input$xvar)
    vars <- c(input$yvar, input$xvar)
    if (!is.null(input$facet) && input$facet != "") vars <- c(vars, input$facet)
    df <- dat() %>% select(all_of(vars)) %>%
      rename(.y = all_of(input$yvar), .x = all_of(input$xvar))
    if (!is.null(input$facet) && input$facet != "") df <- df %>% rename(.f = all_of(input$facet))
    df <- df %>% drop_na()
    df$.x <- as.factor(df$.x)
    if (".f" %in% names(df)) df$.f <- as.factor(df$.f)
    df
  })
  
  pairwise_res <- reactive({
    df <- dsel()
    req(nlevels(df$.x) >= 2)
    if (!(".f" %in% names(df))) {
      if (input$test == "t") df %>% pairwise_t_test(.y ~ .x, pool.sd = FALSE, var.equal = FALSE, p.adjust.method = input$padj)
      else df %>% pairwise_wilcox_test(.y ~ .x, p.adjust.method = input$padj)
    } else {
      if (input$test == "t") df %>%
        group_by(.f) %>%
        pairwise_t_test(.y ~ .x, pool.sd = FALSE, var.equal = FALSE, p.adjust.method = input$padj) %>%
        ungroup()
      else df %>%
        group_by(.f) %>%
        pairwise_wilcox_test(.y ~ .x, p.adjust.method = input$padj) %>%
        ungroup()
    }
  })
  
  make_plot <- reactive({
    df <- dsel()
    req(nlevels(df$.x) >= 2)
    pal <- scales::hue_pal()(nlevels(df$.x))
    
    # --- Main layers ---
    base <- ggplot(df, aes(.x, .y, fill = .x)) +
      # ✅ semi-transparent boxplot
      geom_boxplot(outlier.shape = NA, alpha = 0.4, color = "black", width = 0.6) 
    
    # Optional violin underlay
    if (input$violin)
      base <- base + geom_violin(alpha = 0.3, trim = FALSE, color = NA)
    
    # Jittered points overlay
    if (input$show_points)
      base <- base + geom_jitter(aes(color = .x), width = 0.12, alpha = 0.6, size = 2)
    
    # --- Aesthetics ---
    base <- base +
      scale_fill_manual(values = pal) +
      scale_color_manual(values = pal) +
      labs(
        title = paste("Comparing", input$yvar, "across", input$xvar),
        x = input$xvar,
        y = input$yvar
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face ="bold"),
        axis.text = element_text(size = 12, face = "bold"),
        legend.position = "none"
      )
    
    # Axis & facets
    if (input$logy) base <- base + scale_y_continuous(trans = "log10")
    if (".f" %in% names(df) && nlevels(df$.f) > 1) base <- base + facet_wrap(~.f)
    if (input$flip) base <- base + coord_flip()
    
    # --- p-value bars ---
    if (input$show_pbars) {
      pdat <- pairwise_res()
      if (nrow(pdat) > 0) {
        pdat <- pdat %>%
          mutate(p.label = ifelse(p.adj < 0.001, "p < 0.001",
                                  paste0("p = ", signif(p.adj, 3))))
        pdat <- tryCatch({
          add_xy_position(pdat, x = ".x")
        }, error = function(e) {
          pdat$y.position <- max(df$.y, na.rm = TRUE) * 1.05
          pdat
        })
        base <- base +
          stat_pvalue_manual(pdat, label = "p.label", xmin = "group1", xmax = "group2",
                             y.position = "y.position", tip.length = 0.02, size = 5)
      }
    }
    
    base
  })
  
  output$plt <- renderPlot({ make_plot() })
  
  output$pairwise_tbl <- renderDT({ pairwise_res() })
  
  observeEvent(input$save_plots, {
    req(save_dir())
    dir.create(save_dir(), recursive = TRUE, showWarnings = FALSE)
    p <- make_plot()
    if (is.null(p)) {
      showNotification("Plot could not be generated.", type = "error")
      return()
    }
    
    # Save PNG
    png(file.path(save_dir(), paste0("boxplot_", input$yvar, "_by_", input$xvar, ".png")), width = 1200, height = 800)
    print(p)
    dev.off()
    
    # Save Word document
    doc <- read_docx() %>% body_add_gg(p)
    print(doc, target = file.path(save_dir(), "Boxplot.docx"))
    
    showNotification(paste("Saved plot and Word doc to", save_dir()), type = "message")
  })
}

shinyApp(ui, server)

