# App 7 Scatter plots with Word download + Pearson/Spearman annotation
library(shiny)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(haven)
library(readxl)
library(officer)

ui <- fluidPage(
  titlePanel("Scatterplot with Labels and Correlation Annotation"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("datasrc", "Data source",
                   choices = c("Upload Excel/Stata/RData" = "upload",
                               "Select from environment" = "env"),
                   inline = TRUE),
      
      conditionalPanel("input.datasrc == 'upload'",
                       fileInput("file", "Upload Excel/Stata/RData",
                                 accept = c(".RData", ".rds", ".xlsx", ".xls", ".dta", ".csv"))
      ),
      
      conditionalPanel("input.datasrc == 'env'",
                       selectInput("env_dataset", "Choose dataset from environment", choices = ls(envir = .GlobalEnv)),
                       actionButton("load_env", "Load")
      ),
      
      selectInput("xvar", "X Variable:", choices = NULL),
      selectInput("yvar", "Y Variable:", choices = NULL),
      
      checkboxInput("show_labels", "Show point labels", FALSE),
      checkboxInput("add_lm", "Add linear regression line", TRUE),
      checkboxInput("add_ci", "Show 95% CI for regression", TRUE),
      checkboxInput("use_loess", "Use LOESS smoother", FALSE),
      checkboxInput("show_corr", "Show R & p-value", TRUE),
      numericInput("label_x", "R & p-value X position:", value = NA),
      numericInput("label_y", "R & p-value Y position:", value = NA),
      
      hr(),
      textInput("plot_title", "Custom plot title", value = "My Scatterplot"),
      textInput("save_name_scatter", "Word file name (no ext)", value = "Scatterplot"),
      downloadButton("download_scatter", "Download Word File", class = "btn-success")
    ),
    
    mainPanel(
      plotOutput("scatterplot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  df <- reactiveVal(NULL)
  
  # --- Load data ---
  observeEvent(input$file, {
    ext <- tolower(tools::file_ext(input$file$name))
    path <- input$file$datapath
    if (ext == "csv") df(read.csv(path, stringsAsFactors = FALSE))
    else if (ext %in% c("xlsx", "xls")) df(readxl::read_excel(path) %>% as.data.frame())
    else if (ext == "dta") df(haven::read_dta(path) %>% as.data.frame())
    else if (ext %in% c("rdata", "rda")) { e <- new.env(); load(path, envir = e); df(get(ls(e)[1], e)) }
    else if (ext == "rds") df(readRDS(path))
    showNotification("Data loaded", type = "message")
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset)
    obj <- get(input$env_dataset, envir = .GlobalEnv)
    if (is.data.frame(obj)) df(obj)
  })
  
  # --- Update variable selectors ---
  observeEvent(df(), {
    num_vars <- names(df())[sapply(df(), is.numeric)]
    if (length(num_vars) >= 2) {
      updateSelectInput(session, "xvar", choices = num_vars, selected = num_vars[1])
      updateSelectInput(session, "yvar", choices = num_vars, selected = num_vars[2])
    }
  })
  
  # --- Correlation info with Pearson/Spearman logic ---
  corr_info <- reactive({
    req(df(), input$xvar, input$yvar)
    d <- df() %>% select(all_of(c(input$xvar, input$yvar))) %>% drop_na()
    x <- d[[input$xvar]]; y <- d[[input$yvar]]
    
    # Decide Pearson vs Spearman based on normality
    ok <- function(v) {
      n <- length(v)
      n >= 3 && var(v) > 0 &&
        !is.null(try(shapiro.test(v)$p.value, silent = TRUE)) &&
        shapiro.test(v)$p.value > 0.05
    }
    method <- if (ok(x) && ok(y)) "pearson" else "spearman"
    cor_res <- cor.test(x, y, method = method)
    
    r_val <- round(cor_res$estimate, 3)
    p_val <- cor_res$p.value
    p_txt <- if (p_val < 0.001) "p < 0.001" else sprintf("p = %.3f", p_val)
    
    label <- paste0(
      ifelse(method == "pearson", "Pearson", "Spearman"),
      " R = ", r_val, ", ", p_txt
    )
    
    list(label = label, x = x, y = y)
  })
  
  # --- Build plot ---
  make_plot <- reactive({
    req(df(), input$xvar, input$yvar)
    data <- df()
    p <- ggplot(data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point(size = 3, color = "steelblue") +
      theme_bw() +
      labs(x = input$xvar, y = input$yvar, title = input$plot_title) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 12, face = "bold"))
    
    if (input$show_labels) {
      p <- p + geom_label_repel(aes(label = rownames(data)), size = 3)
    }
    if (input$add_lm) {
      method <- if (input$use_loess) "loess" else "lm"
      p <- p + geom_smooth(method = method, se = input$add_ci, color = "red")
    }
    if (input$show_corr) {
      info <- corr_info()
      x_pos <- if (!is.na(input$label_x)) input$label_x else mean(info$x, na.rm = TRUE)
      y_pos <- if (!is.na(input$label_y)) input$label_y else max(info$y, na.rm = TRUE) * 0.95
      p <- p + annotate("text", x = x_pos, y = y_pos, label = info$label,
                        hjust = 0, vjust = 0, size = 5, fontface = "bold")
    }
    p
  })
  
  output$scatterplot <- renderPlot({ make_plot() })
  
  # --- Download Word file ---
  output$download_scatter <- downloadHandler(
    filename = function() paste0(input$save_name_scatter, ".docx"),
    content = function(file) {
      req(make_plot())
      p <- make_plot()
      doc <- read_docx() %>% body_add_gg(p, width = 7, height = 6)
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)


