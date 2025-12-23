# App 7 Scatter plots
# Load libraries

library(shiny)
library(shinyFiles)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(haven)

ui <- fluidPage(
  titlePanel("Scatterplot with Labels and Correlation Annotation"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("datasrc", "Data source",
                   choices = c("Upload Excel or Stata or RData" = "upload",
                               "Select from environment" = "env"),
                   inline = TRUE),
      
      conditionalPanel(
        "input.datasrc == 'upload'",
        fileInput("file", "Upload Excel or Stata or RData", accept =  c(".RData", ".rds", ".xlsx", ".xls", ".dta")),
      ),
      
      conditionalPanel(
        "input.datasrc == 'env'",
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
      
      shinyDirButton("save_dir", "Choose folder", "Select folder to save plot", multiple = FALSE),
      verbatimTextOutput("folder_path"),
      actionButton("save_plot", "Save Plot")
    ),
    
    mainPanel(
      plotOutput("scatterplot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # -----------------------------------------------------------------
  # Folder selection (unchanged)
  # -----------------------------------------------------------------
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
  
  # -----------------------------------------------------------------
  # Load data (unchanged)
  # -----------------------------------------------------------------
  df <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    ext <- tolower(tools::file_ext(input$file$name))
    path <- input$file$datapath
    if (ext == "csv") {
      df(read.csv(path, stringsAsFactors = FALSE))
    } else if (ext %in% c("xlsx", "xls")) {
      df(readxl::read_excel(path) %>% as.data.frame())
    } else if (ext == "dta") {
      df(haven::read_dta(path) %>% as.data.frame())
    } else if (ext %in% c("rdata", "rda")) {
      e <- new.env(); load(path, envir = e)
      df(get(ls(e)[1], envir = e))
    } else if (ext == "rds") {
      df(readRDS(path))
    }
    showNotification("Data loaded", type = "message")
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset)
    obj <- get(input$env_dataset, envir = .GlobalEnv)
    if (is.data.frame(obj)) {
      df(obj)
      showNotification("Data loaded from environment", type = "message")
    } else {
      showNotification("Not a data frame", type = "error")
    }
  })
  
  # -----------------------------------------------------------------
  # Update variable selectors
  # -----------------------------------------------------------------
  observeEvent(df(), {
    num_vars <- names(df())[sapply(df(), is.numeric)]
    if (length(num_vars) >= 2) {
      updateSelectInput(session, "xvar", choices = num_vars, selected = num_vars[1])
      updateSelectInput(session, "yvar", choices = num_vars, selected = num_vars[2])
    }
  })
  
  # -----------------------------------------------------------------
  # Helper: decide correlation method + build annotation text
  # -----------------------------------------------------------------
  corr_info <- reactive({
    req(df(), input$xvar, input$yvar)
    d <- df() %>% select(all_of(c(input$xvar, input$yvar))) %>% drop_na()
    x <- d[[input$xvar]]; y <- d[[input$yvar]]
    
    # --- Assumptions for Pearson ---
    ok <- function(v) {
      n <- length(v)
      n >= 3 && n <= 5000 && var(v) > 0 &&
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
    
    list(method = method, label = label, x = x, y = y)
  })
  
  # -----------------------------------------------------------------
  # Build plot
  # -----------------------------------------------------------------
  # -----------------------------------------------------------------
  # Build plot
  # -----------------------------------------------------------------
  make_plot <- reactive({
    req(df(), input$xvar, input$yvar)
    data <- df()
    
    p <- ggplot(data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point(size = 3, color = "steelblue") +
      theme_bw() +
      labs(
        x = input$xvar,
        y = input$yvar,
        title = paste("Scatterplot of", input$yvar, "vs", input$xvar)
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12, face = "bold")
      )
    
    # Point labels
    if (input$show_labels) {
      p <- p + geom_label_repel(aes(label = rownames(data)), size = 3, color = "black")
    }
    
    # Regression line
    if (input$add_lm) {
      method <- if (input$use_loess) "loess" else "lm"
      p <- p + geom_smooth(method = method, se = input$add_ci, color = "red")
    }
    
    # Correlation annotation
    if (input$show_corr) {
      info <- corr_info()
      x_pos <- if (!is.na(input$label_x)) input$label_x else quantile(info$x, 0.1, na.rm = TRUE)
      y_pos <- if (!is.na(input$label_y)) input$label_y else quantile(info$y, 0.9, na.rm = TRUE)
      
      p <- p + annotate(
        "text",
        x = x_pos, y = y_pos,
        label = info$label,
        hjust = 0, vjust = 0,
        size = 7,
        colour = "black",
        fontface = "bold"
      )
    }
    
    p  # ←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←←
  })  
  # -----------------------------------------------------------------
  # Render plot
  # -----------------------------------------------------------------
  output$scatterplot <- renderPlot({ make_plot() })
  
  # -----------------------------------------------------------------
  # Save plot
  # -----------------------------------------------------------------
  observeEvent(input$save_plot, {
    req(save_dir(), df(), input$xvar, input$yvar)
    p <- make_plot()
    filename <- paste0("scatterplot_", input$yvar, "_vs_", input$xvar, ".png")
    ggsave(file.path(save_dir(), filename), plot = p, width = 8, height = 6, dpi = 300)
    showNotification(paste("Plot saved to", file.path(save_dir(), filename)), type = "message")
  })
}
shinyApp(ui, server)
shinyApp(ui, server)

