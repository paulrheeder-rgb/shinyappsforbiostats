# App 5 Graphs All - Cleaned for browser download
library(shiny)
library(haven)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(rlang)
library(officer)

ui <- fluidPage(
  titlePanel("Variable Explorer: Numeric & Categorical Plots"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Select data source:",
                   choices = c("Upload file" = "upload",
                               "R Environment" = "env"),
                   inline = TRUE),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Upload Excel or Stata or RData", 
                  accept = c(".RData", ".rds", ".xlsx", ".xls", ".dta"))
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'env'",
        uiOutput("env_data_ui")
      ),
      
      uiOutput("numeric_ui"),
      uiOutput("factor_ui"),
      
      radioButtons("cat_plot_type", "Categorical Plot Type:",
                   choices = c("Count" = "count", "Percent" = "percent")),
      
      actionButton("plot_btn", "Generate Plots"),
      hr(),
      textInput("save_name", "Word file name (without extension)", value = "All_Plots"),
      downloadButton("download_docx", "Download Word File", class = "btn-success")
    ),
    
    mainPanel(
      h4("Combined Plots"),
      plotOutput("combined_plot", height = "1200px")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- UI for R environment datasets ----
  output$env_data_ui <- renderUI({
    env_dfs <- Filter(function(x) is.data.frame(get(x, envir = .GlobalEnv)),
                      ls(envir = .GlobalEnv))
    if (length(env_dfs) == 0)
      return(helpText("No data frames found in the R environment."))
    selectInput("env_df", "Select dataset from R environment:",
                choices = env_dfs, selected = env_dfs[1])
  })
  
  # ---- Data upload or environment load ----
  df <- reactive({
    if (input$data_source == "upload") {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        read.csv(input$file$datapath)
      } else if (ext == "RData") {
        e <- new.env()
        load(input$file$datapath, envir = e)
        dfs <- Filter(is.data.frame, as.list(e))
        dfs[[1]]
      }
    } else if (input$data_source == "env") {
      req(input$env_df)
      get(input$env_df, envir = .GlobalEnv)
    }
  })
  
  # ---- Variable selectors ----
  output$numeric_ui <- renderUI({
    req(df())
    num_vars <- names(df()[sapply(df(), is.numeric)])
    selectInput("numeric_vars", "Select Numeric Variables:", choices = num_vars, multiple = TRUE)
  })
  
  output$factor_ui <- renderUI({
    req(df())
    fac_vars <- names(df()[sapply(df(), function(x) is.factor(x) || is.character(x))])
    selectInput("factor_vars", "Select Factor Variables:", choices = fac_vars, multiple = TRUE)
  })
  
  # ---- Plot functions ----
  make_normality_panels <- function(data, varname) {
    x <- suppressWarnings(as.numeric(data[[varname]]))
    x <- na.omit(x)
    if (length(x) < 3) {
      msg <- paste(varname, "has too few non-missing values")
      return(list(ggplot() + ggtitle(msg), ggplot() + ggtitle(msg)))
    }
    
    x_mean <- mean(x)
    x_sd <- sd(x)
    sw <- shapiro.test(x)
    sw_text <- paste0("Shapiro-Wilk W = ", round(sw$statistic, 3),
                      ", p = ", format.pval(sw$p.value, digits = 3, eps = .001))
    
    p1 <- ggplot(data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = (max(x) - min(x)) / 30,
                     color = "blue", fill = "lightblue") +
      stat_function(fun = dnorm, args = list(mean = x_mean, sd = x_sd), color = "red") +
      ggtitle(paste0(varname, "\nHistogram w/ Normal Overlay")) +
      labs(subtitle = sw_text) +
      theme_bw()
    
    p2 <- ggplot(data.frame(x = x), aes(sample = x)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      ggtitle(paste0(varname, "\nNormal Q-Q Plot")) +
      labs(subtitle = sw_text) +
      theme_bw()
    
    return(list(p1, p2))
  }
  
  make_bar_plot <- function(data, varname, type = "count") {
    if (type == "count") {
      ggplot(data, aes(x = .data[[varname]])) +
        geom_bar(fill = "skyblue", color = "black") +
        ggtitle(paste("Bar Plot:", varname)) +
        theme_bw()
    } else {
      plot_data <- data %>%
        group_by(.data[[varname]]) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(percent = 100 * n / sum(n))
      
      ymax <- max(plot_data$percent, na.rm = TRUE)
      
      ggplot(plot_data, aes(x = .data[[varname]], y = percent)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        geom_text(aes(label = paste0(round(percent, 1), "%")), 
                  vjust = -0.5, size = 5) +
        ggtitle(paste("Percent Bar Plot:", varname)) +
        theme_bw() +
        ylim(0, ymax * 1.1)
    }
  }
  
  # ---- Generate plots ----
  observeEvent(input$plot_btn, {
    output$combined_plot <- renderPlot({
      req(df())
      plots <- list()
      
      if (!is.null(input$numeric_vars)) {
        for (var in input$numeric_vars) {
          panels <- make_normality_panels(df(), var)
          plots <- c(plots, panels)
        }
      }
      if (!is.null(input$factor_vars)) {
        for (var in input$factor_vars) {
          bar <- make_bar_plot(df(), var, input$cat_plot_type)
          plots <- c(plots, list(bar))
        }
      }
      if (length(plots) == 0) return(NULL)
      grid.arrange(grobs = plots, ncol = 2)
    })
  })
  
  # ---- Download Word file ----
  output$download_docx <- downloadHandler(
    filename = function() {
      paste0(input$save_name, ".docx")
    },
    content = function(file) {
      req(df())
      
      plots <- list()
      if (!is.null(input$numeric_vars)) {
        for (var in input$numeric_vars) {
          panels <- make_normality_panels(df(), var)
          plots <- c(plots, panels)
        }
      }
      if (!is.null(input$factor_vars)) {
        for (var in input$factor_vars) {
          bar <- make_bar_plot(df(), var, input$cat_plot_type)
          plots <- c(plots, list(bar))
        }
      }
      
      doc <- officer::read_docx()
      for (i in seq_along(plots)) {
        doc <- doc %>%
          body_add_par(paste("Plot", i), style = "heading 2") %>%
          body_add_gg(plots[[i]], style = "centered")
      }
      
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)


