# Load libraries ----
library(shiny)
library(readxl)
library(dplyr)
library(gtsummary)
library(gt)
library(flextable)
library(summarytools)
library(haven)

# UI ----
ui <- fluidPage(
  titlePanel("Descriptive Statistics Summary"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Load Dataset"),
      fileInput("file", "Upload Excel / Stata / RData / RDS / CSV",
                accept = c(".csv", ".xlsx", ".xls", ".RData", ".rds", ".dta")),
      selectInput("env_dataset", "Or select dataset from R environment:", choices = ls(envir = .GlobalEnv)),
      actionButton("load_env", "Load Selected Dataset"),
      hr(),
      
      h4("Data Subset Options"),
      selectInput("subset_mode", "Subset Mode:", choices = c("Use all data", "Select columns", "Filter rows")),
      uiOutput("subset_ui"),
      actionButton("apply_subset", "Apply Subset", class = "btn-primary"),
      hr(),
      
      h4("Summary Table Options"),
      checkboxInput("include_cat", "Include Categorical Variables", value = TRUE),
      selectInput("group_var", "Group By (optional):", choices = NULL),
      actionButton("view_summary", "View dfSummary"),
      hr(),
      
      h4("Save Summary Table"),
      textInput("save_name", "File name (no extension):", "summary_table"),
      actionButton("save_btn", "💾 Save Word File", class = "btn-success"),
      textOutput("save_msg")
    ),
    
    mainPanel(
      h3("Summary Table"),
      uiOutput("grouping_info"),
      div(style = "width:100%; overflow-x:auto;", gt_output("summary_table")),
      verbatimTextOutput("error_log")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  error_msg <- reactiveVal(NULL)
  
  # Refresh environment dataset list
  observe({
    updateSelectInput(session, "env_dataset", choices = ls(envir = .GlobalEnv))
  })
  
  # Load dataset from file ----
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    df <- tryCatch({
      switch(ext,
             csv  = read.csv(input$file$datapath, stringsAsFactors = FALSE),
             xlsx = read_excel(input$file$datapath),
             xls  = read_excel(input$file$datapath),
             rds  = readRDS(input$file$datapath),
             RData = { e <- new.env(); load(input$file$datapath, envir = e); get(ls(e)[1], envir = e) },
             dta = read_dta(input$file$datapath),
             stop("Unsupported file type")
      )
    }, error = function(e) { showNotification(e$message, type="error"); return(NULL) })
    
    if (!is.null(df)) {
      dataset(df)
      filtered_data(df)
    }
  })
  
  # Load dataset from R environment ----
  observeEvent(input$load_env, {
    req(input$env_dataset)
    obj <- get(input$env_dataset, envir = .GlobalEnv)
    
    if (!is.data.frame(obj)) {
      showNotification("Selected object is not a data frame", type="error")
    } else {
      dataset(obj)
      filtered_data(obj)
      showNotification(paste("Loaded dataset:", input$env_dataset), type="message")
    }
  })
  
  # Update Group By choices ----
  observe({
    req(dataset())
    updateSelectInput(session, "group_var", choices = c("None", names(dataset())))
  })
  
  # Subset UI ----
  output$subset_ui <- renderUI({
    req(dataset())
    df <- dataset()
    
    switch(input$subset_mode,
           "Select columns" = checkboxGroupInput("sel_cols", "Select columns to keep:", choices = names(df)),
           "Filter rows" = textAreaInput("filter_expr", "Enter filter expression:", rows = 3,
                                         placeholder = 'e.g. age > 50 & gender == "M"'),
           NULL)
  })
  
  # Apply subset ----
  observeEvent(input$apply_subset, {
    req(dataset())
    df <- dataset()
    
    new_df <- tryCatch({
      if (input$subset_mode == "Select columns" && !is.null(input$sel_cols)) {
        df %>% select(all_of(input$sel_cols))
      } else if (input$subset_mode == "Filter rows" && nzchar(input$filter_expr)) {
        df %>% filter(eval(parse(text = input$filter_expr)))
      } else {
        df
      }
    }, error = function(e) {
      showNotification(paste("Subset error:", e$message), type = "error")
      df
    })
    
    filtered_data(new_df)
  })
  
  # Grouping info ----
  output$grouping_info <- renderUI({
    req(filtered_data())
    if (input$group_var != "None") {
      div(style = "color:blue; font-weight:bold;", paste("📊 Grouping by:", input$group_var))
    } else {
      div(style = "color:gray;", "📊 No grouping applied")
    }
  })
  
  # Summary Table ----
  summaryTable <- reactive({
    df <- filtered_data()
    req(df)
    
    tryCatch({
      df <- df %>% mutate(across(where(haven::is.labelled), haven::as_factor))
      
      cont_vars <- names(df)[sapply(df, is.numeric)]
      cat_vars  <- names(df)[!sapply(df, is.numeric)]
      
      type_list <- list()
      if (length(cont_vars) > 0) {
        type_list <- append(type_list, setNames(rep(list("continuous"), length(cont_vars)), cont_vars))
      }
      if (length(cat_vars) > 0 && input$include_cat) {
        type_list <- append(type_list, setNames(rep(list("categorical"), length(cat_vars)), cat_vars))
      }
      
      by_var <- input$group_var
      valid_by <- !is.null(by_var) && by_var != "None" && by_var %in% names(df)
      
      if (valid_by) {
        tbl <- tbl_summary(
          data = df,
          by = by_var,
          type = type_list,
          statistic = list(
            all_continuous() ~ "{mean} ({sd}) [{min}, {max}] {median} [{p25}, {p75}]",
            all_categorical() ~ "{n} ({p}%)"
          ),
          missing = "ifany",
          digits = all_continuous() ~ 2
        )
      } else {
        tbl <- tbl_summary(
          data = df,
          type = type_list,
          statistic = list(
            all_continuous() ~ "{mean} ({sd}) [{min}, {max}] {median} [{p25}, {p75}]",
            all_categorical() ~ "{n} ({p}%)"
          ),
          missing = "ifany",
          digits = all_continuous() ~ 2
        )
      }
      
      tbl
    }, error = function(e) {
      error_msg(e$message)
      NULL
    })
  })
  
  output$summary_table <- render_gt({
    req(summaryTable())
    summaryTable() %>%
      as_gt() %>%
      tab_header(title = "Descriptive Summary") %>%
      tab_options(table.width = pct(100))
  })
  
  # dfSummary ----
  observeEvent(input$view_summary, {
    req(filtered_data())
    df <- filtered_data()
    st_options(bootstrap.css = TRUE, plain.ascii = FALSE)
    
    temp_html <- capture.output(
      print(dfSummary(df, varnumbers=TRUE, valid.col=TRUE, graph.col=TRUE, labels.col=TRUE, style="grid"),
            method="render", footnote=NA)
    )
    
    showModal(modalDialog(
      title = "Data Summary (dfSummary)",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML(paste0('<div style="overflow-x:auto; width:1200px;">', paste(temp_html, collapse="\n"), '</div>'))
    ))
  })
  
  output$error_log <- renderText({ error_msg() })
  
  # ---- Save Word file directly to Results ----
  observeEvent(input$save_btn, {
    req(summaryTable())
    
    results_dir <- "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/Results"
    save_file <- file.path(results_dir, paste0(input$save_name, ".docx"))
    
    tryCatch({
      summaryTable() %>%
        as_flex_table() %>%
        flextable::save_as_docx(path = save_file)
      
      output$save_msg <- renderText(paste("✅ File saved to:", save_file))
    },
    error = function(e) {
      output$save_msg <- renderText(paste("❌ Error saving file:", e$message))
    })
  })
}

# Run App ----
shinyApp(ui, server)
