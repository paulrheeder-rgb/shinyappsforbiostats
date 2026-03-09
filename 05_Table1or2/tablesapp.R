# =========================================================
# App4: Table1/Table2 Descriptive Statistics with Download & dfSummary Browser
# =========================================================
library(shiny)
library(shinyFiles)
library(readxl)
library(dplyr)
library(gtsummary)
library(gt)
library(flextable)
library(summarytools)
library(haven)
library(conflicted)
library(sortable)   # for drag-and-drop ordering

conflicts_prefer(dplyr::filter)
conflicts_prefer(shiny::validate)

# --- Function to summarize missing values ---
make_missing_table <- function(df) {
  data.frame(
    Variable = names(df),
    NA_Count = sapply(df, function(x) sum(is.na(x))),
    NA_Percent = sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 2))
  )
}

ui <- fluidPage(
  titlePanel("Table 1: Descriptive Statistics with Group Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Load Dataset"),
      fileInput("file", "Upload Excel/Stata/RData/CSV", 
                accept = c(".RData", ".rds", ".xlsx", ".xls", ".dta", ".csv")),
      
      selectInput("env_dataset", "Or select dataset from R environment:", 
                  choices = ls(envir = .GlobalEnv)),
      actionButton("load_env", "Load Selected Dataset"),
      hr(),
      
      uiOutput("group_var"),
      uiOutput("parametric_vars"),
      uiOutput("nonparametric_vars"),
      uiOutput("categorical_vars"),
      uiOutput("reorder_vars"),
      
      actionButton("view_summary", "View Data Summary (dfSummary)"),
      actionButton("view_missing", "View Missing Data"),
      hr(),
      
      h4("💾 Save Table"),
      textInput("save_name", "File name (no extension):", "table1_summary"),
      downloadButton("download_summary", "Download Word File", class = "btn-success")
    ),
    
    mainPanel(
      h3("Descriptive Statistics Table"),
      div(style = "width:100%; overflow-x:auto;",
          gt_output("summary_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactiveVal(NULL)
  
  # ---- Load dataset ----
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
                 csv  = read.csv(input$file$datapath, stringsAsFactors = FALSE),
                 xlsx = read_excel(input$file$datapath),
                 xls  = read_excel(input$file$datapath),
                 RData = { e <- new.env(); load(input$file$datapath, envir = e);
                 get(ls(e)[1], envir = e) },
                 rds  = readRDS(input$file$datapath),
                 validate("Unsupported file type"))
    
  #  df <- df %>%
  #                  ~ iconv(.x, from = "", to = "UTF-8", sub = "byte"))) %>%
  # mutate(across(where(is.factor),
  #   ~ factor(iconv(as.character(.x),
  #              from = "", to = "UTF-8", sub = "byte"))))
    dataset(df)
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset)
    obj <- get(input$env_dataset, envir = .GlobalEnv)
    if (!is.data.frame(obj)) {
      showNotification("Selected object is not a data frame", type = "error")
    } else {
      dataset(obj)
      showNotification(paste("Loaded dataset from R environment:", input$env_dataset),
                       type = "message")
    }
  })
  
  # ---- Dynamic selectors ----
  output$group_var <- renderUI({
    req(dataset())
    selectInput("group", "Grouping variable",
                choices = c("None", names(dataset())), selected = "None")
  })
  output$parametric_vars <- renderUI({
    req(dataset())
    selectInput("vars_param", "Parametric (mean ± SD)",
                choices = names(dataset()), multiple = TRUE)
  })
  output$nonparametric_vars <- renderUI({
    req(dataset())
    selectInput("vars_nonparam", "Non-parametric (median [p25, p75])",
                choices = names(dataset()), multiple = TRUE)
  })
  output$categorical_vars <- renderUI({
    req(dataset())
    selectInput("vars_cat", "Categorical (N, %)",
                choices = names(dataset()), multiple = TRUE)
  })
  
  output$reorder_vars <- renderUI({
    req(dataset())
    # Collect selected variables
    selected_vars <- c(input$vars_param, input$vars_nonparam, input$vars_cat)
    if (length(selected_vars) == 0) return(NULL)
    
    rank_list(
      text = "Drag to reorder variables:",
      labels = selected_vars,
      input_id = "ordered_vars"
    )
  })
  
  
  # ---- Adaptive Chi-square or Fisher test ----
  chisq_or_fisher <- function(data, variable, by, ...) {
    tbl <- table(data[[variable]], data[[by]])
    if (any(dim(tbl) < 2)) return(tibble::tibble(variable = variable, p.value = NA_real_, method = "Insufficient data"))
    test_chi <- suppressWarnings(chisq.test(tbl))
    expected <- test_chi$expected
    if (any(expected < 5)) {
      test_result <- fisher.test(tbl, workspace = 2e7)
      method <- "Fisher's exact test"
    } else {
      test_result <- test_chi
      method <- "Chi-squared test"
    }
    tibble::tibble(variable = variable, p.value = test_result$p.value, method = method)
  }
  
  # ---- Summary Table ----
  # ---- Summary Table ----
  summaryTable <- reactive({
    req(dataset())
    df <- dataset()
    
    # collect selected variables
    selected_vars <- input$ordered_vars
    
    validate(need(length(selected_vars) > 0, "Please select at least one variable."))
    
    # determine grouping variable
    by_var <- if (input$group != "None") input$group else NULL
    
    # subset while preserving labels
    df_use <- if (!is.null(by_var)) {
      df %>% select(all_of(c(selected_vars, by_var)))
    } else {
      df %>% select(all_of(selected_vars))
    }
    
    # reattach labels from original df
    for (v in names(df_use)) {
      attr(df_use[[v]], "label") <- attr(df[[v]], "label")
    }
    
    # build statistic and type lists
    stat_list <- list()
    type_list <- list()
    for (v in input$vars_param) {
      stat_list[[v]] <- "{mean} ({sd})"
      type_list[[v]] <- "continuous"
    }
    for (v in input$vars_nonparam) {
      stat_list[[v]] <- "{median} [{p25}, {p75}] [{min}, {max}]"
      type_list[[v]] <- "continuous"
    }
    for (v in input$vars_cat) {
      stat_list[[v]] <- "{n} ({p}%)"
      type_list[[v]] <- "categorical"
    }
    
    # build gtsummary table
    tbl <- tbl_summary(
      data = df_use,
      by = by_var,
      statistic = stat_list,
      digits = all_continuous() ~ 1,
      missing = "ifany",
      type = type_list
    ) %>%
      modify_header(label = "**Variable**") %>%
      modify_caption(if (is.null(by_var)) 
        "**Descriptive statistics (overall)**" 
        else 
          "**Descriptive statistics by group**") %>%
      bold_labels()
    
    # add p-values if grouping variable is present
    if (!is.null(by_var)) {
      n_groups <- df %>%
        filter(!is.na(.data[[by_var]])) %>%
        pull(.data[[by_var]]) %>%
        unique() %>%
        length()
      
      test_list <- list()
      for (v in input$vars_param)    test_list[[v]] <- ifelse(n_groups > 2, "oneway.test", "t.test")
      for (v in input$vars_nonparam) test_list[[v]] <- ifelse(n_groups > 2, "kruskal.test", "wilcox.test")
      for (v in input$vars_cat)      test_list[[v]] <- "chisq_or_fisher"
      
      if (length(test_list) > 0) tbl <- tbl %>% add_p(test = test_list)
    }
    
    tbl
  })
  
  output$summary_table <- render_gt({
    req(summaryTable())
    summaryTable() %>% as_gt()
  })
  
  # ---- dfSummary Modal ----
  observeEvent(input$view_summary, {
    req(dataset())
    df <- dataset()
    st_options(bootstrap.css = TRUE, plain.ascii = FALSE)
    temp_html <- capture.output(
      print(dfSummary(df, varnumbers = TRUE, valid.col = TRUE,
                      graph.col = TRUE, labels.col = TRUE, style = "grid"),
            method = "browser", footnote = NA)
    )
    showModal(modalDialog(
      title = "Data Summary (dfSummary)",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML(paste0('<div style="overflow-x:auto; width:1200px; max-width:95vw;">',
                  paste(temp_html, collapse = "\n"), '</div>'))
    ))
  })
  
  # ---- Missing Data Modal ----
  observeEvent(input$view_missing, {
    req(dataset())
    df <- dataset()
    missing_tbl <- make_missing_table(df)
    
    showModal(modalDialog(
      title = "Missing Data Summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      renderTable(missing_tbl, striped = TRUE, bordered = TRUE, spacing = 's'),
      tags$head(tags$style(HTML("
        .modal-body { overflow-x: auto; max-height: 80vh; }
      ")))
    ))
  })
  
  # ---- Download Word File ----
  output$download_summary <- downloadHandler(
    filename = function() { paste0(input$save_name, ".docx") },
    content = function(file) {
      req(summaryTable())
      summaryTable() %>%
        as_flex_table() %>%
        flextable::save_as_docx(path = file)
    }
  )
}

# ---- Launch App ----
shinyApp(ui, server)



