# App4 Table1 or 2
# Load libraries
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
conflicts_prefer(dplyr::filter)

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
      fileInput("file", "Upload Excel or Stata or RData", 
                accept = c(".RData", ".rds", ".xlsx", ".xls", ".dta", ".csv")),
      
      selectInput("env_dataset", "Or select dataset from R environment:", 
                  choices = ls(envir = .GlobalEnv)),
      actionButton("load_env", "Load Selected Dataset"),
      hr(),
      
      uiOutput("group_var"),
      uiOutput("parametric_vars"),
      uiOutput("nonparametric_vars"),
      uiOutput("categorical_vars"),
      
      actionButton("view_summary", "View Data Summary (dfSummary)"),
      actionButton("view_missing", "View Missing Data"),
      hr(),
      
      h4("💾 Save Table"),
      textInput("save_name", "File name (no extension):", "table1_summary"),
      actionButton("save_btn", "Save Word File", class = "btn-success"),
      textOutput("save_msg")
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
    
    df <- df %>%
      mutate(across(where(is.character),
                    ~ iconv(.x, from = "", to = "UTF-8", sub = "byte"))) %>%
      mutate(across(where(is.factor),
                    ~ factor(iconv(as.character(.x),
                                   from = "", to = "UTF-8", sub = "byte"))))
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
  
  # ---- Custom helper: switch between Chi-square and Fisher ----
  # ---- Custom helper: adaptive Chi-square or Fisher test ----
  chisq_or_fisher <- function(data, variable, by, ...) {
    tbl <- table(data[[variable]], data[[by]])
    
    # Handle missing or degenerate tables gracefully
    if (any(dim(tbl) < 2)) {
      return(tibble::tibble(
        variable = variable,
        p.value = NA_real_,
        method = "Insufficient data"
      ))
    }
    
    # Compute expected counts safely
    test_chi <- suppressWarnings(chisq.test(tbl))
    expected <- test_chi$expected
    
    if (any(expected < 5)) {
      test_result <- fisher.test(tbl,workspace = 2e7)
      method <- "Fisher's exact test"
    } else {
      test_result <- test_chi
      method <- "Chi-squared test"
    }
    
    # Return as tibble for gtsummary compatibility
    tibble::tibble(
      variable = variable,
      p.value = test_result$p.value,
      method = method
    )
  }
  
  
  # ---- Summary Table ----
  summaryTable <- reactive({
    req(dataset())
    df <- dataset()
    
    # --- Get selected variables ---
    selected_vars <- unique(c(input$vars_param, input$vars_nonparam, input$vars_cat))
    validate(need(length(selected_vars) > 0, "Please select at least one variable."))
    
    numeric_vars <- c(input$vars_param, input$vars_nonparam)
    
    # --- Clean numeric variables ---
    if (length(numeric_vars) > 0) {
      df[numeric_vars] <- lapply(df[numeric_vars], function(x) {
        suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
      })
    }
    
    # --- Force categorical variables to retain ALL levels ---
    if (length(input$vars_cat) > 0) {
      df[input$vars_cat] <- lapply(df[input$vars_cat], function(x) {
        x <- as.factor(x)
        x <- factor(x, levels = unique(c(levels(x), as.character(unique(x)))))
        x
      })
    }
    
    # --- Prepare statistics list ---
    stat_list <- list()
    if (length(input$vars_param) > 0)
      stat_list <- c(stat_list, setNames(rep("{mean} ({sd})", length(input$vars_param)),
                                         input$vars_param))
    if (length(input$vars_nonparam) > 0)
      stat_list <- c(stat_list, setNames(rep("{median} [{p25}, {p75}]",
                                             length(input$vars_nonparam)),
                                         input$vars_nonparam))
    if (length(input$vars_cat) > 0)
      stat_list <- c(stat_list, setNames(rep("{n} ({p}%)", length(input$vars_cat)),
                                         input$vars_cat))
    
    # --- Determine grouping variable (if any) ---
    by_var <- if (input$group != "None") input$group else NULL
    
    # --- Create table summary ---
    # ---- Determine variable types explicitly ----
    type_list <- list()
    
    if (length(c(input$vars_param, input$vars_nonparam)) > 0) {
      type_list <- c(
        type_list,
        setNames(rep("continuous", length(c(input$vars_param, input$vars_nonparam))),
                 c(input$vars_param, input$vars_nonparam))
      )
    }
    
    if (length(input$vars_cat) > 0) {
      type_list <- c(
        type_list,
        setNames(rep("categorical", length(input$vars_cat)),
                 input$vars_cat)
      )
    }
    
    # --- Determine data to include safely ---
    if (!is.null(by_var)) {
      df_use <- df[, unique(c(selected_vars, by_var)), drop = FALSE]
    } else {
      df_use <- df[, selected_vars, drop = FALSE]
    }
    
    # --- Build the gtsummary table ---
    tbl <- tbl_summary(
      data = df_use,
      by = by_var,
      statistic = stat_list,
      digits = all_continuous() ~ 1, 
      missing = "ifany",
      type = type_list,
      label = list()
    ) %>%
      modify_header(label = "**Variable**") %>%
      modify_caption(if (is.null(by_var))
        "**Descriptive statistics (overall)**"
        else
          "**Descriptive statistics by group**") %>%
      bold_labels()
    
    # --- Add p-values if grouping variable present ---
    if (!is.null(by_var)) {
      n_groups <- df %>%
        filter(!is.na(.data[[by_var]])) %>%
        pull(.data[[by_var]]) %>%
        unique() %>%
        length()
      
      test_list <- list()
      if (length(input$vars_param) > 0)
        test_list <- c(test_list,
                       setNames(rep(ifelse(n_groups > 2, "oneway.test", "t.test"),
                                    length(input$vars_param)), input$vars_param))
      if (length(input$vars_nonparam) > 0)
        test_list <- c(test_list,
                       setNames(rep(ifelse(n_groups > 2, "kruskal.test", "wilcox.test"),
                                    length(input$vars_nonparam)), input$vars_nonparam))
      if (length(input$vars_cat) > 0)
        test_list <- c(test_list,
                       setNames(rep("chisq_or_fisher", length(input$vars_cat)),
                                input$vars_cat))
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
            method = "render", footnote = NA)
    )
    showModal(modalDialog(
      title = "Data Summary (dfSummary)",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML(paste0('<div style="overflow-x:auto; width:1200px; max-width:95vw;">',
                  paste(temp_html, collapse = "\n"), '</div>')),
      tags$head(tags$style(HTML("
        .modal-xl { max-width: 95vw !important; width: auto !important; }
        .modal-body { overflow-x: auto; }
        table { table-layout: auto !important; width: 100% !important; }
      ")))
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
  
  # ---- Folder-Based Save Option ----
  # ---- Auto Save to Results folder ----
  observeEvent(input$save_btn, {
    req(summaryTable())
    
    # ✅ Define fixed save folder
    save_dir <- "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/Results"
    if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    
    # ✅ Save Word file
    save_path <- file.path(save_dir, paste0(input$save_name, ".docx"))
    summaryTable() %>%
      as_flex_table() %>%
      flextable::save_as_docx(path = save_path)
    
    # ✅ Confirmation
    output$save_msg <- renderText(paste("✅ File saved successfully at:\n", save_path))
    showNotification("✅ Table saved to Results folder", type = "message")
  })
}
shinyApp(ui, server)



