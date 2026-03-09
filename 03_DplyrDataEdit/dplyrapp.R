# ============================================================
# App2 Dplyr DataEdit (Fully functional + download/save + forcats panel)
# ============================================================
library(shiny)
library(readxl)
library(haven)
library(dplyr)
library(writexl)
library(DT)
library(forcats)
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),
    tags$link(rel = "stylesheet",
              href = "https://code.jquery.com/ui/1.13.2/themes/base/jquery-ui.css")
  ),
  
  titlePanel("Interactive Data Editor (No Code dplyr)"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Load Dataset"),
      fileInput("file", "Upload Excel, Stata, or RData",
                accept = c(".RData", ".rds", ".xlsx", ".xls", ".dta")),
      selectInput("env_dataset", "Or select dataset from R environment:", 
                  choices = character(0)),
      actionButton("load_env", "Load Selected Dataset"),
      hr(),
      
      h4("2. Data Operations"),
      selectInput("operation", "Choose Operation:",
                  c("Select Columns", "Rename Columns",
                    "Filter Rows", "Arrange Rows",
                    "Mutate (if_else)", "Mutate (case_when)",
                    "Mutate (expression)", "Recode Variable", "Order Columns")),
      uiOutput("operation_ui"),
      actionButton("apply_btn", "Apply", class = "btn-primary"),
      hr(),
      
      # ---- NEW: Forcats Panel ABOVE Save Data ----
      uiOutput("forcats_ui"),
      hr(),
      
      h4("3. Save Data"),
      textInput("save_name", "File name (without .RData):", "edited_data"),
      downloadButton("download_rdata", "Download RData"),
      downloadButton("download_dict", "Download Dictionary"),
      hr(),
      
      actionButton("reset_btn", "Reset to Original")
    ),   # <-- closes sidebarPanel
    
    mainPanel(
      h4("Editable Data Preview"),
      DTOutput("preview")
    )
  )      # <-- closes sidebarLayout
)


# ---- SERVER ----
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, edited = NULL)
  
  # Update env dataset choices
  observe({
    updateSelectInput(session, "env_dataset", choices = ls(envir = .GlobalEnv))
  })
  
  # ---- Load Data ----
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
                 "xlsx" = , "xls" = read_excel(input$file$datapath),
                 "dta" = read_dta(input$file$datapath),
                 "RData" = , "rds" = { e <- new.env(); load(input$file$datapath, envir = e); get(ls(e)[1], e) },
                 stop("Unsupported file type"))
    rv$data <- df
    rv$edited <- df
    showNotification("Data loaded successfully", type = "message")
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset)
    df <- get(input$env_dataset, envir = .GlobalEnv)
    if (!is.data.frame(df)) {
      showNotification("Not a data frame", type = "error")
    } else {
      rv$data <- df
      rv$edited <- df
      showNotification("Loaded from environment", type = "message")
    }
  })
  
  # ---- Dynamic Operation UI ----
  output$operation_ui <- renderUI({
    req(rv$edited)
    df <- rv$edited
    cols <- names(df)
    op <- input$operation
    
    switch(op,
           "Select Columns" = checkboxGroupInput("sel_cols", "Select columns:", choices = cols, selected = cols),
           "Rename Columns" = fluidRow(
             column(6, selectInput("rename_from", "Rename from:", choices = cols)),
             column(6, textInput("rename_to", "New name:"))
           ),
           "Filter Rows" = tagList(
             selectInput("filter_col", "Column:", choices = cols),
             selectInput("filter_op", "Operator:", choices = c("==", "!=", ">", ">=", "<", "<=")),
             textInput("filter_val", "Value:")
           ),
           "Arrange Rows" = selectInput("arrange_col", "Arrange by:", choices = cols),
           "Mutate (if_else)" = tagList(
             textInput("mutate_new", "New variable name:"),
             selectInput("mutate_col", "Base column:", choices = cols),
             textInput("mutate_cond", "Condition (e.g. > 65):"),
             textInput("mutate_true", "Value if TRUE:"),
             textInput("mutate_false", "Value if FALSE:")
           ),
           "Mutate (case_when)" = tagList(
             textInput("mutate_new2", "New variable name:"),
             textAreaInput("mutate_cases", "case_when logic (one per line):", rows = 5,
                           placeholder = "age > 65 ~ \"Elderly\"\nage <= 65 ~ \"Young\"")
           ),
           "Mutate (expression)" = tagList(
             textInput("mutate_expr_new", "New variable name:", placeholder = "invb2m"),
             textAreaInput("mutate_expr", "Expression (using existing variables):", 
                           rows = 4, 
                           placeholder = "1 / b2m\nlog(weight)\nage^2")
           ),
           "Recode Variable" = tagList(
             selectInput("recode_col", "Variable:", choices = cols),
             textAreaInput("recode_rules", "Rules (one per line):", rows = 5,
                           placeholder = "1 = \"Yes\"\n0 = \"No\"\nNA = \"Missing\"")
           ),
           "Order Columns" = tagList(
             tags$div(tags$ul(id = "col_list", class = "sortable-list",
                              lapply(cols, function(cn) tags$li(class = "col-item", `data-col` = cn, cn)))),
             tags$style(HTML(" .sortable-list { list-style: none; padding-left: 0; margin: 0 0 8px 0; }
                               .sortable-list li { padding: 8px 12px; margin-bottom: 6px;
                               background: #fff; border: 1px solid #ddd;
                               border-radius: 4px; cursor: move; }")),
             tags$script(HTML("
               $(function(){
                 var $list = $('#col_list');
                 $list.sortable({
                   update: function(){
                     var order = [];
                     $list.find('li').each(function(){ order.push($(this).attr('data-col')); });
                     Shiny.setInputValue('order_cols', order, {priority: 'event'});
                   }
                 });
               });
             "))
           )
    )
  })
  
  # ---- Apply Operation ----
  observeEvent(input$apply_btn, {
    req(rv$edited)
    df <- rv$edited
    op <- input$operation
    
    tryCatch({
      if (op == "Select Columns") df <- df %>% select(all_of(input$sel_cols))
      if (op == "Rename Columns" && nzchar(input$rename_to))
        df <- df %>% rename(!!input$rename_to := all_of(input$rename_from))
      if (op == "Filter Rows" && nzchar(input$filter_val)) {
        val <- input$filter_val
        col <- input$filter_col
        if (is.numeric(df[[col]])) val <- as.numeric(val)
        expr <- paste0(".data[['", col, "']] ", input$filter_op, " ", deparse(val))
        df <- df %>% filter(!!rlang::parse_expr(expr))
      }
      if (op == "Arrange Rows") df <- df %>% arrange(.data[[input$arrange_col]])
      
      if (op == "Mutate (if_else)" && nzchar(input$mutate_new)) {
        cond <- paste0(".data[['", input$mutate_col, "']]", input$mutate_cond)
        df <- df %>% mutate(!!input$mutate_new := if_else(!!rlang::parse_expr(cond),
                                                          input$mutate_true, input$mutate_false))
      }
      if (op == "Mutate (case_when)" && nzchar(input$mutate_cases)) {
        expr <- paste0("case_when(", paste(input$mutate_cases, collapse = ", "), ")")
        df <- df %>% mutate(!!input$mutate_new2 := !!rlang::parse_expr(expr))
      }
      if (op == "Mutate (expression)" && nzchar(input$mutate_expr_new) && nzchar(input$mutate_expr)) {
        expr <- try(rlang::parse_expr(input$mutate_expr), silent = TRUE)
        if (!inherits(expr, "try-error")) {
          df <- df %>% mutate(!!input$mutate_expr_new := !!expr)
        }
      }
      if (op == "Recode Variable" && nzchar(input$recode_rules)) {
        # Split rules entered by user
        rules <- strsplit(trimws(input$recode_rules), "\n")[[1]]
        recode_list <- list()
        na_values <- character(0)
        
        for (r in rules) {
          r <- trimws(r)
          if (nchar(r) == 0 || !grepl("=", r)) next
          
          parts <- strsplit(r, "=", fixed = TRUE)[[1]]
          from <- gsub('[\'",]', '', trimws(parts[1]))  # sanitize input
          to   <- gsub('[\'",]', '', trimws(parts[2]))  # sanitize input
          
          if (toupper(to) %in% c("NA","<NA>","MISSING","")) {
            na_values <- c(na_values, from)
          } else {
            recode_list[[from]] <- to
          }
        }
        
        col <- input$recode_col
        df <- df %>% mutate(
          !!col := {
            x <- .data[[col]]
            
            if (length(recode_list) > 0) {
              if (is.factor(x)) {
                # fct_recode expects new = old, so flip mapping
                x <- forcats::fct_recode(x, !!!setNames(unname(recode_list), names(recode_list)))
              } else {
                x <- dplyr::recode(as.character(x), !!!recode_list, .default = x)
              }
            }
            
            if (length(na_values) > 0) {
              na_vals_parsed <- suppressWarnings(as.numeric(na_values))
              if (all(!is.na(na_vals_parsed))) {
                x <- na_if(x, na_vals_parsed)
              } else {
                x <- na_if(as.character(x), na_values)
              }
            }
            
            x
          }
        )
      }
      
      if (op == "Order Columns" && !is.null(input$order_cols)) {
        df <- df %>% select(all_of(input$order_cols))
      }
      rv$edited <- df
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
  })
  
  # ---- Editable DT ----
  output$preview <- renderDT({
    req(rv$edited)
    datatable(rv$edited, editable = list(target="cell"), options=list(scrollX=TRUE, pageLength=10))
  })
  
  # ---- FORCATS PANEL ----
  output$forcats_ui <- renderUI({
    req(rv$edited)
    tagList(
      hr(),
      h4("Create forcats-based factors"),
      
      # 1. fct_infreq
      selectInput("freq_var", "Variable → fct_infreq (ordered by frequency)", 
                  choices = names(rv$edited)),
      actionButton("make_infreq", "Create fct_infreq", class = "btn-primary", width = "100%"),
      
      hr(),
      
      # 2. NA → Missing
      selectInput("na_var", "Variable → NA → Missing", 
                  choices = names(rv$edited)),
      actionButton("make_na", "Create NA → Missing", class = "btn-primary", width = "100%"),
      
      hr(),
      
      # 3. Lump Top N
      selectInput("lump_var", "Variable → Lump top N", 
                  choices = names(rv$edited)),
      numericInput("lump_n", "Keep top N", value = 5, min = 1),
      textInput("lump_other", "Other level name", value = "Other"),
      actionButton("make_lump", "Create Lump Top N", class = "btn-primary", width = "100%"),
      
      hr(),
      
      # 4. Drop Unused Levels
      selectInput("drop_var", "Variable → Drop unused levels", 
                  choices = names(rv$edited)),
      actionButton("make_drop", "Drop Unused Levels", class = "btn-warning", width = "100%")
    )
  })
  
  # ---- FORCATS ACTIONS ----
  observeEvent(input$make_infreq, {
    req(rv$edited, input$freq_var)
    df <- rv$edited
    new_name <- paste0(input$freq_var, "_infreq")
    if (new_name %in% names(df)) {
      showNotification("Column already exists.", type = "error"); return()
    }
    df[[new_name]] <- forcats::fct_infreq(df[[input$freq_var]])
    attr(df[[new_name]], "label") <- paste("Frequency-ordered:", input$freq_var)
    rv$edited <- df
    showNotification(paste("Created:", new_name), type = "message")
  })
  
  observeEvent(input$make_na, {
    req(rv$edited, input$na_var)
    df <- rv$edited
    new_name <- paste0(input$na_var, "_na")
    if (new_name %in% names(df)) {
      showNotification("Column already exists.", type = "error"); return()
    }
    df[[new_name]] <- forcats::fct_explicit_na(df[[input$na_var]], na_level = "Missing")
    attr(df[[new_name]], "label") <- paste("NA → Missing:", input$na_var)
    rv$edited <- df
    showNotification(paste("Created:", new_name), type = "message")
  })
  
  observeEvent(input$make_lump, {
    req(rv$edited, input$lump_var, input$lump_n)
    df <- rv$edited
    new_name <- paste0(input$lump_var, "_top", input$lump_n)
    if (new_name %in% names(df)) {
      showNotification("Column already exists.", type = "error"); return()
    }
    df[[new_name]] <- forcats::fct_lump_n(df[[input$lump_var]], n = input$lump_n, other_level = input$lump_other)
    attr(df[[new_name]], "label") <- paste0("Top ", input$lump_n, " + ", input$lump_other, ": ", input$lump_var)
    rv$edited <- df
    showNotification(paste("Created:", new_name), type = "message")
  })
  
  observeEvent(input$make_drop, {
    req(rv$edited, input$drop_var)
    df <- rv$edited
    old_levels <- levels(df[[input$drop_var]])
    df[[input$drop_var]] <- forcats::fct_drop(df[[input$drop_var]])
    new_levels <- levels(df[[input$drop_var]])
    
    dropped <- setdiff(old_levels, new_levels)
    if (length(dropped) == 0) {
      showNotification("No unused levels to drop.", type = "message")
    } else {
      showNotification(paste("Dropped:", paste(dropped, collapse = ", ")), type = "message")
    }
    rv$edited <- df
  })
  
  # ---- Fixed cell edit ----
  observeEvent(input$preview_cell_edit, {
    info <- input$preview_cell_edit
    req(rv$edited, info$row, info$col)
    col_name <- names(rv$edited)[info$col]
    val <- info$value
    if (is.null(val) || val == "" || tolower(trimws(val)) %in% c("na","<na>")) val <- NA
    rv$edited[info$row, col_name] <- if (is.factor(rv$edited[[col_name]])) {
      if (!val %in% levels(rv$edited[[col_name]])) rv$edited[[col_name]] <- fct_expand(rv$edited[[col_name]], val)
      factor(val, levels = levels(rv$edited[[col_name]]))
    } else if (is.numeric(rv$edited[[col_name]])) suppressWarnings(as.numeric(val))
    else as.character(val)
  })
  
  # ---- Reset ----
  observeEvent(input$reset_btn, { rv$edited <- rv$data; showNotification("Reset to original data") })
  
  # ---- Download Handlers ----
  output$download_rdata <- downloadHandler(
    filename = function() paste0(input$save_name, ".RData"),
    content = function(file) { df <- rv$edited; save(df, file = file) }
  )
  
  output$download_dict <- downloadHandler(
    filename = function() paste0(input$save_name, "_dictionary.xlsx"),
    content = function(file) {
      df <- rv$edited
      dict <- data.frame(
        Variable = names(df),
        Class = sapply(df, function(x) paste(class(x), collapse=", ")),
        Label = sapply(df, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else paste(l, collapse="; ") }),
        Levels = sapply(df, function(x) if (is.factor(x)) paste(levels(x), collapse="; ") else ""),
        stringsAsFactors = FALSE
      )
      writexl::write_xlsx(dict, file)
    }
  )
}

# ---- Launch ----
shinyApp(ui, server)

