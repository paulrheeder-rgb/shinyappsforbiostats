# ============================================================
# App2 Dplyr DataEdit (Fixed Save Folder + FULL CELL EDIT SUPPORT)
# ============================================================
library(shiny)
library(readxl)
library(haven)
library(dplyr)
library(writexl)
library(DT)
library(forcats)  # ← ADDED
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
                    "Mutate (expression)",     # ← NEW
                    "Recode Variable", "Order Columns")),
      uiOutput("operation_ui"),
      actionButton("apply_btn", "Apply", class = "btn-primary"),
      hr(),
      
      h4("3. Table Display"),
      numericInput("n_rows", "Rows per page:", value = 50, min = 10, max = 1000, step = 50),
      hr(),
      
      h4("4. Save Data"),
      textInput("save_name", "File name (without .RData):", "edited_data"),
      actionButton("save_btn", "Save Files", class = "btn-success"),
      textOutput("save_msg"),
      hr(),
      actionButton("reset_btn", "Reset to Original")
    ),
    
    mainPanel(
      h4("Editable Data Preview"),
      DTOutput("preview")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, edited = NULL)
  save_folder <- "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/data"
  
  # Update env dataset choices
  observe({
    updateSelectInput(session, "env_dataset", choices = ls(envir = .GlobalEnv))
  })
  
  # ---- 1. Load Data ----
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
                 "xlsx" = , "xls" = read_excel(input$file$datapath),
                 "dta" = read_dta(input$file$datapath),
                 "RData" = , "rds" = {
                   e <- new.env(); load(input$file$datapath, envir = e); get(ls(e)[1], e)
                 },
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
  
  # ---- 2. Dynamic Operation UI ----
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
                           placeholder = "1 / b2m\nlog(weight)\nage^2\nround(egfr / 10) * 10")
           ),
           "Recode Variable" = tagList(
             selectInput("recode_col", "Variable:", choices = cols),
             textAreaInput("recode_rules", "Rules (one per line):", rows = 5,
                           placeholder = "1 = \"Yes\"\n0 = \"No\"\nNA = \"Missing\"")
           ),
           "Order Columns" = tagList(
             tags$div(tags$ul(id = "col_list", class = "sortable-list",
                              lapply(cols, function(cn) tags$li(class = "col-item", `data-col` = cn, cn)))),
             tags$style(HTML("
               .sortable-list { list-style: none; padding-left: 0; margin: 0 0 8px 0; }
               .sortable-list li { padding: 8px 12px; margin-bottom: 6px;
                                   background: #fff; border: 1px solid #ddd;
                                   border-radius: 4px; cursor: move; }
             ")),
             tags$script(HTML("
               $(function(){
                 var $list = $('#col_list');
                 $list.sortable({
                   update: function(){
                     var order = [];
                     $list.find('li').each(function(){
                       order.push($(this).attr('data-col'));
                     });
                     Shiny.setInputValue('order_cols', order, {priority: 'event'});
                   }
                 });
               });
             "))
           )
    )
  })
  
  # ---- 3. Apply Operation ----
  observeEvent(input$apply_btn, {
    req(rv$edited)
    df <- rv$edited
    op <- input$operation
    
    tryCatch({
      if (op == "Select Columns") df <- df %>% select(all_of(input$sel_cols))
      if (op == "Rename Columns" && nzchar(input$rename_to))
        df <- df %>% rename(!!input$rename_to := all_of(input$rename_from))
      if (op == "Filter Rows" && nzchar(input$filter_val)) {
        col <- input$filter_col
        op_sym <- input$filter_op
        val <- input$filter_val
        if (is.numeric(df[[col]])) val <- as.numeric(val)
        expr <- paste0(".data[[\"", col, "\"]] ", op_sym, " ", deparse(val))
        df <- df %>% filter(!!rlang::parse_expr(expr))
      }
      if (op == "Arrange Rows") df <- df %>% arrange(.data[[input$arrange_col]])
      if (op == "Mutate (if_else)" && nzchar(input$mutate_new)) {
        cond <- paste0(".data[[\"", input$mutate_col, "\"]]", input$mutate_cond)
        df <- df %>% mutate(!!input$mutate_new := if_else(!!rlang::parse_expr(cond),
                                                          input$mutate_true, input$mutate_false))
      }
      if (op == "Mutate (case_when)" && nzchar(input$mutate_cases)) {
        expr <- paste0("case_when(", paste(input$mutate_cases, collapse = ", "), ")")
        df <- df %>% mutate(!!input$mutate_new2 := !!rlang::parse_expr(expr))
      }
      if (op == "Recode Variable" && nzchar(input$recode_rules)) {
        rules <- strsplit(trimws(input$recode_rules), "\n")[[1]]
        recode_pairs <- list()
        na_values <- character(0)  # Collect values to turn into NA
        
        for (r in rules) {
          r <- trimws(r)
          if (nchar(r) == 0 || !grepl("=", r, fixed = TRUE)) next
          
          parts <- strsplit(r, "=", fixed = TRUE)[[1]]
          from_raw <- trimws(parts[1])
          to_raw <- trimws(parts[2])
          
          # Clean from value: remove quotes if present
          from <- gsub('^"(.*)"$|^\'(.*)\'$', '\\1', from_raw)
          
          # Clean to value
          to_clean <- gsub('^"(.*)"$|^\'(.*)\'$', '\\1', to_raw)
          
          # Detect if user wants to map to missing
          if (toupper(to_clean) %in% c("NA", "<NA>", "MISSING", "")) {
            na_values <- c(na_values, from)
          } else {
            # Try to convert to appropriate type
            if (is.numeric(df[[input$recode_col]])) {
              to_num <- suppressWarnings(as.numeric(to_clean))
              if (!is.na(to_num)) {
                recode_pairs[[from]] <- to_num
              } else {
                recode_pairs[[from]] <- to_clean  # fallback to character
              }
            } else {
              recode_pairs[[from]] <- to_clean
            }
          }
        }
        
        col <- input$recode_col
        df <- df %>% mutate(
          !!col := {
            x <- .data[[col]]
            # First, apply any true recodes (not to NA)
            if (length(recode_pairs) > 0) {
              if (is.factor(x)) {
                x <- fct_recode(x, !!!recode_pairs)
              } else {
                x <- recode(x, !!!recode_pairs, .default = x)
              }
            }
            # Then, turn specified values into true NA
            if (length(na_values) > 0) {
              na_vals_parsed <- suppressWarnings(as.numeric(na_values))
              # If any are valid numbers, use na_if with numbers; else treat as character
              if (all(!is.na(na_vals_parsed))) {
                x <- na_if(x, na_vals_parsed)  # vectorized
              } else {
                x <- na_if(as.character(x), na_values)
              }
            }                                                                                                                                          
            x
          }
        )
      }
      if (op == "Mutate (expression)" && 
          nzchar(input$mutate_expr_new) && 
          nzchar(input$mutate_expr)) {
        
        new_var <- trimws(input$mutate_expr_new)
        expr_text <- trimws(input$mutate_expr)
        
        # Basic safety: prevent overwriting existing columns unless intentional
        if (new_var %in% names(df)) {
          confirm <- showModal(modalDialog(
            title = "Column exists",
            paste0("Variable '", new_var, "' already exists. Overwrite?"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("overwrite_confirm", "Overwrite", class = "btn-warning")
            )
          ))
          # We'll handle confirmation separately below
          return()
        }
        
        # Parse and apply the expression
        expr <- try(rlang::parse_expr(expr_text), silent = TRUE)
        if (inherits(expr, "try-error")) {
          showNotification("Invalid R expression", type = "error")
        } else {
          df <- df %>% mutate(!!new_var := !!expr)
        }
      }
      if (op == "Order Columns" && !is.null(input$order_cols)) {
        df <- df %>% select(all_of(input$order_cols))
      }
      rv$edited <- df
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Handle overwrite confirmation for Mutate (expression)
  observeEvent(input$overwrite_confirm, {
    req(input$mutate_expr_new, input$mutate_expr)
    new_var <- trimws(input$mutate_expr_new)
    expr_text <- trimws(input$mutate_expr)
    
    expr <- try(rlang::parse_expr(expr_text), silent = TRUE)
    if (inherits(expr, "try-error")) {
      showNotification("Invalid R expression", type = "error")
    } else {
      df <- rv$edited %>% mutate(!!new_var := !!expr)
      rv$edited <- df
      showNotification(paste("Created/overwritten variable:", new_var), type = "message")
    }
    removeModal()
  })
  # ---- 4. Editable Preview + FIXED CELL EDIT ----
  output$preview <- renderDT({
    req(rv$edited)
    datatable(rv$edited, editable = list(target = "cell"), 
              options = list(pageLength = input$n_rows, scrollX = TRUE))
  })
  
  # FIXED CELL EDIT: Supports factor (gender), numeric (age), character
  observeEvent(input$preview_cell_edit, {
    info <- input$preview_cell_edit
    req(rv$edited, info$row, info$col)
    
    col_name <- names(rv$edited)[info$col]
    old_val <- rv$edited[info$row, info$col][[1]]
    new_val <- info$value
    
    # Handle empty / "NA" / whitespace → NA
    if (is.null(new_val) || new_val == "" || tolower(trimws(new_val)) %in% c("na", "<na>")) {
      new_val <- NA
    } else {
      new_val <- trimws(new_val)
    }
    
    # CASE 1: Factor (e.g. gender)
    if (is.factor(rv$edited[[col_name]])) {
      if (is.na(new_val)) {
        rv$edited[info$row, col_name] <- NA
      } else {
        current_levels <- levels(rv$edited[[col_name]])
        if (!new_val %in% current_levels) {
          rv$edited[[col_name]] <- fct_expand(rv$edited[[col_name]], new_val)
        }
        rv$edited[info$row, col_name] <- factor(new_val, levels = levels(rv$edited[[col_name]]))
      }
      
      # CASE 2: Numeric (e.g. age)
    } else if (is.numeric(rv$edited[[col_name]])) {
      new_num <- suppressWarnings(as.numeric(new_val))
      if (is.na(new_num) && !is.na(new_val) && new_val != "") {
        showNotification(
          paste("Invalid number for", col_name, "→ reverted to", old_val),
          type = "error", duration = 5
        )
        return()
      }
      rv$edited[info$row, col_name] <- if (is.na(new_val)) NA else new_num
      
      # CASE 3: Character or other
    } else {
      rv$edited[info$row, col_name] <- as.character(new_val)
    }
  })
  
  # Reset
  observeEvent(input$reset_btn, {
    rv$edited <- rv$data
    showNotification("Reset to original data", type = "message")
  })
  
  # ---- 5. Save ----
  observeEvent(input$save_btn, {
    req(rv$edited)
    dir <- normalizePath(save_folder, mustWork = FALSE)
    base <- input$save_name
    df <- rv$edited
    
    # Save RData
    rdata_path <- file.path(dir, paste0(base, ".RData"))
    save(df, file = rdata_path)
    
    # Save Dictionary
    dict <- data.frame(
      Variable = names(df),
      Class = sapply(df, function(x) paste(class(x), collapse = ", ")),
      Label = sapply(df, function(x) {
        l <- attr(x, "label")
        if (is.null(l) || length(l) == 0) "" else paste(l, collapse = "; ")
      }),
      Levels = sapply(df, function(x) {
        if (is.factor(x)) paste(levels(x), collapse = "; ") else ""
      }),
      stringsAsFactors = FALSE
    )
    dict_path <- file.path(dir, paste0(base, "_dictionary.xlsx"))
    writexl::write_xlsx(dict, dict_path)
    
    output$save_msg <- renderText(paste0(
      "Files saved:\n", rdata_path, "\n", dict_path
    ))
  })
}

# ---- Launch ----
shinyApp(ui, server)