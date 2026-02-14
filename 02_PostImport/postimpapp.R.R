library(shiny)
library(readxl)
library(haven)
library(labelled)
library(writexl)
library(dplyr)
library(stringr)
library(DT)
library(forcats)
library(conflicted)
conflicts_prefer(dplyr::filter)

ui <- fluidPage(
  titlePanel("Data Loader & Variable Manager"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Load Dataset"),
      fileInput("file", "Upload Excel / Stata / RData / RDS",
                accept = c(".RData", ".rds", ".xlsx", ".xls", ".dta")),
      selectInput("env_dataset", "Or select dataset from R environment:",
                  choices = character(0)),
      actionButton("load_env", "Load Selected Dataset"),
      hr(),
      
      h4("Data Cleaning Options"),
      actionButton("btn_df", "Convert to data.frame"),
      actionButton("btn_trim", "Trim whitespace (character columns)"),
      actionButton("btn_lower", "Lowercase column names"),
      actionButton("btn_clean_names", "Remove underscores in names"),
      hr(),
      
      h4("Duplicate Check"),
      actionButton("show_dupes", "Show Duplicate Rows"),
      actionButton("remove_dupes", "Remove Duplicates"),
      hr(),
      
      uiOutput("var_selector"),
      textInput("new_varname", "Rename selected variable to:"),
      actionButton("rename_var", "Apply Rename"),
      hr(),
      
      radioButtons("convert_type", "Convert selected variable(s) to:",
                   choices = c("Numeric", "Factor", "Character")),
      
      conditionalPanel(
        condition = "input.convert_type == 'Factor'",
        textInput("levels", "Factor levels (comma separated)"),
        textInput("labels", "Factor labels (comma separated)"),
        textInput("factor_label", "Variable label (e.g. Sex of patient)")
      ),
      conditionalPanel(
        condition = "input.convert_type == 'Numeric'",
        textInput("num_label", "Variable label (e.g. Ejection Fraction)")
      ),
      actionButton("apply", "Apply Conversion"),
      hr(),
      
      h4("Create New Factor with case_when"),
      textInput("new_var", "New variable name"),
      textAreaInput("case_expr", "case_when expression", rows = 5,
                    placeholder = 'e.g. age > 65 ~ "Elderly", age <= 65 ~ "Young"'),
      textInput("new_levels", "Factor levels (comma-separated) – required",
                placeholder = "Elderly, Young"),
      textInput("new_labels", "Factor labels (comma-separated) – optional",
                placeholder = "Leave empty to use values as labels"),
      textInput("new_var_label", "Variable label (e.g. Age group: >65 = Elderly)"),
      actionButton("mutate_case", "Mutate New Factor"),
      hr(),
      
      uiOutput("forcats_ui"),
      hr(),
      h4("Save Modified Data"),
      textInput("save_name", "Base file name (no extension)", "modified_dataset"),
      downloadButton("download_rdata", "Download modified data (.RData)", class = "btn-success"),
      downloadButton("download_dict", "Download data dictionary (.xlsx)", class = "btn-success")
    ),   # close sidebarPanel
    
    mainPanel(
      h4("Dataset Preview"),
      DTOutput("preview"),
      hr(),
      h4("Variable Summary (complete unique values)"),
      DTOutput("var_summary"),
      hr(),
      h4("Structure & Labels"),
      verbatimTextOutput("structure")
    )
  )   # close sidebarLayout
)     # close fluidPage

  server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
 
  # Update environment dataset choices
  observe({
    updateSelectInput(session, "env_dataset", choices = ls(envir = .GlobalEnv))
  })
  
  # Load from file
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
                 "RData" = { e <- new.env(); load(input$file$datapath, envir = e); get(ls(e)[1], envir = e) },
                 "rds" = readRDS(input$file$datapath),
                 "xlsx" = , "xls" = read_excel(input$file$datapath),
                 "dta" = read_dta(input$file$datapath),
                 stop("Unsupported file type")
    )
    dataset(df)
    showNotification(paste("Loaded:", input$file$name), type = "message")
  })
  
  # Load from environment
  observeEvent(input$load_env, {
    req(input$env_dataset)
    obj <- get(input$env_dataset, envir = .GlobalEnv)
    if (!is.data.frame(obj)) {
      showNotification("Not a data frame.", type = "error")
    } else {
      dataset(obj)
      showNotification(paste("Loaded:", input$env_dataset), type = "message")
    }
  })
  
  # Cleaning actions
  observeEvent(input$btn_df, { req(dataset()); dataset(as.data.frame(dataset())); showNotification("Converted to data.frame") })
  observeEvent(input$btn_trim, { req(dataset()); dataset(dataset() %>% mutate(across(where(is.character), str_trim))); showNotification("Trimmed whitespace") })
  observeEvent(input$btn_lower, { req(dataset()); df <- dataset(); names(df) <- tolower(names(df)); dataset(df); showNotification("Lowercased names") })
  observeEvent(input$btn_clean_names, { req(dataset()); dataset(dataset() %>% rename_with(~str_replace_all(., "_", ""))); showNotification("Removed underscores") })
  
  # --- DUPLICATE HANDLING ---
  observeEvent(input$show_dupes, {
    req(dataset())
    df <- dataset()
    id_col <- names(df)[[1]]
    id_counts <- table(df[[id_col]])
    dup_ids <- names(id_counts)[id_counts > 1]
    dupes <- df[df[[id_col]] %in% dup_ids, , drop = FALSE]
    
    if (length(dup_ids) == 0) {
      showNotification(paste0("No duplicates in '", id_col, "'"), type = "message")
      return()
    }
    
    showModal(modalDialog(
      title = paste0("Duplicates in '", id_col, "'"),
      DTOutput("dupes_table"), easyClose = TRUE, size = "l"
    ))
    
    output$dupes_table <- renderDT({
      datatable(dupes, options = list(scrollX = TRUE, pageLength = 10, order = list(0, "asc")), rownames = FALSE)
    })
  })
  
  observeEvent(input$remove_dupes, {
    req(dataset())
    df <- dataset()
    id_col <- names(df)[[1]]
    n_before <- nrow(df)
    df <- df %>% distinct(.data[[id_col]], .keep_all = TRUE)
    n_after <- nrow(df)
    dataset(df)
    showNotification(paste0("Removed ", n_before - n_after, " duplicates"), type = "message")
  })
  
  # Variable selector
  output$var_selector <- renderUI({
    req(dataset())
    selectizeInput("variable", "Select variable(s)", choices = names(dataset()), multiple = TRUE)
  })
  
  # AUTO-FILL LEVELS & LABELS
  observeEvent(input$variable, {
    req(dataset())
    if (length(input$variable) != 1) return()
    v <- input$variable[[1]]
    df <- dataset()
    col <- df[[v]]
    
    vals <- NULL
    if (inherits(col, "haven_labelled")) {
      vals <- unique(as.character(haven::as_factor(col)))
    } else if (is.factor(col)) {
      vals <- levels(col)
    } else {
      vals <- unique(col[!is.na(col)])
      vals <- as.character(vals)
    }
    
    numeric_vals <- suppressWarnings(as.numeric(vals))
    if (!any(is.na(numeric_vals))) {
      vals_sorted <- as.character(sort(numeric_vals))
    } else {
      vals_sorted <- sort(vals)
    }
    
    updateTextInput(session, "levels", value = paste(vals_sorted, collapse = ", "))
    updateTextInput(session, "labels", value = paste(vals_sorted, collapse = ", "))
  })
  
  # Conversion
  observeEvent(input$apply, {
    req(dataset(), input$variable)
    df <- dataset()
    
    for (v in input$variable) {
      if (input$convert_type == "Numeric") {
        if (inherits(df[[v]], "haven_labelled")) {
          tmp <- as.character(haven::as_factor(df[[v]]))
          tmp_num <- suppressWarnings(as.numeric(tmp))
          if (all(!is.na(tmp_num))) df[[v]] <- tmp_num else df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
        } else if (is.factor(df[[v]])) {
          df[[v]] <- as.numeric(as.character(df[[v]]))
        } else {
          df[[v]] <- as.numeric(df[[v]])
        }
        if (nzchar(input$num_label)) var_label(df[[v]]) <- input$num_label
        
      } else if (input$convert_type == "Character") {
        if (inherits(df[[v]], "haven_labelled")) {
          df[[v]] <- as.character(haven::as_factor(df[[v]]))
        } else {
          df[[v]] <- as.character(df[[v]])
        }
        df[[v]] <- str_trim(df[[v]])
        
      } else if (input$convert_type == "Factor") {
        raw_lvls <- if (nzchar(input$levels)) trimws(strsplit(input$levels, ",")[[1]]) else character(0)
        raw_lbls <- if (nzchar(input$labels)) trimws(strsplit(input$labels, ",")[[1]]) else NULL
        
        if (inherits(df[[v]], "haven_labelled")) {
          data_vals <- as.character(haven::as_factor(df[[v]]))
        } else {
          data_vals <- as.character(df[[v]])
        }
        data_vals <- str_trim(data_vals)
        
        if (length(raw_lvls) == 0) {
          uniqs <- unique(data_vals[!is.na(data_vals)])
          uniq_num <- suppressWarnings(as.numeric(uniqs))
          raw_lvls <- if (!any(is.na(uniq_num))) as.character(sort(uniq_num)) else sort(uniqs)
        }
        raw_lbls <- if (is.null(raw_lbls)) raw_lvls else raw_lbls
        
        unmatched <- setdiff(unique(data_vals[!is.na(data_vals)]), raw_lvls)
        if (length(unmatched) > 0) {
          showNotification(paste0("Unmatched values in '", v, "': ", paste(unmatched, collapse=", ")), type="warning", duration=8)
        }
        
        df[[v]] <- factor(data_vals, levels = raw_lvls, labels = raw_lbls)
        if (nzchar(input$factor_label)) var_label(df[[v]]) <- input$factor_label
      }
    }
    dataset(df)
    showNotification("Conversion applied", type = "message")
  })
  
  # Rename
  observeEvent(input$rename_var, {
    req(dataset(), input$variable, input$new_varname)
    if (length(input$variable) != 1) { showNotification("Select one variable.", type="error"); return() }
    if (input$new_varname %in% names(dataset())) { showNotification("Name exists.", type="error"); return() }
    df <- dataset()
    names(df)[names(df) == input$variable] <- input$new_varname
    dataset(df)
    showNotification(paste("Renamed to", input$new_varname), type = "message")
  })
  
  # case_when mutate
  observeEvent(input$mutate_case, {
    req(dataset(), input$new_var, input$case_expr, input$new_levels)
    df <- dataset()
    expr_text <- paste0("case_when(", input$case_expr, ")")
    parsed <- tryCatch(parse(text = expr_text), error = function(e) NULL)
    if (is.null(parsed)) { showNotification("Syntax error", type="error"); return() }
    new_vals <- tryCatch(eval(parsed, envir = df), error = function(e) NULL)
    if (is.null(new_vals)) return()
    
    lvl_vec <- trimws(strsplit(input$new_levels, ",")[[1]])
    lbl_vec <- if (nzchar(input$new_labels)) trimws(strsplit(input$new_labels, ",")[[1]]) else NULL
    df[[input$new_var]] <- if (is.null(lbl_vec)) factor(new_vals, levels = lvl_vec) else factor(new_vals, levels = lvl_vec, labels = lbl_vec)
    if (nzchar(input$new_var_label)) var_label(df[[input$new_var]]) <- trimws(input$new_var_label)
    dataset(df)
    showNotification(paste("Created", input$new_var), type = "message")
  })
  
  # ------------------- NEW: FORCATS PANEL (ONE BUTTON PER FUNCTION) -------------------
  output$forcats_ui <- renderUI({
    req(dataset())
    tagList(
      hr(),
      h4("Create forcats-based factors"),
      
      # 1. fct_infreq
      selectInput("freq_var", "Variable → fct_infreq (ordered by frequency)", 
                  choices = names(dataset())),
      actionButton("make_infreq", "Create fct_infreq", 
                   class = "btn-primary", width = "100%"),
      
      hr(),
      
      # 2. NA → Missing
      selectInput("na_var", "Variable → NA → Missing", 
                  choices = names(dataset())),
      actionButton("make_na", "Create NA → Missing", 
                   class = "btn-primary", width = "100%"),
      
      hr(),
      
      # 3. Lump Top N
      selectInput("lump_var", "Variable → Lump top N", 
                  choices = names(dataset())),
      numericInput("lump_n", "Keep top N", value = 5, min = 1),
      textInput("lump_other", "Other level name", value = "Other"),
      actionButton("make_lump", "Create Lump Top N", 
                   class = "btn-primary", width = "100%"),
      
      hr(),
      
      # 4. Drop Unused Levels
      selectInput("drop_var", "Variable → Drop unused levels", 
                  choices = names(dataset())),
      actionButton("make_drop", "Drop Unused Levels", 
                   class = "btn-warning", width = "100%")
    )  # ← tagList closes here
  })  # ← renderUI closes here
  
  
  # 1. Create fct_infreq
  observeEvent(input$make_infreq, {
    req(dataset(), input$freq_var)
    df <- dataset()
    new_name <- paste0(input$freq_var, "_infreq")
    if (new_name %in% names(df)) {
      showNotification("Column already exists.", type = "error"); return()
    }
    df[[new_name]] <- fct_infreq(df[[input$freq_var]])
    var_label(df[[new_name]]) <- paste("Frequency-ordered:", input$freq_var)
    dataset(df)
    showNotification(paste("Created:", new_name), type = "message")
  })
  
  # 2. Create NA → Missing
  observeEvent(input$make_na, {
    req(dataset(), input$na_var)
    df <- dataset()
    new_name <- paste0(input$na_var, "_na")
    if (new_name %in% names(df)) {
      showNotification("Column already exists.", type = "error"); return()
    }
    df[[new_name]] <- fct_explicit_na(df[[input$na_var]], na_level = "Missing")
    var_label(df[[new_name]]) <- paste("NA → Missing:", input$na_var)
    dataset(df)
    showNotification(paste("Created:", new_name), type = "message")
  })
  
  # 3. Create Lump Top N
  observeEvent(input$make_lump, {
    req(dataset(), input$lump_var, input$lump_n)
    df <- dataset()
    new_name <- paste0(input$lump_var, "_top", input$lump_n)
    if (new_name %in% names(df)) {
      showNotification("Column already exists.", type = "error"); return()
    }
    df[[new_name]] <- fct_lump_n(df[[input$lump_var]], n = input$lump_n, other_level = input$lump_other)
    var_label(df[[new_name]]) <- paste0("Top ", input$lump_n, " + ", input$lump_other, ": ", input$lump_var)
    dataset(df)
    showNotification(paste("Created:", new_name), type = "message")
  })
  
  # 4. Drop Unused Levels
  observeEvent(input$make_drop, {
    req(dataset(), input$drop_var)
    df <- dataset()
    old_levels <- levels(df[[input$drop_var]])
    df[[input$drop_var]] <- fct_drop(df[[input$drop_var]])
    new_levels <- levels(df[[input$drop_var]])
    
    dropped <- setdiff(old_levels, new_levels)
    if (length(dropped) == 0) {
      showNotification("No unused levels to drop.", type = "message")
    } else {
      showNotification(paste("Dropped:", paste(dropped, collapse = ", ")), type = "message")
    }
    dataset(df)
  })
  # -----------------------------------------------------------------------------------
  
  # Preview
  output$preview <- renderDT({
    req(dataset())
    datatable(head(dataset(), 1000), options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # Var summary
  output$var_summary <- renderDT({
    req(dataset())
    df <- dataset()
    summary_df <- data.frame(
      Variable = names(df),
      Class = sapply(df, function(x) paste(class(x), collapse = ", ")),
      N_unique = sapply(df, function(x) length(unique(x[!is.na(x)]))),
      Unique_values = sapply(df, function(x) {
        if (is.numeric(x)) {
          vals <- unique(x[!is.na(x)])
          if (length(vals) < 6) paste(sort(vals), collapse = ", ") else ""
        } else {
          vals <- unique(x[!is.na(x)])
          if (length(vals) > 50) "Too many" else paste(vals, collapse = ", ")
        }
      }),
      stringsAsFactors = FALSE
    )
    datatable(summary_df, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE)
  })
  
  # Structure
  output$structure <- renderPrint({
    req(dataset())
    str(dataset())
  })
  
  # DOWNLOADS
  output$download_rdata <- downloadHandler(
    filename = function() paste0(input$save_name, ".RData"),
    content = function(file) {
      req(dataset())
      df <- dataset()
      save(df, file = file)
    }
  )
  
  output$download_dict <- downloadHandler(
    filename = function() paste0(input$save_name, "_dictionary.xlsx"),
    content = function(file) {
      req(dataset())
      df <- dataset()
      
      dict <- data.frame(
        Variable = names(df),
        Class = sapply(df, function(x) paste(class(x), collapse = ", ")),
        Label = sapply(df, function(x) { 
          l <- var_label(x); if (is.null(l)) "" else l 
        }),
        Levels = sapply(df, function(x) {
          if (is.factor(x)) paste(levels(x), collapse = "; ") else ""
        }),
        Codes_Labels = sapply(df, function(x) {
          if (is.factor(x)) {
            # factors: show level = label
            paste0(levels(x), "=", levels(x), collapse = "; ")
          } else if (!is.null(val_labels(x))) {
            # haven/labelled variables: show code = label
            vlab <- val_labels(x)
            paste0(names(vlab), "=", vlab, collapse = "; ")
          } else {
            # fallback: show first few unique values
            vals <- unique(x)
            vals <- vals[!is.na(vals)]
            paste(head(vals, 10), collapse = ", ")
          }
        }),
        stringsAsFactors = FALSE
      )
      
      writexl::write_xlsx(dict, file)
    }
  )
  
}

shinyApp(ui, server)

