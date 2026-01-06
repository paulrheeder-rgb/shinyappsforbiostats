library(shiny)
library(readxl)
library(haven)
library(labelled)
library(dplyr)
library(DT)
library(writexl)

# ---- (your apply_dictionary stays exactly the same) ----
apply_dictionary <- function(df, dict) {
  dict <- dict %>%
    mutate(across(everything(), ~trimws(as.character(.x)))) %>%
    mutate(
      levels = ifelse(grepl("^\\d\\.\\d+$", levels),
                      gsub("\\.", ",", levels), levels),
      level_labels = ifelse(grepl("^\\d\\.\\d+$", level_labels),
                            gsub("\\.", ",", level_labels), level_labels)
    )
  
  for (i in seq_len(nrow(dict))) {
    var <- dict$varname[i]
    if (!var %in% names(df)) next
    if (is.na(dict$levels[i]) || dict$levels[i] == "") next
    
    split_values <- function(x) {
      if (grepl(";", x)) strsplit(x, ";")[[1]] else strsplit(x, ",")[[1]]
    }
    codes  <- trimws(split_values(dict$levels[i]))
    labels <- trimws(split_values(dict$level_labels[i]))
    codes  <- codes[codes != "" & !is.na(codes)]
    labels <- labels[labels != "" & !is.na(labels)]
    
    codes_num <- suppressWarnings(as.numeric(codes))
    codes_are_numeric <- all(!is.na(codes_num))
    
    if (codes_are_numeric) {
      df_num <- suppressWarnings(as.numeric(df[[var]]))
      if (all(!is.na(df_num))) df[[var]] <- df_num
      df[[var]] <- factor(df[[var]], levels = codes_num, labels = labels)
    } else {
      df[[var]] <- factor(trimws(as.character(df[[var]])), levels = codes, labels = labels)
    }
  }
  
  for (i in seq_len(nrow(dict))) {
    var <- dict$varname[i]
    if (var %in% names(df) && !is.na(dict$label[i]) && dict$label[i] != "") {
      attr(df[[var]], "label") <- dict$label[i]
    }
  }
  df
}

# ---- UI ----------------------------------------------------
ui <- fluidPage(
  titlePanel("Apply Data Dictionary (Excel / RData / Stata)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Dataset", accept = c(".xlsx",".xls",".RData",".dta")),
      fileInput("dictfile", "Dictionary:varname,type,label,levels,level_labels", accept = c(".xlsx",".xls",".RData",".dta")),
      actionButton("apply", "Apply Dictionary", class = "btn-primary"),
      hr(),
      textInput("save_name", "Base name (no extension)", "cleaned_data"),
      actionButton("save_files", "Save Files", class = "btn-success"),
      verbatimTextOutput("save_msg")
    ),
    mainPanel(
      h4("Cleaned data preview"),
      DTOutput("table"),
      hr(),
      h4("Variable summary (name | type | label)"),
      DTOutput("labels")
    )
  )
)

# ---- SERVER ------------------------------------------------
server <- function(input, output, session) {
  save_folder <- "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/data"
  
  dataset <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    path <- input$datafile$datapath
    if (ext %in% c("xlsx","xls")) read_excel(path)
    else if (ext == "RData") { e <- new.env(); load(path, envir = e); as.data.frame(e[[ls(e)[1]]]) }
    else if (ext == "dta") as.data.frame(read_dta(path))
    else stop("Unsupported")
  })
  
  dictionary <- reactive({
    req(input$dictfile)
    ext <- tools::file_ext(input$dictfile$name)
    path <- input$dictfile$datapath
    if (ext %in% c("xlsx","xls")) read_excel(path)
    else if (ext == "RData") { e <- new.env(); load(path, envir = e); as.data.frame(e[[ls(e)[1]]]) }
    else if (ext == "dta") as.data.frame(read_dta(path))
    else stop("Unsupported")
  })
  
  modified <- eventReactive(input$apply, {
    apply_dictionary(dataset(), dictionary())
  })
  
  output$table <- renderDT({
    req(modified())
    datatable(modified(), options = list(scrollX = TRUE))
  })
  
  ## ---- CLEAN SUMMARY (the only part you needed) ----
  make_var_summary <- function(df) {
    data.frame(
      Variable = names(df),
      `Data-type` = sapply(df, function(col) {
        cls <- class(col)
        cls <- setdiff(cls, c("haven_labelled", "labelled", "vctrs_vctr"))
        if (length(cls) == 0) "character"
        else paste(cls, collapse = ", ")
      }),
      `Variable-label` = sapply(df, function(col) {
        lbl <- attr(col, "label")
        if (is.null(lbl)) "" else trimws(as.character(lbl))
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  
  output$labels <- renderDT({
    req(modified())
    datatable(
      make_var_summary(modified()),
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE
    )
  })
  
  ## ---- SAVE ------------------------------------------------
  observeEvent(input$save_files, {
    req(modified())
    dir <- normalizePath(save_folder)
    base <- input$save_name
    df <- modified()
    
    rdata_path <- file.path(dir, paste0(base, ".RData"))
    save(df, file = rdata_path)
    
    dict <- make_var_summary(df)
    dict$Levels <- sapply(df, function(col) {
      if (is.factor(col)) paste(levels(col), collapse = "; ") else ""
    })
    dict_path <- file.path(dir, paste0(base, "_dictionary.xlsx"))
    writexl::write_xlsx(dict, dict_path)
    
    output$save_msg <- renderText(paste0(
      "Files saved to:\n", rdata_path, "\n", dict_path
    ))
  })
}

shinyApp(ui, server)
