# ============================================================
# Missing Value Manager – Density Plots + Unified Save (Updated)
# ============================================================

library(shiny)
library(readxl)
library(haven)
library(dplyr)
library(DT)
library(naniar)
library(mice)
library(ggplot2)
library(tidyr)
library(writexl)
library(gridExtra)
library(stringr)
library(purrr)
library(tibble)
library(zip)
conflicts_prefer(dplyr::filter)

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Data Importer and Missing Value Manager"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload Data"),
      fileInput("file", "Upload Excel / Stata / RData / CSV",
                accept = c(".xlsx",".xls",".dta",".RData",".rda",".rds",".csv")),
      selectInput("env_dataset", "Or select dataset from R environment:",
                  choices = character(0)),
      actionButton("load_env", "Load Selected Dataset"),
      hr(),
      
      h4("2. Show Extreme Values (numeric)"),
      numericInput("hilo_n", "Top / bottom n values", value = 5, min = 1, step = 1),
      actionButton("show_hilo", "Show Extremes"),
      hr(),
      
      h4("3. Replace Extreme Values with NA"),
      textInput("extreme_values", "Values to treat as missing (comma-sep):",
                value = "999, -99"),
      checkboxInput("exclude_id", "Exclude ID column from replacement", TRUE),
      selectInput("id_column", "ID column:", choices = NULL),
      actionButton("replace_extreme", "Replace Extremes"),
      hr(),
      
      h4("4. MICE Imputation"),
      checkboxInput("use_polr", "Use polr for ordered factors?", FALSE),
      actionButton("impute_mice", "Run MICE Imputation"),
      hr(),
      
      h4("5. Download Dataset"),
      textInput("save_name", "Base filename (no extension):", value = "dataset"),
      downloadButton("download_cleaned", "Download Cleaned (.RData)", class = "btn-primary"),
      downloadButton("download_dict", "Download Dictionary (.xlsx)", class = "btn-success"),
      downloadButton("download_imputed", "Download Imputed mids (.rds)", class = "btn-info"),
      hr(),
      verbatimTextOutput("save_msg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Missing Summary",
                 h4("Missing Values Table"),
                 DTOutput("missing_table"),
                 h4("Missingness Visualisation"),
                 plotOutput("vis_miss_plot"),
                 plotOutput("miss_upset_plot")
        ),
        tabPanel("Extreme Values",
                 h4("Top / Bottom numeric values"),
                 DTOutput("hilo_table")
        ),
        tabPanel("Imputation Results",
                 h4("Imputed Data Preview"),
                 DTOutput("imputed_table"),
                 h4("Density Comparison (Original vs Imputed)"),
                 fluidRow(
                   column(2, actionButton("prev_density", "Previous")),
                   column(2, actionButton("next_density", "Next")),
                   column(4, textOutput("density_page_info")),
                   column(4, textOutput("density_var_names"))
                 ),
                 plotOutput("imputation_density", height = "600px")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ---- 1. Load data ----
  df <- reactiveVal()
  imputed_df <- reactiveVal()
  imp_obj <- reactiveVal() # store full mids object
  
  observe({
    env_dfs <- Filter(function(x) is.data.frame(get(x, .GlobalEnv)), ls(.GlobalEnv))
    updateSelectInput(session, "env_dataset", choices = env_dfs)
  })
  
  observeEvent(input$file, {
    ext <- tolower(tools::file_ext(input$file$name))
    path <- input$file$datapath
    df_val <- switch(ext,
                     xlsx = , xls = read_excel(path) %>% as.data.frame(),
                     dta = read_dta(path) %>% as.data.frame(),
                     rdata = , rda = {e <- new.env(); load(path, e); get(ls(e)[1], e)},
                     rds = readRDS(path),
                     csv = read.csv(path, stringsAsFactors = FALSE),
                     NULL)
    df(df_val)
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset)
    df(get(input$env_dataset, .GlobalEnv))
  })
  
  # ---- 2. ID column selector ----
  observe({ req(df()); updateSelectInput(session, "id_column", choices = names(df())) })
  
  # ---- 3. Hi-Lo table ----
  hilo_df <- reactiveVal()
  observeEvent(input$show_hilo, {
    req(df())
    n <- max(1, input$hilo_n)
    hilo_fun <- function(x, n) {
      if (inherits(x, "labelled")) x <- haven::zap_labels(x)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      if (length(x) == 0) return(tibble(top = NA_real_, bottom = NA_real_))
      tibble(top = sort(x, decreasing = TRUE)[seq_len(min(n, length(x)))],
             bottom = sort(x)[seq_len(min(n, length(x)))])
    }
    df_num <- df() %>% select(where(is.numeric) | where(haven::is.labelled))
    if (ncol(df_num) == 0) {
      showNotification("No numeric variables found.", type = "warning")
      hilo_df(NULL); return()
    }
    res <- df_num %>% map_dfr(hilo_fun, n = n, .id = "variable") %>%
      pivot_longer(-variable, names_to = "type", values_to = "value") %>%
      arrange(variable, desc(type), desc(value))
    hilo_df(as.data.frame(res))
  })
  output$hilo_table <- renderDT({ req(hilo_df()); datatable(hilo_df(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) })
  
  # ---- 4. Replace extreme values ----
  observeEvent(input$replace_extreme, {
    req(df())
    extreme_vals <- as.numeric(str_split(trimws(input$extreme_values), ",")[[1]])
    extreme_vals <- extreme_vals[!is.na(extreme_vals)]
    if (length(extreme_vals) == 0) return()
    df_val <- df()
    cols <- if (input$exclude_id && input$id_column %in% names(df_val)) setdiff(names(df_val), input$id_column) else names(df_val)
    df_val[cols] <- lapply(df_val[cols], function(col) { col[col %in% extreme_vals] <- NA; col })
    df(df_val)
  })
  
  # ---- 5. Missing summary ----
  output$missing_table <- renderDT({
    req(df())
    miss <- data.frame(
      Variable = names(df()),
      NA_Count = sapply(df(), function(x) sum(is.na(x))),
      NA_Percent = round(sapply(df(), function(x) sum(is.na(x))/length(x)*100), 2)
    )
    datatable(miss, options = list(pageLength = 10))
  })
  output$vis_miss_plot   <- renderPlot({ req(df()); vis_miss(df()) })
  output$miss_upset_plot <- renderPlot({ req(df()); gg_miss_upset(df()) })
  
  # ---- 6. MICE imputation ----
  observeEvent(input$impute_mice, {
    req(df())
    df_val <- df() %>% as.data.frame()
    meth <- make.method(df_val)
    factor_vars <- names(df_val)[sapply(df_val, is.factor)]
    for (v in factor_vars) {
      nlev <- nlevels(df_val[[v]])
      if (nlev == 2) { df_val[[v]] <- as.numeric(df_val[[v]]) - 1; meth[v] <- "logreg" }
      else if (nlev > 2) { if (input$use_polr && is.ordered(df_val[[v]])) meth[v] <- "polr" else meth[v] <- "polyreg" }
    }
    numeric_vars <- names(df_val)[sapply(df_val, is.numeric)]
    if(length(numeric_vars) > 0) meth[numeric_vars] <- "pmm"
    withProgress(message = "Running MICE ...", {
      set.seed(123)
      imp <- mice(df_val, method = meth, m = 5, maxit = 50, printFlag = FALSE, seed = 42)
    })
    imp_obj(imp)
    imputed_df(lapply(1:imp$m, function(i) complete(imp, i)))
  })
  
  # ---- 7. Imputed preview ----
  output$imputed_table <- renderDT({
    req(imputed_df())
    datatable(imputed_df()[[1]], options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- 8. Continuous variables ----
  cont_vars <- reactive({
    req(df())
    vars <- names(df())
    num_vars <- vars[sapply(df()[vars], is.numeric)]
    cont <- sapply(df()[num_vars], function(x) length(unique(na.omit(x))) >= 10)
    num_vars[cont]
  })
  
  # ---- 9. Pagination for density plots ----
  density_page <- reactiveVal(1)
  
  observeEvent(input$next_density, {
    n <- length(cont_vars())
    if (n == 0) return()
    max_page <- ceiling(n / 2)
    density_page(min(density_page() + 1, max_page))
  })
  
  observeEvent(input$prev_density, {
    density_page(max(density_page() - 1, 1))
  })
  
  observeEvent(df(), density_page(1))
  observeEvent(imputed_df(), density_page(1))
  
  # ---- 10. Density plots ----
  output$imputation_density <- renderPlot({
    req(df(), imputed_df(), cont_vars())
    vars <- cont_vars()
    n <- length(vars)
    if (n == 0) return(ggplot() + labs(title = "No continuous variables") + theme_void())
    
    page <- density_page()
    start <- (page - 1) * 2 + 1
    end   <- min(start + 1, n)
    plot_vars <- vars[start:end]
    
    plots <- list()
    orig <- df()
    all_imp <- imputed_df()
    
    for (v in plot_vars) {
      dfc <- tibble(
        value = c(orig[[v]], unlist(lapply(all_imp, function(d) d[[v]]))),
        type  = c(
          rep("Original", nrow(orig)),
          rep(paste0("Imputed_", seq_along(all_imp)), each = nrow(orig))
        )
      ) %>% drop_na(value)
      
      if (nrow(dfc) == 0) next
      
      p <- ggplot(dfc, aes(x = value, color = type, fill = type)) +
        geom_density(alpha = 0.3, size = 1) +
        scale_color_manual(
          values = c("Original" = "#A41D3C",
                     setNames(rep("#377EB8", length(all_imp)), paste0("Imputed_", seq_along(all_imp))))
        ) +
        scale_fill_manual(
          values = c("Original" = "#A41D3C",
                     setNames(rep("#377EB8", length(all_imp)), paste0("Imputed_", seq_along(all_imp))))
        ) +
        labs(title = v, x = v, y = "Density") +
        theme_bw() +
        theme(legend.position = "top")
      
      plots[[v]] <- p
    }
    
    do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
  })
  
  # ---- 11. Page info ----
  output$density_page_info <- renderText({
    req(cont_vars())
    n <- length(cont_vars())
    if (n == 0) return("No continuous variables")
    page <- density_page()
    max_page <- ceiling(n / 2)
    paste0("Page ", page, " of ", max_page)
  })
  
  output$density_var_names <- renderText({
    req(cont_vars(), density_page())
    vars <- cont_vars()
    n <- length(vars)
    if (n == 0) return("")
    page <- density_page()
    start <- (page - 1) * 2 + 1
    end   <- min(start + 1, n)
    paste(vars[start:end], collapse = " | ")
  })
  
  # ---- 12. Unified Save with downloadHandler ----
  output$download_cleaned <- downloadHandler(
    filename = function() {
      paste0(make.names(trimws(input$save_name)), ".RData")
    },
    content = function(file) {
      req(df())
      ds <- df()
      save(ds, file = file)
    }
  )
  
  output$download_dict <- downloadHandler(
    filename = function() {
      paste0(make.names(trimws(input$save_name)), "_dictionary.xlsx")
    },
    content = function(file) {
      req(df())
      ds <- df()
      dict <- data.frame(
        Variable = names(ds),
        Class = sapply(ds, function(x) paste(class(x), collapse = ", ")),
        Label = sapply(ds, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else l }),
        Levels = sapply(ds, function(x) {
          if (is.factor(x)) paste(levels(x), collapse = "; ") else ""
        }),
        stringsAsFactors = FALSE
      )
      writexl::write_xlsx(dict, file)
    }
  )
  
  output$download_imputed <- downloadHandler(
    filename = function() {
      paste0(make.names(trimws(input$save_name)), "_mids.rds")
    },
    content = function(file) {
      req(imp_obj())
      saveRDS(imp_obj(), file)   # saves the full mids object with all imputations
    }
  )
  
  output$save_msg <- renderText({
    "Use the buttons above to download the cleaned dataset, dictionary, or full mids object."
  })
}

shinyApp(ui, server)
