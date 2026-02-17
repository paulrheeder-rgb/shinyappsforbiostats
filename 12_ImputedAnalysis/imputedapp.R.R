# ============================================================
# Shiny App: MICE Analysis App (Regression + Group Comparisons)
# Fixed: Safe regression, all variables inspect, factor/binary handling
# + Corrected stripplot for observed vs imputed values
# ============================================================

library(shiny)
library(mice)
library(DT)
library(gtsummary)
library(gt)
library(officer)
library(flextable)
library(dplyr)
library(ggplot2)
library(conflicted)
conflicts_prefer(zip::zip)


# =========================
# UI
# =========================
ui <- fluidPage(
  titlePanel("Multiple Imputation Analysis App (mice + gtsummary)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Load Imputed Dataset (mids object)"),
      fileInput("file", "Upload .rds mids object", accept = ".rds"),
      selectInput("env_mids", "Or select mids from environment:", "None"),
      actionButton("load_env", "Load from environment"),
      hr(),
      
      h4("2. Analysis setup"),
      radioButtons(
        "analysis_type",
        "Analysis type:",
        choices = c(
          "Regression" = "reg",
          "Group comparison (t-test equivalent)" = "ttest",
          "Group means" = "means",
          "Inspect imputations" = "inspect"
        )
      ),
      radioButtons(
        "model_type",
        "Model type:",
        choices = c("Linear" = "lm", "Logistic" = "glm")
      ),
      
      uiOutput("response_ui"),
      uiOutput("predictors_ui"),
      uiOutput("group_ui"),
      uiOutput("inspect_ui"),
      
      hr(),
      actionButton("fit", "Run analysis"),
      hr(),
      
      textInput("tbl_title", "Regression table title:", value = "Regression Results")
      ,
      downloadButton("download_bundle", "Download Results (ZIP)")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Imputation Summary",
          verbatimTextOutput("imp_summary"),
          hr(),
          verbatimTextOutput("inspect_imp"),
          hr(),
          plotOutput("impute_plot", height = "400px")
        ),
        tabPanel(
          "Results",
          gt::gt_output("pooled_gt")
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  mids_obj   <- reactiveVal(NULL)
  pooled_tbl <- reactiveVal(NULL)
  
  # ---- detect mids in env ----
  observe({
    mids_names <- Filter(
      function(x) inherits(get(x, .GlobalEnv), "mids"),
      ls(.GlobalEnv)
    )
    updateSelectInput(session, "env_mids",
                      choices = c("None", mids_names))
  })
  
  # ---- load mids ----
  observeEvent(input$file, {
    obj <- readRDS(input$file$datapath)
    req(inherits(obj, "mids"))
    mids_obj(obj)
  })
  
  observeEvent(input$load_env, {
    req(input$env_mids != "None")
    obj <- get(input$env_mids, .GlobalEnv)
    req(inherits(obj, "mids"))
    mids_obj(obj)
  })
  
  # ---- original data ----
  original_data <- reactive({
    req(mids_obj())
    mids_obj()$data
  })
  
  output$imp_summary <- renderPrint({
    req(mids_obj())
    mids_obj()
  })
  
  # ---- Inspect imputations safely ----
  output$inspect_ui <- renderUI({
    req(mids_obj())
    if (input$analysis_type == "inspect") {
      selectInput(
        "inspect_var",
        "Variable to inspect:",
        choices = names(mids_obj()$data)  # show all variables
      )
    }
  })
  
  output$inspect_imp <- renderPrint({
    req(mids_obj(), input$analysis_type == "inspect", input$inspect_var)
    
    imp <- mids_obj()
    var <- input$inspect_var
    
    if (var %in% names(imp$imp)) {
      cat("Variable:", var, "(imputed)\n\n")
      imps <- imp$imp[[var]]
      for (i in seq_along(imps)) {
        cat("Imputation", i, ":\n")
        print(head(imps[[i]]))
        cat("\n")
      }
    } else {
      cat("Variable:", var, "(complete / no missing)\n")
      print(head(imp$data[[var]]))
    }
  })
  
  # ---- Plot: stripplot of observed + imputed ----
  output$impute_plot <- renderPlot({
    req(mids_obj(), input$analysis_type == "inspect", input$inspect_var)
    
    imp <- mids_obj()
    var <- input$inspect_var
    n <- nrow(imp$data)
    
    # Observed values
    plot_df <- data.frame(
      row = 1:n,
      value = imp$data[[var]],
      type = "observed"
    )
    
    # Only add imputed values if variable was imputed
    if (var %in% names(imp$imp)) {
      imps_list <- imp$imp[[var]]
      for (i in seq_along(imps_list)) {
        vals <- imps_list[[i]]
        
        # Get positions of missing values in original data
        missing_rows <- which(is.na(imp$data[[var]]))
        
        # Safety check
        if(length(vals) != length(missing_rows)) next
        
        plot_df <- rbind(
          plot_df,
          data.frame(
            row = missing_rows,
            value = vals,
            type = paste0("imputation_", i)
          )
        )
      }
    }
    
    # Colors: black for observed, rainbow for imputations
    type_levels <- unique(plot_df$type)
    colors <- c("observed" = "black",
                setNames(rainbow(length(type_levels)-1), type_levels[type_levels != "observed"]))
    
    ggplot(plot_df, aes(x = row, y = value, color = type)) +
      geom_jitter(width = 0.2, height = 0, alpha = 0.8, size = 2) +
      scale_color_manual(values = colors) +
      labs(
        title = paste("Inspect variable:", var),
        x = "Row index",
        y = var,
        color = "Value type"
      ) +
      theme_minimal()
  })
  
  
  # =========================
  # Dynamic UI
  # =========================
  output$response_ui <- renderUI({
    req(original_data(), input$analysis_type != "inspect")
    selectInput("response", "Outcome variable:", names(original_data()))
  })
  
  output$predictors_ui <- renderUI({
    req(original_data(), input$response, input$analysis_type == "reg")
    selectInput(
      "predictors",
      "Predictors:",
      choices = setdiff(names(original_data()), input$response),
      multiple = TRUE
    )
  })
  
  output$group_ui <- renderUI({
    req(original_data())
    if (input$analysis_type %in% c("ttest", "means")) {
      selectInput(
        "group_var",
        "Grouping variable:",
        choices = names(original_data())
      )
    }
  })
  
  # =========================
  # Fit + pool
  # =========================
  # Fit + pool
  observeEvent(input$fit, {
    
    imp <- mids_obj()
    
    # ---- Inspect only ----
    if (input$analysis_type == "inspect") {
      pooled_tbl(NULL)
      return()
    }
    
    # ---- ensure grouping variable is factor ----
    if (input$analysis_type %in% c("ttest", "means")) {
      imp$data[[input$group_var]] <- as.factor(imp$data[[input$group_var]])
    }
    
    # ---- Fit safely using your previous working code ----
    mira <- with(imp, {
      
      if (input$analysis_type == "reg") {
        f <- as.formula(
          paste(input$response, "~", paste(input$predictors, collapse = " + "))
        )
        if (input$model_type == "glm") {
          glm(f, family = binomial)
        } else {
          lm(f)
        }
        
      } else if (input$analysis_type == "ttest") {
        lm(get(input$response) ~ factor(get(input$group_var)))
        
      } else if (input$analysis_type == "means") {
        lm(get(input$response) ~ 0 + factor(get(input$group_var)))
      }
    })
    
    req(inherits(mira, "mira"))
    
    tbl <- tbl_regression(
      mira,
      exponentiate = input$model_type == "glm",
      estimate_fun = ~style_number(.x, digits = 3),
      pvalue_fun   = ~style_pvalue(.x, digits = 3)
    ) |> 
      modify_header(label ~ "**Variable**") |> 
      bold_p(t = 0.05)
    
    pooled_tbl(tbl)
  })
  # =========================
  # Output pooled table
  # =========================
  output$pooled_gt <- gt::render_gt({
    req(pooled_tbl())
    
    tbl_gt <- pooled_tbl() |> as_gt()
    
    # Add custom title if provided
    if (!is.null(input$tbl_title) && input$tbl_title != "") {
      tbl_gt <- tbl_gt |> 
        tab_header(title = input$tbl_title)
    }
    
    tbl_gt
  })
  
  # save to folder
  
  output$download_bundle <- downloadHandler(
    filename = function() {
      paste0("results_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip")
    },
    content = function(file) {
      req(pooled_tbl())   # make sure results exist
      
      tmpdir <- tempdir()
      prefix <- paste0("results_", format(Sys.time(), "%Y%m%d_%H%M"))
      
      # --- Save regression table as Word ---
      tbl <- pooled_tbl()
      ft <- as_flex_table(tbl) %>% theme_booktabs() %>% autofit()
      doc <- read_docx() %>% body_add_flextable(ft)
      word_path <- file.path(tmpdir, paste0(prefix, "_results.docx"))
      print(doc, target = word_path)
      
      # --- Save plot (only if inspect mode was used) ---
      plot_files <- c()
      if (input$analysis_type == "inspect" && !is.null(input$inspect_var)) {
        # Recreate the plot
        imp <- mids_obj()
        var <- input$inspect_var
        n <- nrow(imp$data)
        
        plot_df <- data.frame(
          row = 1:n,
          value = imp$data[[var]],
          type = "observed"
        )
        
        if (var %in% names(imp$imp)) {
          imps_list <- imp$imp[[var]]
          for (i in seq_along(imps_list)) {
            vals <- imps_list[[i]]
            missing_rows <- which(is.na(imp$data[[var]]))
            if (length(vals) != length(missing_rows)) next
            plot_df <- rbind(
              plot_df,
              data.frame(
                row = missing_rows,
                value = vals,
                type = paste0("imputation_", i)
              )
            )
          }
        }
        
        type_levels <- unique(plot_df$type)
        colors <- c("observed" = "black",
                    setNames(rainbow(length(type_levels)-1),
                             type_levels[type_levels != "observed"]))
        
        p <- ggplot(plot_df, aes(x = row, y = value, color = type)) +
          geom_jitter(width = 0.2, height = 0, alpha = 0.8, size = 2) +
          scale_color_manual(values = colors) +
          labs(title = paste("Inspect variable:", var),
               x = "Row index", y = var, color = "Value type") +
          theme_minimal()
        
        plot_path <- file.path(tmpdir, paste0(prefix, "_inspect_plot.png"))
        ggsave(plot_path, plot = p, width = 9, height = 6, dpi = 300)
        plot_files <- c(plot_files, plot_path)
      }
      
      # --- Bundle into ZIP ---
      files_to_zip <- c(word_path, plot_files)
      zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
      
    }
  )
  
  
  
}

# =========================
# Run app
# =========================
shinyApp(ui, server)

