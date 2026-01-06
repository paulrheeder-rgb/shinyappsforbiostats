# REGRESSION DASHBOARD


library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(officer)
library(patchwork)
library(pROC)
library(rms)
library(DescTools)
library(margins)
library(ggeffects)
library(vip)
library(ggstats)
library(rempsyc)
library(see)
library(performance)
library(gridExtra)
library(broom)
library(flextable)
library(ResourceSelection)
library(haven)
library(api2lm)
library(shinyFiles)
library(readxl)

ui <- fluidPage(
  titlePanel("Regression Dashboard with DFBETAs Option"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Load Dataset"),
      fileInput("file", "Upload Excel, CSV, STATA or RData",
                accept = c(".xlsx", ".xls", ".csv", ".RData", ".rda", ".dta")),
      selectInput("env_dataset", "Or select dataset from R environment:",
                  choices = c("None")),
      actionButton("load_env", "Load Dataset"),
      hr(),
      
      radioButtons("model_type", "Model type:",
                   choices = c("Linear" = "lm", "Logistic" = "glm")),
      uiOutput("yvar_ui"),
      uiOutput("xvars_ui"),
      textInput("interaction_terms", "Specify interaction terms (e.g., x1:x2 + x3:x4)", ""),
      actionButton("fit_model", "Fit Model"),
      hr(),
      
      h4("Save Outputs"),
      shinyDirButton("save_dir", "Choose Folder", "Select folder to save outputs"),
      textInput("save_prefix", "Prefix for saved files:", "regression"),
      actionButton("save_outputs", "Save All Outputs"),
      textOutput("save_msg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("First 10 rows"),
                 tableOutput("data_preview_table"),
                 br(),
                 h4("Data Summary"),
                 verbatimTextOutput("data_summary")
        ),
        tabPanel("Model Summary", verbatimTextOutput("model_summary")),
        tabPanel("Model Commentary", textOutput("model_commentary")),
        
        tabPanel("Diagnostics Continuous",
                 radioButtons("diag_option", "Select diagnostic view:",
                              choices = c("Base Plots", "DFBETAs Plot")),
                 conditionalPanel(
                   condition = "input.diag_option == 'DFBETAs Plot'",
                   numericInput("id_n", "Number of labeled points:", 4, min = 1, max = 20)
                 ),
                 plotOutput("diagnostic_plot", height = "700px"),
                 actionButton("show_influential", "Show Top Influential Cases"),
                 tableOutput("influential_cases")
        ),
        tabPanel("Diagnostics Categorical",
                 h4("Select Variables"),
                 uiOutput("cat_var_ui"),
                 uiOutput("cont_var_ui"),
                 hr(),
                 h4("Plots"),
                 plotOutput("catdiag_qq"),
                 plotOutput("catdiag_density"),
                 plotOutput("catdiag_varplot"),
                 plotOutput("catdiag_outliers")
        ),
        
        tabPanel("Predicted Probabilities", plotOutput("pred_plot")),
        tabPanel("Regression Table",
                 h4("Regression Table (with Global P-values for factors >2 levels)"),
                 gt_output("reg_table_gt")
        ),
        tabPanel("Variable Importance", plotOutput("vip_plot")),
        tabPanel("Model Performance",
                 verbatimTextOutput("perf_metrics"),
                 br(),
                 uiOutput("perf_text")
        ),
        
        tabPanel("Calibration & GOF",
                 verbatimTextOutput("gof_metrics"),
                 verbatimTextOutput("brier_score"),
                 uiOutput("brier_text"),
                 br(),
                 h4("Basic Calibration Plot"),
                 plotOutput("calibration_plot"),
                 br(),
                 conditionalPanel(
                   condition = "input$model_type == 'glm'",
                   h4("Bootstrapped Calibration Plot (rms)"),
                   plotOutput("boot_calibration_plot")
                 )
        ),
        tabPanel("ROC Curve", plotOutput("roc_plot")),
        tabPanel("Marginal Effects", plotOutput("margins_plot")),
        tabPanel("Marginal Effects Table", tableOutput("margins_table")),
        tabPanel("Interaction Plot",
                 h4("Visualization of Specified Interaction"),
                 p("Shows the first interaction term entered in the sidebar."),
                 br(),
                 plotOutput("interaction_plot", height = "600px")
        ),
        tabPanel("Coefficient Plot", plotOutput("coef_plot")),
        tabPanel("Coefficient Table (ggstats)", plotOutput("coef_table_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Update environment dropdown
  observe({
    dfs <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
    updateSelectInput(session, "env_dataset", choices = c("None", dfs))
  })
  
  # Dataset loading
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    
    df <- tryCatch({
      switch(ext,
             csv = read.csv(input$file$datapath, stringsAsFactors = TRUE),
             xlsx = , xls = readxl::read_excel(input$file$datapath),
             rdata = , rda = {
               e <- new.env()
               load(input$file$datapath, envir = e)
               get(ls(e)[1], e)
             },
             dta = haven::read_dta(input$file$datapath) %>%
               mutate(across(where(is.labelled), as_factor)),
             stop("Unsupported file type")
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
    
    if (is.data.frame(df)) dataset(df)
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset != "None")
    df <- get(input$env_dataset, envir = .GlobalEnv)
    if (is.data.frame(df)) dataset(df)
  })
  
  # Data Preview
  output$data_preview_table <- renderTable({ req(dataset()); head(dataset(), 10) }, rownames = TRUE)
  output$data_summary <- renderPrint({ req(dataset()); summary(dataset()) })
  
  # Variable selectors
  output$yvar_ui <- renderUI({ req(dataset()); selectInput("yvar", "Response:", choices = names(dataset())) })
  output$xvars_ui <- renderUI({ req(dataset()); selectInput("xvars", "Predictors:", choices = names(dataset()), multiple = TRUE) })
  
  # Model fit
  model_fit <- eventReactive(input$fit_model, {
    req(dataset(), input$yvar, input$xvars)
    df <- dataset()
    formula_text <- paste(input$yvar, "~", paste(input$xvars, collapse = " + "))
    if (nzchar(input$interaction_terms)) formula_text <- paste(formula_text, "+", trimws(input$interaction_terms))
    formula <- as.formula(formula_text)
    if (input$model_type == "lm") lm(formula, data = df) else glm(formula, data = df, family = binomial)
  })
  
  # Model summary
  output$model_summary <- renderPrint({ req(model_fit()); summary(model_fit()) })
  
  # Model commentary
  output$model_commentary <- renderText({
    req(model_fit())
    fit <- model_fit()
    coef_sum <- summary(fit)$coefficients
    coef_df <- as.data.frame(coef_sum)
    coef_df$term <- rownames(coef_sum)
    rownames(coef_df) <- NULL
    coef_df <- coef_df[!coef_df$term %in% c("(Intercept)", "Intercept"), ]
    sig_df <- coef_df[coef_df[,4] < 0.05, ]
    if (nrow(sig_df) == 0) return("No significant associations found (p < 0.05).")
    comments <- sapply(1:nrow(sig_df), function(i) {
      term <- sig_df$term[i]
      est <- round(sig_df$Estimate[i], 3)
      pval <- format.pval(sig_df[i,4], digits = 3, eps = 0.001)
      if (input$model_type == "glm") {
        or <- round(exp(est), 2)
        direction <- ifelse(est > 0, "increases odds", "decreases odds")
        paste0(term, " ", direction, " (OR = ", or, ", p = ", pval, ")")
      } else {
        direction <- ifelse(est > 0, "positively", "negatively")
        paste0(term, " is ", direction, " associated (β = ", est, ", p = ", pval, ")")
      }
    })
    paste(comments, collapse = ". ")
  })
  
  # Regression Table
  # Regression Table
  output$reg_table_gt <- render_gt({
    req(model_fit(), input$yvar)
    
    tbl <- tbl_regression(
      model_fit(),
      exponentiate = (input$model_type == "glm")
    ) %>%
      add_global_p() %>%
      as_gt() %>%
      tab_header(
        title = paste("Regression Table for Outcome:", input$yvar)
      )
    
    tbl
  })
  
  
  # Diagnostics Continuous
  output$diagnostic_plot <- renderPlot({
    req(model_fit())
    fit <- model_fit()
    is_logit <- input$model_type == "glm"
    
    if (input$diag_option == "Base Plots") {
      if (is_logit) {
        model_data <- fit$model
        probs <- predict(fit, type = "response")
        probs <- pmin(pmax(probs, 1e-8), 1 - 1e-8)
        logit <- log(probs / (1 - probs))
        numeric_cols <- model_data %>% select(where(is.numeric))
        if (input$yvar %in% names(numeric_cols)) {
          numeric_preds <- numeric_cols %>% select(-all_of(input$yvar))
        } else {
          numeric_preds <- numeric_cols
        }
        if (ncol(numeric_preds) == 0) {
          plot.new()
          text(0.5, 0.5, "No numeric predictors for linearity check.", cex = 1.3, col = "darkred")
          return()
        }
        plot_df <- numeric_preds %>%
          mutate(logit = logit) %>%
          pivot_longer(cols = -logit, names_to = "predictor", values_to = "value") %>%
          drop_na()
        ggplot(plot_df, aes(x = logit, y = value)) +
          geom_point(size = 0.8, alpha = 0.6, color = "#2C3E50") +
          geom_smooth(method = "loess", se = TRUE, color = "#E31A1C", fill = "#FFBABA") +
          facet_wrap(~ predictor, scales = "free_y") +
          theme_bw() +
          labs(title = "Linearity Assumption Check (Logistic)", x = "Logit", y = "Predictor")
      } else {
        par(mfrow = c(2, 2))
        plot(fit, which = 1:4)
        par(mfrow = c(1, 1))
      }
    } else {
      if (is_logit) {
        plot.new()
        text(0.5, 0.5, "DFBETAs plot is not available for logistic regression.", cex = 1.4, col = "darkred")
      } else {
        req(input$id_n)
        api2lm::dfbetas_plot(fit, id_n = input$id_n)
      }
    }
  })
  
  # Influential Cases
  influential_cases <- eventReactive(input$show_influential, {
    req(model_fit())
    fit <- model_fit()
    aug <- broom::augment(fit)
    aug %>%
      select(.cooksd, .hat) %>%
      mutate(row = row_number()) %>%
      arrange(desc(.cooksd)) %>%
      head(10)
  })
  
  output$influential_cases <- renderTable({
    req(influential_cases())
    influential_cases()
  })
  
  # Categorical Diagnostics UI & Plots
  output$cat_var_ui <- renderUI({
    req(dataset())
    cats <- names(dataset())[sapply(dataset(), function(x) is.factor(x) || is.character(x))]
    selectInput("cat_var", "Categorical Variable:", choices = cats)
  })
  
  output$cont_var_ui <- renderUI({
    req(dataset())
    conts <- names(dataset())[sapply(dataset(), is.numeric)]
    selectInput("cont_var", "Continuous Variable:", choices = conts)
  })
  
  output$catdiag_qq <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    rempsyc::nice_qq(data = dataset(), variable = input$cont_var, group = input$cat_var)
  })
  
  output$catdiag_density <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    rempsyc::nice_density(data = dataset(), variable = input$cont_var, group = input$cat_var)
  })
  
  output$catdiag_varplot <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    rempsyc::nice_varplot(data = dataset(), variable = input$cont_var, group = input$cat_var)
  })
  
  output$catdiag_outliers <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    rempsyc::plot_outliers(data = dataset(), response = input$cont_var, group = input$cat_var)
  })
  
  # Predicted Probabilities
  output$pred_plot <- renderPlot({
    req(model_fit(), input$model_type == "glm")
    fit <- model_fit()
    df <- fit$model
    df$predicted <- predict(fit, type = "response")
    df$actual <- df[[input$yvar]]
    ggplot(df, aes(x = predicted, fill = factor(actual))) +
      geom_histogram(position = "identity", bins = 30, alpha = 0.6) +
      scale_fill_manual(values = c("#E74C3C", "#2ECC71"), name = "Outcome") +
      theme_minimal() +
      labs(title = "Distribution of Predicted Probabilities", x = "Predicted Probability", y = "Count")
  })
  
  # Variable Importance
  output$vip_plot <- renderPlot({
    req(model_fit())
    vip::vip(model_fit()) + theme_minimal()
  })
  
  # Model Performance
  output$perf_metrics <- renderPrint({
    req(model_fit())
    performance::model_performance(model_fit())
  })
  
  output$perf_text <- renderUI({
    req(model_fit())
    fam <- tryCatch(model_fit()$family$family, error = function(e) NULL)
    is_logit <- !is.null(fam) && fam == "binomial"
    
    if (is_logit) {
      HTML("
    <b>Interpretation of logistic model performance metrics:</b><br><br>
    <ul>
      <li><b>AIC / BIC:</b> Penalized model fit — lower values indicate a better tradeoff between fit and complexity.</li>
      <li><b>Tjur’s R²:</b> Difference in mean predicted probabilities between outcome groups (discrimination measure).</li>
      <li><b>RMSE:</b> Root mean squared error between predicted probabilities and observed 0/1 outcomes.</li>
      <li><b>Sigma:</b> Fixed at 1 for logistic models (variance of logistic distribution).</li>
      <li><b>Log_loss:</b> Measures calibration; smaller = better probabilistic accuracy.</li>
      <li><b>Score_spherical:</b>	Calibration + confidence reward; higher = better.</li>
      <li><b>MAE:</b> Average absolute difference between observed and predicted probabilities (smaller = better).</li>
      <li><b>PCP:</b> Proportion correctly predicted using a cutoff (often 0.5).</li>
    </ul>
    <p><i>Summary:</i> For logistic regression, Tjur’s R² and PCP describe discrimination; Log_loss, RMSE, and MAE describe probability calibration.</p>
    ")
    } else {
      HTML("
    <b>Interpretation of linear model performance metrics:</b><br><br>
    <ul>
      <li><b>Adjusted R²:</b> Proportion of variance explained, adjusted for number of predictors.</li>
      <li><b>RMSE:</b> Root mean squared error — average deviation between predicted and observed values.</li>
      <li><b>MAE:</b> Mean absolute error — average absolute difference between observed and predicted values.</li>
      <li><b>Sigma:</b> Residual standard deviation — typical size of residuals in the outcome’s units.</li>
      <li><b>AIC / BIC:</b> Penalized fit measures — lower = better.</li>
    </ul>
    <p><i>Summary:</i> For linear models, Adjusted R², RMSE, MAE, and Sigma describe how closely predictions match actual values.</p>
    ")
    }
  })
  
  # GOF & Brier
  output$gof_metrics <- renderPrint({
    req(model_fit(), input$model_type == "glm")
    actual <- model_fit()$y
    ResourceSelection::hoslem.test(actual, fitted(model_fit()), g = 10)
  })
  
  output$brier_score <- renderPrint({
    req(model_fit(), input$model_type == "glm")
    cat("Brier Score:", round(DescTools::BrierScore(model_fit()), 4))
  })
  
  output$brier_text <- renderUI({
    HTML("
  <b>Interpretation of the Brier Score:</b><br><br>
  <ul>
    <li><b>Definition:</b> The Brier Score measures the mean squared difference between predicted probabilities and the actual 0/1 outcomes.</li>
    <li><b>Range:</b> 0 to 1. Lower is better — 0 indicates perfect probabilistic predictions.</li>
    <li><b>Typical Values:</b> Around 0.25 for random guessing (if outcome prevalence ≈ 0.5).</li>
    <li><b>Meaning:</b> It combines calibration (how close predicted probabilities are to true probabilities) and discrimination (how well predictions separate cases from non-cases).</li>
    <li><b>In practice:</b> Smaller values indicate more accurate, better-calibrated models.</li>
  </ul>
  <p><i>Summary:</i> The Brier Score is like an RMSE for predicted probabilities — lower means better overall probability accuracy.</p>
  ")
})

  
  # Basic Calibration Plot
  output$calibration_plot <- renderPlot({
    req(model_fit(), input$model_type == "glm")
    fit <- model_fit()
    predicted <- fitted(fit)
    actual <- fit$y
    df_cal <- data.frame(predicted = predicted, actual = actual)
    df_cal$bin <- cut(predicted, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
    df_summary <- df_cal %>%
      group_by(bin) %>%
      summarise(mean_pred = mean(predicted), obs_rate = mean(actual), n = n(), .groups = "drop")
    ggplot(df_summary, aes(x = mean_pred, y = obs_rate)) +
      geom_point(aes(size = n), color = "#2C3E50", alpha = 0.8) +
      geom_line(color = "#2C3E50") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      scale_size_continuous(range = c(3, 10)) +
      theme_minimal(base_size = 14) +
      labs(title = "Basic Calibration Plot",
           x = "Mean Predicted Probability",
           y = "Observed Event Rate")
  })
  
  # Bootstrapped Calibration Plot (rms) – FIXED
  output$boot_calibration_plot <- renderPlot({
    req(model_fit(), input$model_type == "glm")
    fit <- model_fit()
    df_model <- fit$model  # model frame with complete cases
    
    # Convert response to numeric 0/1 to satisfy lrm requirements
    response_var <- input$yvar
    df_model[[response_var]] <- as.numeric(df_model[[response_var]]) - 1
    
    # Set datadist on model data
    dd <<- datadist(df_model)
    options(datadist = "dd")
    
    # Refit lrm on model data with numeric response
    lrm_fit <- rms::lrm(formula(fit), data = df_model, x = TRUE, y = TRUE)
    
    # Bootstrapped calibration
    cal <- calibrate(lrm_fit, method = "boot", B = 200)
    
    plot(cal,
         xlab = "Predicted Probability",
         ylab = "Observed Proportion",
         main = "Bootstrapped Calibration Curve (rms)")
  })
  
  # ROC Curve
  output$roc_plot <- renderPlot({
    req(model_fit(), input$model_type == "glm")
    fit <- model_fit()
    actual <- fit$y
    roc_obj <- pROC::roc(actual, fitted(fit))
    plot(roc_obj, print.auc = TRUE, main = "ROC Curve")
  })
  
  # Marginal Effects – fixed type mismatch
  output$margins_plot <- renderPlot({
    req(model_fit(), input$model_type == "glm")
    df <- dataset()
    formula <- formula(model_fit())
    refit <- glm(formula, data = df, family = binomial)
    m <- margins(refit)
    plot(m)
  })
  
  output$margins_table <- renderTable({
    req(model_fit(), input$model_type == "glm")
    df <- dataset()
    formula <- formula(model_fit())
    refit <- glm(formula, data = df, family = binomial)
    summary(margins(refit))
  })
  
  # Interaction Plot
  output$interaction_plot <- renderPlot({
    req(model_fit())
    fit <- model_fit()
    interaction_input <- trimws(input$interaction_terms)
    if (!nzchar(interaction_input)) {
      plot.new()
      text(0.5, 0.5, "No interaction term specified.", cex = 1.3)
      return()
    }
    first_term <- trimws(strsplit(interaction_input, "\\+")[[1]][1])
    if (!grepl(":", first_term)) {
      plot.new()
      text(0.5, 0.5, "Invalid format. Use x1:x2", cex = 1.3)
      return()
    }
    terms <- trimws(strsplit(first_term, ":")[[1]])
    pred <- ggpredict(fit, terms = terms)
    plot(pred) + labs(title = paste("Interaction:", first_term))
  })
  
  # Coefficient Plots
  output$coef_plot <- renderPlot({
    req(model_fit())
    ggstats::ggcoef_model(model_fit(), exponentiate = (input$model_type == "glm"))
  })
  
  output$coef_table_plot <- renderPlot({
    req(model_fit())
    ggstats::ggcoef_table(model_fit(), exponentiate = (input$model_type == "glm"))
  })
  
  # Save functionality
  # ---- Folder selection ----
  # Save functionality – NOW FULLY FIXED
  roots <- c(Home = "~", Results = "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/Results")
  shinyDirChoose(input, "save_dir", roots = roots, session = session)
  save_path <- reactiveVal(NULL)
  
  observeEvent(input$save_dir, {
    req(input$save_dir)
    path <- parseDirPath(roots, input$save_dir)
    save_path(path)
  })
  
  observeEvent(input$save_outputs, {
    req(model_fit(), save_path())
    dir <- save_path()
    prefix <- input$save_prefix
    fit <- model_fit()
    
    # --- Regression Table ---
    reg_table <- tbl_regression(fit, exponentiate = (input$model_type == "glm")) %>%
      add_global_p() %>%
      as_flex_table()
    
    doc <- read_docx() %>%
      body_add_par("Regression Table", style = "heading 1") %>%
      body_add_flextable(reg_table)
    
    # --- Save coefficient plot ---
    g_coef <- ggstats::ggcoef_model(fit, exponentiate = (input$model_type == "glm"))
    ggsave(file.path(dir, paste0(prefix, "_coefplot.png")), g_coef,
           width = 9, height = 6, dpi = 300)
    
    # --- Save ROC plot if logistic ---
    if (input$model_type == "glm") {
      actual <- as.numeric(as.factor(fit$model[[input$yvar]])) - 1
      roc_obj <- pROC::roc(actual, fitted(fit))
      png(file.path(dir, paste0(prefix, "_roc.png")), width = 700, height = 600)
      plot(roc_obj, print.auc = TRUE)
      dev.off()
    }
    
    # --- Save interaction plot if specified ---
    if (nzchar(trimws(input$interaction_terms))) {
      first_term <- trimws(strsplit(input$interaction_terms, "\\+")[[1]][1])
      if (grepl(":", first_term)) {
        terms <- trimws(strsplit(first_term, ":")[[1]])
        pred <- ggpredict(fit, terms = terms)
        g_inter <- plot(pred) + labs(title = paste("Interaction:", first_term))
        ggsave(file.path(dir, paste0(prefix, "_interaction.png")), g_inter,
               width = 9, height = 6, dpi = 300)
      }
    }
    
    # --- Save Word doc with regression table only ---
    doc_path <- file.path(dir, paste0(prefix, "_report.docx"))
    print(doc, target = doc_path)
    
    output$save_msg <- renderText(paste("Regression table and plots saved to:\n", dir))
  })
  
}

shinyApp(ui, server)#