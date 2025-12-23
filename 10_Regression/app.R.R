# https://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#linearity-assumption

# ============================================================
# Regression Dashboard with DFBETAs Option (Stata .dta upload added)
# + Basic Diagnostic Plots + DFBETAs Plot + Automatic Commentary
# ============================================================

library(shiny)
library(shinyFiles)
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
library(readr)
library(ResourceSelection)
library(haven)
library(api2lm) # for dfbetas_plot

ui <- fluidPage(
  titlePanel("Regression Dashboard with DFBETAs Option"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Load Dataset"),
      # accept .dta in the UI
      fileInput("file", "Upload Excel or STATA or RData", accept = c(".xlsx", ".xls",'.csv', '.RData', '.dta')),
      selectInput("env_dataset", "Or select dataset from R environment:",
                  choices = c("None", ls(.GlobalEnv)), selected = "None"),
            actionButton("load_env", "Load Dataset"),
      hr(),
      radioButtons("model_type", "Model type:", choices = c("Linear" = "lm", "Logistic" = "glm")),
      uiOutput("yvar_ui"),
      uiOutput("xvars_ui"),
      textInput("interaction_terms", "Specify interaction terms (e.g., x1:x2 + x3:x4)", ""),
      actionButton("fit_model", "Fit Model"),
      hr(),
      
      h4("💾 Save Outputs"),
      shinyDirButton("save_dir", "Choose Folder", "Select folder to save outputs"),
      textInput("save_prefix", "Prefix for saved files:", "regression"),
      actionButton("save_outputs", "Save All Outputs"),
      textOutput("save_msg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary", verbatimTextOutput("model_summary")),
        tabPanel("Model Commentary", textOutput("model_commentary")),
        
        tabPanel("Diagnostics Continuous",
                              radioButtons("diag_option", "Select diagnostic view:",
                              choices = c("Base Plots", "DFBETAs Plot")),
                 conditionalPanel(
                   condition = "input.diag_option == 'DFBETAs Plot'",
                   numericInput("id_n", "Number of labeled points:", 4, min = 1, max = 20)
                 ),
                 plotOutput("diagnostic_plot"),
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
                 plotOutput("catdiag_outliers"),
                 hr(),
 
        ),
        
        tabPanel("Predicted Probabilities", plotOutput("pred_plot")),
        tabPanel("Regression Table", tableOutput("reg_table")),
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
                 plotOutput("calibration_plot"),
                 plotOutput("boot_calibration_plot")),
        tabPanel("ROC Curve", plotOutput("roc_plot")),
        tabPanel("Marginal Effects", plotOutput("margins_plot")),
        tabPanel("Marginal Effects Table", tableOutput("margins_table")),
        tabPanel("Interaction Plot", plotOutput("interaction_plot")),
        tabPanel("Coefficient Plot", plotOutput("coef_plot")),
        tabPanel("Coefficient Table (ggstats)", plotOutput("coef_table_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # ---- Dataset ----
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    # normalize extension to lower case
    ext <- tolower(tools::file_ext(input$file$name))
    
    df <- tryCatch({
      switch(ext,
             csv   = read.csv(input$file$datapath, stringsAsFactors = FALSE),
             rdata = {
               e <- new.env()
               load(input$file$datapath, envir = e)
               # get first object (like your original)
               get(ls(e)[1], envir = e)
             },
             dta   = {
               # read Stata and convert labelled variables to factors
               haven::read_dta(input$file$datapath) %>%
                 dplyr::mutate(dplyr::across(where(haven::is.labelled), haven::as_factor)) %>%
                 as.data.frame()
             },
             stop("Unsupported file type. Please upload a .csv, .RData, or .dta file.")
      )
    }, error = function(e){
      showNotification(paste0("Error reading file: ", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(df)) {
      # ensure data.frame
      if (!is.data.frame(df)) {
        showNotification("Uploaded object is not a data frame.", type = "error")
      } else {
        dataset(df)
      }
    }
  })
  
  observeEvent(input$load_env, {
    req(input$env_dataset)
    df <- get(input$env_dataset, envir = .GlobalEnv)
    if (!is.data.frame(df)) showNotification("Selected object is not a data frame", type = "error")
    else dataset(df)
  })
  
  output$yvar_ui <- renderUI({ req(dataset()); selectInput("yvar", "Select response variable:", choices = names(dataset())) })
  output$xvars_ui <- renderUI({ req(dataset()); selectInput("xvars", "Select predictor(s):", choices = names(dataset()), multiple = TRUE) })
  
  # ---- Model fit ----
  model_fit <- eventReactive(input$fit_model, {
    req(dataset(), input$yvar, input$xvars)
    df <- dataset()
    formula_text <- paste(input$yvar, "~", paste(input$xvars, collapse = " + "))
    if (nzchar(input$interaction_terms)) formula_text <- paste(formula_text, "+", input$interaction_terms)
    formula <- as.formula(formula_text)
    if (input$model_type == "lm") lm(formula, data = df) else glm(formula, data = df, family = binomial)
  })
  
  # ---- Model Summary ----
  output$model_summary <- renderPrint({
    req(model_fit())
    summary(model_fit())
  })
  
  # ---- Model Commentary ----
  output$model_commentary <- renderText({
    req(model_fit())
    fit <- model_fit()
    coef_sum <- summary(fit)$coefficients
    colnames(coef_sum) <- c("Estimate", "StdError", "StatValue", "PValue")
    coef_df <- as.data.frame(coef_sum)
    coef_df$term <- rownames(coef_sum)
    rownames(coef_df) <- NULL
    coef_df <- coef_df[!coef_df$term %in% c("(Intercept)", "Intercept"), ]
    sig_df <- coef_df[coef_df$PValue < 0.05, ]
    if (nrow(sig_df) == 0) return("No significant associations found (p < 0.05).")
    comments <- sapply(1:nrow(sig_df), function(i) {
      term <- sig_df$term[i]
      est <- round(sig_df$Estimate[i], 3)
      pval <- round(sig_df$PValue[i], 3)
      if (input$model_type == "glm") {
        direction <- ifelse(est > 0, "increases odds", "decreases odds")
        est_text <- round(exp(est), 2)
        paste0(term, " ", direction, " (OR = ", est_text, ", p = ", pval, ")")
      } else {
        direction <- ifelse(est > 0, "positively", "negatively")
        paste0(term, " is ", direction, " associated (Beta = ", est, ", p = ", pval, ")")
      }
    })
    paste(comments, collapse = ". ")
  })
  
  # ---- Diagnostics (Base vs DFBETAs) ----
  output$diagnostic_plot <- renderPlot({
    req(model_fit())
    fit <- model_fit()
    
    # Detect logistic regression
    fam <- tryCatch(fit$family$family, error = function(e) NULL)
    is_logit <- !is.null(fam) && fam == "binomial"
    
    # ============================
    # 1. BASE DIAGNOSTICS OPTION
    # ============================
    if (input$diag_option == "Base Plots") {
      
      # -----------------------------------------
      # LOGISTIC REGRESSION → STHDA LINEARITY IN LOGIT
      # -----------------------------------------
      if (is_logit) {
        df_raw <- dataset()
        
        # Predicted probabilities
        probs <- predict(fit, type = "response")
        probs <- pmin(pmax(probs, 1e-8), 1 - 1e-8)  # avoid infinite logits
        
        # Numeric predictors only
        num_df <- df_raw %>% dplyr::select(where(is.numeric))
        
        if (ncol(num_df) == 0) {
          plot.new()
          title("No numeric predictors for linearity-in-the-logit check")
          return(NULL)
        }
        
        # Build plot DF
        plot_df <- num_df %>%
          mutate(logit = log(probs / (1 - probs))) %>%
          tidyr::pivot_longer(
            cols = -logit,
            names_to = "predictor",
            values_to = "value"
          )
        
        # STHDA-style plot
        ggplot(plot_df, aes(x = logit, y = value)) +
          geom_point(size = 0.5, alpha = 0.5) +
          geom_smooth(method = "loess", se = TRUE, color = "blue") +
          theme_bw() +
          facet_wrap(~ predictor, scales = "free_y") +
          labs(
            title = "Linearity in the Logit (Logistic Regression Only)",
            x = "logit(predicted probability)",
            y = "Predictor Value"
          )
        
      } else {
        # -----------------------------------------
        # LINEAR REGRESSION → STANDARD DIAGNOSTICS
        # -----------------------------------------
        par(mfrow = c(3, 2))
        plot(fit, which = 1:5)
      }
    }
    
    # ============================
    # 2. DFBETAs PLOT OPTION
    # ============================
    else {
      if (is_logit) {
        plot.new()
        title("DFBETAs plot not available for logistic regression", 
              col.main = "red", cex.main = 1.3)
      } else {
        api2lm::dfbetas_plot(fit, id_n = input$id_n)
      }
    }
  })
  1
      
    # ---- Influential Cases ----
  influential_cases <- eventReactive(input$show_influential, {
    req(model_fit(), dataset())
    fit_obj <- model_fit()
    df <- dataset() %>% mutate(.orig_row = seq_len(n()))
    aug <- broom::augment(fit_obj)
    # map model rownames to original data rows
    used_rows <- as.integer(rownames(fit_obj$model))
    if (length(used_rows) == 0) used_rows <- seq_len(nrow(fit_obj$model))
    aug$orig_row <- NA_integer_
    valid_idx <- which(!is.na(used_rows) & used_rows <= nrow(df))
    if (length(valid_idx) > 0) aug$orig_row[valid_idx] <- df$.orig_row[used_rows[valid_idx]]
    predictors <- names(fit_obj$model)[-1]
    diag_table <- aug %>%
      select(orig_row, .cooksd, .hat, all_of(predictors)) %>%
      as.data.frame()
    top_cooks <- diag_table %>% slice_max(.cooksd, n = 5)
    top_leverage <- diag_table %>% slice_max(.hat, n = 5)
    bind_rows(top_cooks, top_leverage) %>%
      distinct(orig_row, .keep_all = TRUE) %>%
      arrange(desc(.cooksd), desc(.hat))
  })
  
  output$influential_cases <- renderTable({ req(influential_cases()); influential_cases() })
  
  # ---- Categorical Diagnostics UI ----
  output$cat_var_ui <- renderUI({
    req(dataset())
    df <- dataset()
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    selectInput("cat_var", "Categorical Variable:", choices = cats)
  })
  
  output$cont_var_ui <- renderUI({
    req(dataset())
    df <- dataset()
    conts <- names(df)[sapply(df, is.numeric)]
    selectInput("cont_var", "Continuous Variable:", choices = conts)
  })
  
  # ---- QQ Plot ----
  output$catdiag_qq <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    df <- dataset()
    rempsyc::nice_qq(
      data = df,
      variable = input$cont_var,
      group = input$cat_var
    )
  })
  
  # ---- Density Plot ----
  output$catdiag_density <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    df <- dataset()
    rempsyc::nice_density(
      data = df,
      variable = input$cont_var,
      group = input$cat_var,
      colours = c("#00BA38", "#619CFF", "#F8766D"), 
      xtitle = "cont_var",  ytitle = "Density (vs. Normal Distribution)",  
      shapiro = TRUE,  histogram = TRUE)
   
  })
  
  # ---- Variance / Distribution Plot ----
  output$catdiag_varplot <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    df <- dataset()
    rempsyc::nice_varplot(
      data = df,
      variable = input$cont_var,
      group = input$cat_var
    )
  })
  
  # ---- Outlier Plot ----
   output$catdiag_outliers <- renderPlot({
    req(dataset(), input$cat_var, input$cont_var)
    df <- dataset()
    rempsyc::plot_outliers(
      data = df,
      response = input$cont_var,
      group = input$cat_var
    )
  })

  # ---- Regression Table ----
  output$reg_table <- renderTable({ 
    req(model_fit())
    tbl_regression(model_fit(), exponentiate = (input$model_type=="glm")) %>% as.data.frame() 
  })
  
  # ---- Variable Importance ----
  output$vip_plot <- renderPlot({ req(model_fit()); vip::vip(model_fit()) })
  
  # ---- Predicted Probabilities ----
  output$pred_plot <- renderPlot({
    req(model_fit(), input$model_type == "glm")
    fit_obj <- model_fit()
    df <- fit_obj$model
    
    # get predicted probabilities
    df$predicted <- predict(fit_obj, type = "response")
    
    # actual outcome (ensure 0/1 numeric)
    df$actual <- as.numeric(as.factor(df[[input$yvar]])) - 1
    
    ggplot(df, aes(x = predicted, fill = factor(actual))) +
      geom_histogram(position = "identity", bins = 30, alpha = 0.5) +
      scale_fill_manual(values = c("#E74C3C", "#2ECC71"),
                        name = "Observed Outcome",
                        labels = c("0", "1")) +
      theme_minimal(base_size = 13) +
      labs(title = "Distribution of Predicted Probabilities by Outcome",
           x = "Predicted Probability",
           y = "Count") +
      theme(legend.position = "top")
  })
  # ---- Model Performance ----
  output$perf_metrics <- renderPrint({
    req(model_fit())
    perf <- performance::model_performance(model_fit())
    
    # Compute MAE manually (works for both lm and glm)
    pred <- predict(model_fit(), type = "response")
    obs <- model_fit()$model[[1]]
    
    # convert factor to numeric 0/1 if logistic
    fam <- tryCatch(model_fit()$family$family, error = function(e) NULL)
    if (!is.null(fam) && fam == "binomial") {
      if (is.factor(obs)) obs <- as.numeric(obs) - 1
    }
    
    mae_val <- mean(abs(pred - obs), na.rm = TRUE)
    
    perf$MAE <- mae_val
    perf
  })
  
  # ---- Adaptive Explanation ----
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
  

  # ---- Calibration, ROC, Marginal Effects, etc. ----
  output$gof_metrics <- renderPrint({
    req(model_fit(), input$model_type=="glm")
    actual <- as.numeric(as.factor(model_fit()$model[[input$yvar]])) - 1
    ResourceSelection::hoslem.test(actual, fitted(model_fit()), g=10)
  })
  
  output$brier_score <- renderPrint({
    req(model_fit(), input$model_type=="glm")
    score <- BrierScore(model_fit())
    cat("Brier Score:", round(score,4))
  })
  
  # ---- Brier Score Interpretation ----
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
  
  output$calibration_plot <- renderPlot({
    req(model_fit(), input$model_type=="glm")
    fit_obj <- model_fit()
    actual <- as.numeric(as.factor(fit_obj$model[[input$yvar]])) - 1
    predicted <- fitted(fit_obj)
    bins <- cut(predicted, breaks=seq(0,1,0.1), include.lowest=TRUE)
    df_cal <- data.frame(actual=actual, predicted=predicted, bin=bins)
    df_cal %>%
      group_by(bin) %>%
      summarise(mean_pred=mean(predicted), obs_rate=mean(actual)) %>%
      ggplot(aes(mean_pred, obs_rate)) +
      geom_point(size=3,color="#2C3E50") +
      geom_abline(slope=1,intercept=0, linetype="dashed", color="gray") +
      theme_minimal() +
      labs(title="Calibration Plot", x="Mean Predicted", y="Observed Proportion")
  })
  
  output$boot_calibration_plot <- renderPlot({
    req(model_fit(), input$model_type=="glm")
    fit_obj <- model_fit()
    dd <<- datadist(fit_obj$model)
    options(datadist = "dd")
    lrm_fit <- rms::lrm(formula(fit_obj), data=fit_obj$model, x=TRUE, y=TRUE)
    cal <- calibrate(lrm_fit, method="boot", B=100)
    plot(cal, xlab="Predicted Probability", ylab="Observed Proportion", main="Bootstrapped Calibration")
  })
  
  output$roc_plot <- renderPlot({
    req(model_fit(), input$model_type=="glm")
    actual <- as.numeric(as.factor(model_fit()$model[[input$yvar]]))-1
    roc_obj <- pROC::roc(actual, fitted(model_fit()))
    plot(roc_obj, print.auc=TRUE)
  })
  
  output$margins_plot <- renderPlot({
    req(model_fit(), input$model_type=="glm")
    refit <- glm(formula(model_fit()), data=model_fit()$model, family=binomial)
    plot(margins::margins(refit))
  })
  
  output$margins_table <- renderTable({
    req(model_fit(), input$model_type=="glm")
    refit <- glm(formula(model_fit()), data=model_fit()$model, family=binomial)
    summary(margins(refit))
  })
  
  output$interaction_plot <- renderPlot({
    req(model_fit(), input$xvars, length(input$xvars)>=2)
    fit_obj <- model_fit()
    terms_str <- c(input$xvars[1], input$xvars[2])
    pred <- ggpredict(fit_obj, terms = terms_str)
    plot(pred)
  })
  
  output$coef_plot <- renderPlot({
    req(model_fit())
    ggstats::ggcoef_model(model_fit(), exponentiate = (input$model_type=="glm"),
                          show_p_values = TRUE, signif_stars = TRUE) +
      theme_minimal(base_size=13) +
      labs(title="Coefficient Plot", x="Effect Size (Exponentiated if Logistic)", y="")
  })
  
  output$coef_table_plot <- renderPlot({
    req(model_fit())
    ggstats::ggcoef_table(model_fit(), exponentiate = (input$model_type=="glm"),
                          signif_stars=TRUE, conf.level=0.95, show_p_values=TRUE) +
      theme(panel.grid.major.y=element_blank(),
            panel.grid.major.x=element_line(color="gray85", linetype="dotted"),
            axis.title.y=element_blank(),
            legend.position="none") +
      labs(title="Model Coefficients with 95% CI and p-values",
           x = ifelse(input$model_type=="glm", "Odds Ratio", "Beta"))
  })
  
  # ---- Folder selection ----
  roots <- c(
    Home = normalizePath("~"),
    Results = "G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/Results"
  )
  
  shinyDirChoose(input, "save_dir", roots = roots, session = session)
  save_path <- reactiveVal()
  
  observeEvent(input$save_dir, {
    req(input$save_dir)
    path <- parseDirPath(roots, input$save_dir)
    save_path(path)
    showNotification(paste("Saving outputs to:", path), type = "message")
  })
  # ---- Save Outputs ----
  observeEvent(input$save_outputs, {
    req(model_fit(), save_path())
    dir <- save_path()
    prefix <- input$save_prefix
    fit <- model_fit()
    
    # Build file paths
    doc_path <- file.path(dir, paste0(prefix, "_summary.docx"))
    coef_path <- file.path(dir, paste0(prefix, "_coefplot.png"))
    roc_path <- file.path(dir, paste0(prefix, "_roc.png"))
    
    # Create Word doc
    doc <- read_docx() |>
      body_add_par("Model Summary", style = "heading 1") |>
      body_add_par(capture.output(summary(fit)), style = "Normal") |>
      body_add_par("Model Commentary", style = "heading 1") |>
      body_add_par(output$model_commentary(), style = "Normal")
    
    print(doc, target = doc_path)
    
    # Save Coefficient Plot
    g_coef <- ggstats::ggcoef_model(
      fit,
      exponentiate = (input$model_type == "glm"),
      show_p_values = TRUE,
      signif_stars = TRUE
    ) +
      theme_minimal(base_size = 13) +
      labs(title = "Coefficient Plot",
           x = ifelse(input$model_type == "glm", "Odds Ratio", "Beta"))
    
    ggsave(coef_path, g_coef, width = 7, height = 5, dpi = 300)
    
    # Save ROC Plot if logistic
    if (input$model_type == "glm") {
      actual <- as.numeric(as.factor(fit$model[[input$yvar]])) - 1
      roc_obj <- pROC::roc(actual, fitted(fit))
      png(roc_path, width = 700, height = 600)
      plot(roc_obj, print.auc = TRUE, main = "ROC Curve")
      dev.off()
    }
    
    output$save_msg <- renderText({
      paste("✅ Outputs saved to:", dir)
    })
  })
  
}

shinyApp(ui, server)
