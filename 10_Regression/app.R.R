# REGRESSION DASHBOARD (UPDATED)

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

# ---- Helper function for consistent interpretation ----
interpret_effect <- function(factor, ame, pval, model_type) {
  if (model_type == "glm") {
    if (grepl("@", factor)) {
      paste0("Conditional effect of ", factor,
             ": predicted probability changes by ", ame,
             " (p = ", pval, ").")
    } else {
      paste0("Average effect of ", factor,
             ": predicted probability changes by ", ame,
             " (p = ", pval, ").")
    }
  } else { # linear model
    if (grepl("@", factor)) {
      paste0("Conditional effect of ", factor,
             ": outcome changes by ", ame,
             " units (p = ", pval, ").")
    } else {
      paste0("Average effect of ", factor,
             ": outcome changes by ", ame,
             " units (p = ", pval, ").")
    }
  }
}

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
      checkboxInput("center_numeric", "Mean‑center numeric predictors before interactions", FALSE),
      
      textInput("interaction_terms", "Specify interaction terms (e.g., x1:x2 + x3:x4)", ""),
      actionButton("fit_model", "Fit Model"),
      hr(),
      
      uiOutput("interaction_options"),
      
      
      h4("Download Outputs"),
      textInput("fname_base", "File name prefix:", "regression"),
      downloadButton("download_docx", "Download Word Report", class = "btn-primary"),
      downloadButton("download_zip", "Download Plots ZIP", class = "btn-success")
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
        tabPanel("Marginal Effects Table",
                 tableOutput("margins_table"),
                 uiOutput("margins_text")
        ),
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
  # Environment dataset dropdown
  dfs <- ls(envir = .GlobalEnv)
  is_df <- vapply(dfs, function(x) {
    obj <- tryCatch(get(x, envir = .GlobalEnv), error = function(e) NULL)
    is.data.frame(obj)
  }, logical(1))
  dfs <- dfs[is_df]
  updateSelectInput(session, "env_dataset", choices = c("None", dfs))
  
  dataset <- reactiveVal(NULL)
  
  # Load dataset
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
  
  # Preview
  output$data_preview_table <- renderTable({ req(dataset()); head(dataset(), 10) }, rownames = TRUE)
  output$data_summary <- renderPrint({ req(dataset()); summary(dataset()) })
  
  # Variable selectors
  output$yvar_ui <- renderUI({ req(dataset()); selectInput("yvar", "Response:", choices = names(dataset())) })
  output$xvars_ui <- renderUI({ req(dataset()); selectInput("xvars", "Predictors:", choices = names(dataset()), multiple = TRUE) })
  
  # Model fit
  model_fit <- eventReactive(input$fit_model, {
    req(dataset(), input$yvar, input$xvars)
    df <- dataset()
    xvars <- input$xvars
    interaction_terms <- input$interaction_terms
    
    if (input$center_numeric) {
      for (v in xvars) {
        if (is.numeric(df[[v]])) {
          centered_name <- paste0(v, "_c")
          df[[centered_name]] <- df[[v]] - mean(df[[v]], na.rm = TRUE)
          xvars[xvars == v] <- centered_name
          interaction_terms <- gsub(v, centered_name, interaction_terms)
        }
      }
    }
    
    yvar <- paste0("`", input$yvar, "`")
    xvars_text <- paste0("`", xvars, "`", collapse = " + ")
    formula_text <- paste(yvar, "~", xvars_text)
    
    if (nzchar(interaction_terms)) {
      interactions <- strsplit(interaction_terms, "\\+")[[1]]
      interactions <- trimws(interactions)
      interactions_bt <- sapply(interactions, function(term) {
        parts <- strsplit(term, ":")[[1]]
        paste0(paste0("`", trimws(parts), "`", collapse = ":"))
      })
      formula_text <- paste(formula_text, "+", paste(interactions_bt, collapse = " + "))
    }
    
    formula <- as.formula(formula_text)
    
    if (input$model_type == "lm") {
      lm(formula, data = df)
    } else {
      glm(formula, data = df, family = binomial)
    }
  })
  
  # ---- Model summary ----
  output$model_summary <- renderPrint({ req(model_fit()); summary(model_fit()) })
  
  # ---- Model commentary (patched) ----
  output$model_commentary <- renderText({
    req(model_fit())
    fit <- model_fit()
    coef_sum <- summary(fit)$coefficients
    coef_df <- as.data.frame(coef_sum)
    coef_df$term <- rownames(coef_sum)
    rownames(coef_df) <- NULL
    coef_df <- coef_df[!coef_df$term %in% c("(Intercept)", "Intercept"), ]
    
    pcol <- if (input$model_type == "glm") "Pr(>|z|)" else "Pr(>|t|)"
    sig_df <- coef_df[coef_df[[pcol]] < 0.05, ]
    
    if (nrow(sig_df) == 0) return("No significant associations found (p < 0.05).")
    
    comments <- sapply(1:nrow(sig_df), function(i) {
      term <- sig_df$term[i]
      est <- round(sig_df$Estimate[i], 3)
      pval <- format.pval(sig_df[[pcol]][i], digits = 3, eps = 0.001)
      if (input$model_type == "glm") {
        or <- round(exp(est), 2)
        direction <- ifelse(est > 0, "increases odds", "decreases odds")
        paste0(term, " ", direction, " (OR = ", or, ", p = ", pval, ")")
      } else {
        direction <- ifelse(est > 0, "positively", "negatively")
        paste0(term, " is ", direction, " associated with the outcome ",
               "(β = ", est, ", p = ", pval, ")")
      }
    })
    
    paste(comments, collapse = ". ")
  })
  
  # ---- Regression Table ----
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
      mutate(.cooksd = as.numeric(.cooksd),
             .hat = as.numeric(.hat)) %>%
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
  
  
  output$interaction_options <- renderUI({
    req(model_fit())
    
    fit <- model_fit()
    vars <- attr(terms(fit), "term.labels")
    inter <- grep(":", vars, value = TRUE)
    
    if (length(inter) == 0) return(NULL)
    
    v <- strsplit(inter[1], ":")[[1]]
    is_factor <- sapply(v, function(x) is.factor(fit$model[[x]]))
    is_numeric <- sapply(v, function(x) is.numeric(fit$model[[x]]))
    
    if (all(is_factor)) {
      radioButtons(
        "catcat_estimand",
        "Categorical × Categorical output",
        choices = c(
          "Cell means" = "means",
          "Pairwise contrasts" = "contrasts"
        ),
        inline = TRUE
      )
    } else if (sum(is_factor) == 1 && sum(is_numeric) == 1) {
      radioButtons(
        "catcont_estimand",
        "Categorical × Continuous output",
        choices = c(
          "Slopes (marginal effects)" = "slopes",
          "Compare slopes between groups" = "comparisons"
        ),
        inline = TRUE
      )
    }
  })
  
  # ---- Marginal Effects Table (patched) ----
  # Marginal Effects – works for lm and glm
  output$margins_plot <- renderPlot({
    req(model_fit())
    fit <- model_fit()
    m <- tryCatch(margins(fit, data = fit$model), error = function(e) NULL)
    
    if (!is.null(m)) {
      plot(m)
      title(main = "Average Marginal Effects")
    } else {
      plot.new()
      text(0.5, 0.5, "No marginal effects available.", cex = 1.3, col = "red")
    }
  })
  
  # margins table
  output$margins_table <- renderTable({
    
    req(model_fit())
    fit <- model_fit()
    
    # 🔒 Force reactivity to model structure
    isolate({
      formula(fit)
    })
    
    # 🔥 Always start clean
    df <- NULL
    
    
    vars_in_model <- attr(terms(fit), "term.labels")
    interaction_terms <- grep(":", vars_in_model, value = TRUE)
    
   
    if (length(interaction_terms) > 0) {
      
      inter <- interaction_terms[1]
      vars  <- strsplit(inter, ":")[[1]]
      
      is_factor <- sapply(vars, function(v) is.factor(fit$model[[v]]))
      is_numeric <- sapply(vars, function(v) is.numeric(fit$model[[v]]))
      
      # --------------------------------------------------
      # 1️⃣ Categorical × Categorical
      # --------------------------------------------------
      if (all(is_factor)) {
        
        choice <- input$catcat_estimand %||% "means"
        
        emm <- emmeans::emmeans(
          fit,
          as.formula(paste("~", vars[1], "*", vars[2]))
        )
        
        if (choice == "contrasts") {
          
          df <- emmeans::contrast(
            emm,
            method = "pairwise"
          ) |>
            summary(infer = TRUE) |>
            as.data.frame()
          
          df$Type <- "Pairwise contrasts (Cat × Cat)"
          
        } else {
          
          df <- summary(emm, infer = TRUE) |>
            as.data.frame()
          
          df$Type <- "Estimated marginal means (cell means)"
        }
        
        # --------------------------------------------------
        # 2️⃣ Categorical × Continuous
        # --------------------------------------------------
      } else if (sum(is_factor) == 1 && sum(is_numeric) == 1) {
        
        cont_var <- vars[is_numeric]
        cat_var  <- vars[is_factor]
        
        choice <- input$catcont_estimand %||% "slopes"
        
        if (choice == "comparisons") {
          
          tr <- emmeans::emtrends(
            fit,
            specs = as.formula(paste("~", cat_var)),
            var = cont_var
          )
          
          df <- emmeans::contrast(
            tr,
            method = "pairwise"
          ) |>
            summary(infer = TRUE) |>
            as.data.frame()
          
          df$Type <- "Slope comparisons (Cat × Cont)"
          
        } else {
          
          df <- marginaleffects::slopes(
            fit,
            variables = cont_var,
            by = cat_var
          ) |>
            as.data.frame()
          
          df$Type <- "Marginal slopes by group (Cat × Cont)"
        }
        
        # --------------------------------------------------
        # 3️⃣ Continuous × Continuous
        # --------------------------------------------------
      } else if (all(is_numeric)) {
        
        df <- marginaleffects::slopes(
          fit,
          variables = vars
        ) |>
          as.data.frame()
        
        df$Type <- "Marginal effects (Cont × Cont)"
      }
    }
    
 

    # --------------------------------------------------
    # 4️⃣ No interaction → AMEs for continuous + factors
    # --------------------------------------------------
    if (length(interaction_terms) == 0) {
      
      outcome <- all.vars(formula(fit))[1]
      
      num_vars <- names(fit$model)[
        sapply(fit$model, is.numeric)
      ]
      num_vars <- setdiff(num_vars, outcome)
      
      fac_vars <- names(fit$model)[
        sapply(fit$model, is.factor)
      ]
      
      dfs <- list()
      
      # ---- Continuous predictors ----
      if (length(num_vars) > 0) {
        dfs[["continuous"]] <-
          marginaleffects::avg_slopes(
            fit,
            variables = num_vars
          ) |>
          as.data.frame()
      }
      
      # ---- Factor predictors ----
      if (length(fac_vars) > 0) {
        dfs[["factors"]] <-
          marginaleffects::avg_comparisons(
            fit,
            variables = fac_vars
          ) |>
          as.data.frame()
      }
      
      if (length(dfs) == 0) {
        return(data.frame(Message = "No marginal effects available."))
      }
      
      df <- dplyr::bind_rows(dfs)
      
      df$Type <- "Average marginal effects (main effects)"
    }
    
    
    # --------------------------------------------------
    # 🔧 Normalize columns
    # --------------------------------------------------
    if ("estimate" %in% names(df)) df$AME <- df$estimate
    if ("emmean" %in% names(df))   df$AME <- df$emmean
    if ("p.value" %in% names(df))  df$p   <- df$p.value
    
    # --------------------------------------------------
    # 🧠 Interpretation
    # --------------------------------------------------
    df$Interpretation <- paste0(
      "Estimate = ",
      ifelse(!is.na(df$AME), round(df$AME, 3), "NA"),
      ", p = ",
      ifelse(!is.na(df$p), signif(df$p, 3), "NA")
    )
    
    df
  })
  
  # Interaction Plot
  output$interaction_plot <- renderPlot({
    req(model_fit())
    fit <- model_fit()
    interaction_input <- trimws(input$interaction_terms)
    if (!nzchar(interaction_input)) {
      plot.new(); text(0.5, 0.5, "No interaction term specified.", cex = 1.3); return()
    }
    first_term <- trimws(strsplit(interaction_input, "\\+")[[1]][1])
    if (!grepl(":", first_term)) {
      plot.new(); text(0.5, 0.5, "Invalid format. Use x1:x2", cex = 1.3); return()
    }
    terms <- trimws(strsplit(first_term, ":")[[1]])
    
    # If centering was applied, swap to centered names
    if (input$center_numeric) {
      terms <- sapply(terms, function(t) {
        if (t %in% names(fit$model) && grepl("_c$", t)) t else if (paste0(t, "_c") %in% names(fit$model)) paste0(t, "_c") else t
      })
    }
    
    pred <- ggpredict(fit, terms = terms)
    plot(pred) + labs(title = paste("Interaction:", paste(terms, collapse = ":")))
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
  
  # ---- Word Export (patched) ----
  output$download_docx <- downloadHandler(
    filename = function() { paste0(input$fname_base, "_report.docx") },
    content = function(file) {
      req(model_fit())
      fit <- model_fit()
      
      reg_table <- tbl_regression(fit, exponentiate = (input$model_type == "glm")) %>%
        add_global_p() %>%
        as_flex_table()
      
      vars_in_model <- attr(terms(fit), "term.labels")
      interaction_terms <- grep(":", vars_in_model, value = TRUE)
      
      if (length(interaction_terms) > 0) {
        first_term <- interaction_terms[1]
        parts <- strsplit(first_term, ":")[[1]]
        cont_var <- parts[which(sapply(parts, function(v) is.numeric(fit$model[[v]])))]
        cat_var  <- parts[which(sapply(parts, function(v) is.factor(fit$model[[v]])))]
        at_list <- setNames(list(levels(fit$model[[cat_var]])), cat_var)
        cond_sum <- summary(margins(fit, variables = cont_var, at = at_list, data = fit$model))
        ame_sum <- summary(margins(fit, data = fit$model))
        other_vars <- setdiff(ame_sum$factor, cond_sum$factor)
        ame_sum <- ame_sum[ame_sum$factor %in% other_vars, ]
        m_sum <- dplyr::bind_rows(cond_sum, ame_sum)
      } else {
        m_sum <- summary(margins(fit, data = fit$model))
      }
      
      m_sum$Interpretation <- apply(m_sum, 1, function(row) {
        interpret_effect(row["factor"], round(as.numeric(row["AME"]), 3),
                         signif(as.numeric(row["p"]), 3), input$model_type)
      })
      
      margins_table <- flextable::qflextable(m_sum)
      
      doc <- read_docx() %>%
        body_add_par("Regression Table", style = "heading 1") %>%
        body_add_flextable(reg_table) %>%
        body_add_par("Marginal Effects Table", style = "heading 1") %>%
        body_add_flextable(margins_table)
      
      print(doc, target = file)
    }
  )
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0(input$fname_base, "_plots.zip")
    },
    content = function(file) {
      
      req(model_fit())
      
      fit <- model_fit()
      tmpdir <- tempdir()
      files <- c()
      
      # ---- helper: safely create plots ----
      safe_plot <- function(expr, path) {
        tryCatch({
          expr
          if (file.exists(path)) path else NULL
        }, error = function(e) {
          message("Plot failed: ", e$message)
          NULL
        })
      }
      
      # ---- Coefficient plot (ggplot) ----
      coef_path <- file.path(tmpdir, paste0(input$fname_base, "_coefplot.png"))
      g_coef <- ggstats::ggcoef_model(
        fit,
        exponentiate = (input$model_type == "glm")
      )
      
      files <- c(
        files,
        safe_plot(
          ggsave(coef_path, g_coef, width = 9, height = 6, dpi = 300),
          coef_path
        )
      )
      
      # ---- ROC plot (base R) ----
      if (input$model_type == "glm") {
        roc_path <- file.path(tmpdir, paste0(input$fname_base, "_roc.png"))
        
        files <- c(
          files,
          safe_plot({
            actual <- as.numeric(as.factor(fit$model[[input$yvar]])) - 1
            roc_obj <- pROC::roc(actual, fitted(fit))
            png(roc_path, width = 700, height = 600)
            plot(roc_obj, print.auc = TRUE)
            dev.off()
          }, roc_path)
        )
      }
      
      # ---- Interaction plot (ggpredict) ----
      if (nzchar(trimws(input$interaction_terms))) {
        first_term <- trimws(strsplit(input$interaction_terms, "\\+")[[1]][1])
        if (grepl(":", first_term)) {
          
          inter_path <- file.path(tmpdir, paste0(input$fname_base, "_interaction.png"))
          
          files <- c(
            files,
            safe_plot({
              terms <- trimws(strsplit(first_term, ":")[[1]])
              pred <- ggpredict(fit, terms = terms)
              g_inter <- plot(pred) +
                labs(title = paste("Interaction:", first_term))
              ggsave(inter_path, g_inter, width = 9, height = 6, dpi = 300)
            }, inter_path)
          )
        }
      }
      
      # ---- Marginal effects plot (base R) ----
      margins_path <- file.path(tmpdir, paste0(input$fname_base, "_margins.png"))
      m <- tryCatch(margins(fit, data = fit$model), error = function(e) NULL)
      
      files <- c(
        files,
        safe_plot({
          png(margins_path, width = 900, height = 600)
          if (!is.null(m)) {
            plot(m)
            title(main = "Marginal Effects")
          } else {
            plot.new()
            text(0.5, 0.5, "No marginal effects available.", cex = 1.3)
          }
          dev.off()
        }, margins_path)
      )
      # ---- clean ZIP structure ----
      
      zip_dir <- file.path(tmpdir, "zip_staging")
      dir.create(zip_dir, showWarnings = FALSE)
      
      # copy plots into staging dir
      for (f in files) {
        file.copy(f, file.path(zip_dir, basename(f)), overwrite = TRUE)
      }
      
      zip_files <- list.files(zip_dir, full.names = FALSE)
      
      if (length(zip_files) == 0) {
        stop("No plots available to zip")
      }
      
      old_wd <- setwd(zip_dir)
      on.exit(setwd(old_wd), add = TRUE)
      
      utils::zip(
        zipfile = normalizePath(file, mustWork = FALSE),
        files   = zip_files
      )
    }
  )
}
shinyApp(ui, server)