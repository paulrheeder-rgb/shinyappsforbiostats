# ============================================================
# LINEAR MIXED MODELS – EXPLICIT RANDOM EFFECTS APP (ENV + TRANSFORMATIONS)
# ============================================================

library(shiny)
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(ggeffects)
library(emmeans)
library(broom.mixed)
library(readxl)
library(haven)
library(officer)
library(flextable)
conflicts_prefer(lmerTest::lmer)

ui <- fluidPage(
  titlePanel("Linear Mixed-Effects Model Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Load dataset"),
      fileInput("file", "Upload CSV / Excel / Stata / RData", 
                accept = c(".csv", ".xlsx", ".xls", ".dta", ".RData", ".rda")),
      selectInput("env_dataset", "Or select dataset from R environment:", choices = "None"),
      actionButton("load_env", "Load from environment"),
      hr(),
      
      uiOutput("y_ui"),
      uiOutput("x_ui"),
      
      radioButtons("model_scope", "Model type",
                   choices = c("Univariate (one predictor at a time)" = "uni",
                               "Multivariate (all predictors together)" = "multi"),
                   selected = "multi"),
      
      h4("Transform predictors"),
      uiOutput("center_ui"),
      uiOutput("scale_ui"),
      
      hr(),
      
      checkboxInput("use_interaction", "Include interaction(s)", FALSE),
      uiOutput("interaction_ui"),
      
      hr(),
      
      textInput("random_term", "Random-effects term (e.g., 1 | id, day_scaled | id"),
      
      actionButton("fit", "Fit mixed model", class = "btn-primary"),
      hr(),
      
      h4("Download Outputs"),
      textInput("fname_base", "File name prefix:", "mixedmodel"),
      downloadButton("download_docx", "Download Word Report", class = "btn-primary"),
      downloadButton("download_zip", "Download Plots ZIP", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data preview", tableOutput("head"), verbatimTextOutput("summary")),
        tabPanel("Model summary", verbatimTextOutput("model_summary")),
        tabPanel("Regression table", gt_output("reg_table")),
        tabPanel("Interpretation", htmlOutput("interpretation")),
        tabPanel("Interaction effects", plotOutput("interaction_plot", height = 500)),
        tabPanel("Simple slopes / trends", plotOutput("slopes_plot", height = 500))
      )
    )
  )
)
# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  emmeans::emm_options(lmerTest.limit = 10000)
  
  # ---- Dataset reactive value ----
  dataset <- reactiveVal(NULL)
  
  # ---- Dynamically update environment datasets ----
  observe({
    dfs <- ls(envir = .GlobalEnv)
    is_df <- vapply(dfs, function(x) {
      obj <- tryCatch(get(x, envir = .GlobalEnv), error = function(e) NULL)
      is.data.frame(obj)
    }, logical(1))
    updateSelectInput(session, "env_dataset", choices = c("None", dfs[is_df]))
  })
  
  # ---- Load dataset from file ----
  observeEvent(input$file, {
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    
    df <- tryCatch({
      switch(ext,
             csv   = read.csv(input$file$datapath),
             xlsx  = readxl::read_excel(input$file$datapath),
             xls   = readxl::read_excel(input$file$datapath),
             dta   = haven::read_dta(input$file$datapath) |> mutate(across(where(is.labelled), as_factor)),
             rdata = , rda = { e <- new.env(); load(input$file$datapath, envir = e); get(ls(e)[1], e) },
             stop("Unsupported file type")
      )
    }, error = function(e){ showNotification(e$message, type="error"); NULL })
    
    if(is.data.frame(df)){
      dataset(df)
      showNotification("Dataset loaded from file", type="message")
    }
  })
  
  # ---- Load dataset from environment ----
  observeEvent(input$load_env, {
    req(input$env_dataset)
    if(input$env_dataset == "None") return()
    
    df <- tryCatch({
      get(input$env_dataset, envir = .GlobalEnv)
    }, error = function(e){
      showNotification(paste("Error loading dataset:", e$message), type="error")
      NULL
    })
    
    if(is.data.frame(df)){
      dataset(df)
      showNotification(paste("Dataset", input$env_dataset, "loaded successfully!"), type="message")
    }
  })
  
  df <- reactive({ req(dataset()); dataset() })
  
  # ---- Data preview ----
  output$head <- renderTable(head(df(),10), rownames = TRUE)
  output$summary <- renderPrint(summary(df()))
  
  # ---- Selectors ----
  output$y_ui <- renderUI({ req(df()); selectInput("y","Outcome variable", names(df())) })
  output$x_ui <- renderUI({ req(df()); selectInput("x","Predictors", names(df()), multiple = TRUE) })
  output$center_ui <- renderUI({ selectInput("center_vars","Mean-center variables", input$x, multiple = TRUE) })
  output$scale_ui <- renderUI({ selectInput("scale_vars","Scale variables", input$x, multiple = TRUE) })
  output$interaction_ui <- renderUI({
    req(input$use_interaction, input$x)
    selectizeInput("interactions","Interaction terms",
                   choices = combn(input$x, 2, FUN=function(x) paste(x,collapse=":"), simplify = TRUE),
                   multiple = TRUE)
  })
  
  # ---- Model fitting ----
  models <- eventReactive(input$fit, {
    req(df(), input$y, input$x)
    d <- df()
    
    # Apply transformations first
    for(v in input$center_vars){
      if(is.numeric(d[[v]])) d[[paste0(v,"_mean")]] <- d[[v]] - mean(d[[v]], na.rm = TRUE)
    }
    for(v in input$scale_vars){
      if(is.numeric(d[[v]])) d[[paste0(v,"_scaled")]] <- scale(d[[v]])
    }
    
    # Correct predictors for model
    all_vars <- colnames(d)
    predictors <- sapply(input$x, function(v){
      if(paste0(v,"_mean") %in% all_vars) paste0(v,"_mean")
      else if(paste0(v,"_scaled") %in% all_vars) paste0(v,"_scaled")
      else v
    })
    
    # Interaction terms
    interaction_terms_corrected <- NULL
    if(input$use_interaction && length(input$interactions) > 0){
      interaction_terms_corrected <- sapply(input$interactions, function(term){
        vars <- strsplit(term, ":")[[1]]
        vars <- sapply(vars, function(v){
          if(paste0(v,"_mean") %in% all_vars) paste0(v,"_mean")
          else if(paste0(v,"_scaled") %in% all_vars) paste0(v,"_scaled")
          else v
        })
        paste(vars, collapse=":")
      })
    }
    
    interaction_txt <- if(!is.null(interaction_terms_corrected)) paste("+", paste(interaction_terms_corrected, collapse=" + ")) else ""
    
    # ---- univariate models
    if(input$model_scope=="uni"){
      lapply(predictors, function(v){
        form_txt <- paste(input$y,"~",v, interaction_txt, "+ (", input$random_term,")")
        lmer(as.formula(form_txt), data=d, REML=TRUE)
      })
    } else {
      # multivariate
      fixed <- paste(predictors, collapse=" + ")
      form_txt <- paste(input$y,"~",fixed, interaction_txt, "+ (", input$random_term,")")
      list(lmer(as.formula(form_txt), data=d, REML=TRUE))
    }
  })
  
  primary_model <- reactive({ req(models()); models()[[1]] })
  
  # ---- Model summary
  output$model_summary <- renderPrint({
    mods <- models()
    if(length(mods)==1) summary(mods[[1]])
    else {
      for(i in seq_along(mods)){
        cat("\n====================\n")
        cat("Univariate model:", input$x[i], "\n")
        cat("====================\n\n")
        print(summary(mods[[i]]))
      }
    }
  })
  
  # ---- Regression table
  # ---- Regression table
  output$reg_table <- render_gt({
    mods <- models()
    
    if (input$model_scope == "uni") {
      # build one regression table per predictor
      tbls <- lapply(seq_along(mods), function(i) {
        tbl_regression(
          mods[[i]],
          tidy_fun = broom.mixed::tidy,
          effects  = "fixed"
        ) %>%
          add_global_p() %>%
          modify_caption(paste("Univariate model:", input$x[i]))
      })
      
      # stack them vertically into one table
      stacked <- tbl_stack(tbls)
      as_gt(stacked)
      
    } else {
      # single multivariate model
      tbl_regression(
        mods[[1]],
        tidy_fun = broom.mixed::tidy,
        effects  = "fixed"
      ) %>%
        add_global_p() %>%
        modify_caption("Multivariate mixed-effects model") %>%
        as_gt()
    }
  })
  
  
  # Interpretation
  output$interpretation <- renderUI({
    mods <- models()
    if(is.null(mods)) return(HTML("<i>No model fitted yet.</i>"))
    
    interpretations <- lapply(seq_along(mods), function(i){
      fe <- broom.mixed::tidy(mods[[i]], effects="fixed") |> dplyr::filter(term!="(Intercept)")
      fe <- fe[fe$p.value < 0.05,]
      if(nrow(fe)==0) return(NULL)
      apply(fe,1,function(r){
        scope_txt <- if(input$model_scope=="uni") 
          paste0("<i>Univariate mixed model:</i> ", r["term"], " is associated with outcome.") 
        else 
          paste0("<i>Multivariate mixed model:</i> ", r["term"], " is associated with outcome after adjustment.")
        
        paste0(scope_txt,"<br>Estimated change: <b>", round(as.numeric(r["estimate"]),3),
               "</b> units (p = ", signif(as.numeric(r["p.value"]),3),")")
      })
    })
    interpretations <- unlist(interpretations)
    if(length(interpretations)==0) HTML("<i>No statistically significant fixed effects.</i>") 
    else HTML(paste(interpretations, collapse="<hr>"))
  })
  
  # ---- Interaction plots
  output$interaction_plot <- renderPlot({
    req(primary_model(), input$use_interaction, length(input$interactions)>0)
    d <- df()
    all_vars <- colnames(d)
    interaction_terms_corrected <- sapply(input$interactions, function(term){
      vars <- strsplit(term, ":")[[1]]
      vars <- sapply(vars, function(v){
        if(paste0(v,"_mean") %in% all_vars) paste0(v,"_mean")
        else if(paste0(v,"_scaled") %in% all_vars) paste0(v,"_scaled")
        else v
      })
      paste(vars, collapse=":")
    })
    terms_for_plot <- strsplit(interaction_terms_corrected[1], ":")[[1]]
    pred <- ggeffects::ggpredict(primary_model(), terms = terms_for_plot)
    plot(pred) + labs(title = paste("Interaction:", interaction_terms_corrected[1]))
  })
  
  # ---- Simple slopes / trends
  output$slopes_plot <- renderPlot({
    req(primary_model(), input$use_interaction, length(input$interactions)>0)
    d <- df()
    all_vars <- colnames(d)
    
    # Correct interaction terms to match transformed names
    interaction_terms_corrected <- sapply(input$interactions, function(term){
      vars <- strsplit(term, ":")[[1]]
      vars <- sapply(vars, function(v){
        if(paste0(v,"_mean") %in% all_vars) paste0(v,"_mean")
        else if(paste0(v,"_scaled") %in% all_vars) paste0(v,"_scaled")
        else v
      })
      paste(vars, collapse=":")
    })
    
    terms_for_plot <- strsplit(interaction_terms_corrected[1], ":")[[1]]
    
    slopes <- emmeans::emtrends(
      primary_model(),
      specs = terms_for_plot[2],
      var   = terms_for_plot[1],
      lmerTest.limit = 10000   # increase limit to avoid warning
    )
    plot(slopes)
  })
  
  
  # ---- Word Export ----
  output$download_docx <- downloadHandler(
    filename = function() { paste0(input$fname_base, "_report.docx") },
    content = function(file) {
      mods <- models()
      
      if (input$model_scope == "uni") {
        # build one regression table per predictor
        tbls <- lapply(seq_along(mods), function(i) {
          tbl_regression(
            mods[[i]],
            tidy_fun = broom.mixed::tidy,
            effects  = "fixed"
          ) %>%
            add_global_p() %>%
            modify_caption(paste("Univariate model:", input$x[i]))
        })
        
        # stack them vertically
        stacked <- tbl_stack(tbls)
        reg_table <- as_flex_table(stacked)
        
      } else {
        # single multivariate model
        reg_table <- tbl_regression(
          mods[[1]],
          tidy_fun = broom.mixed::tidy,
          effects  = "fixed"
        ) %>%
          add_global_p() %>%
          modify_caption("Multivariate mixed-effects model") %>%
          as_flex_table()
      }
      
      # Build Word doc
      doc <- read_docx() %>%
        body_add_par("Mixed Model Regression Table", style = "heading 1") %>%
        body_add_flextable(reg_table)
      
      print(doc, target = file)
    }
  )
  
  
  
  # ---- ZIP Export of Plots ----
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0(input$fname_base, "_plots.zip")
    },
    contentType = "application/zip",
    content = function(file) {
      req(primary_model())
      
      tmpdir <- tempdir()
      files  <- character(0)
      
      if (input$use_interaction && length(input$interactions) > 0) {
        
        # ---- FIX: Correct interaction names (same logic as plots tab)
        d <- df()
        all_vars <- colnames(d)
        
        terms <- strsplit(input$interactions[1], ":")[[1]]
        terms <- sapply(terms, function(v) {
          if (paste0(v, "_mean") %in% all_vars) paste0(v, "_mean")
          else if (paste0(v, "_scaled") %in% all_vars) paste0(v, "_scaled")
          else v
        })
        
        # ---- Interaction plot (ggplot)
        inter_path <- file.path(tmpdir, paste0(input$fname_base, "_interaction.png"))
        pred <- ggeffects::ggpredict(primary_model(), terms = terms)
        g_inter <- plot(pred) +
          labs(title = paste("Interaction:", paste(terms, collapse = ":")))
        ggsave(inter_path, g_inter, width = 9, height = 6, dpi = 300)
        files <- c(files, inter_path)
        
        # ---- Slopes plot (base graphics)
        # ---- Slopes plot (base graphics)
        slopes_path <- file.path(tmpdir, paste0(input$fname_base, "_slopes.png"))
        png(slopes_path, width = 900, height = 600)
        slopes <- emmeans::emtrends(
          primary_model(),
          specs = terms[2],
          var   = terms[1]
        )
        print(plot(slopes))   # <<<<<< REQUIRED
        dev.off()
        
        files <- c(files, slopes_path)
      }
      validate(
        need(length(files) > 0, "No plots available to zip.")
      )
      
      # ---- ZIP safely using absolute paths
      utils::zip(
        zipfile = file,
        files   = files,
        flags   = "-j"
      )
    }
  )
  
  
}

shinyApp(ui, server)
