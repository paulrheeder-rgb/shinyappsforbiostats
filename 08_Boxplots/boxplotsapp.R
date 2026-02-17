# app6_boxplot.R
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(haven)
library(rstatix)
library(ggpubr)
library(DT)
library(officer)

ui <- fluidPage(
  titlePanel("Boxplots with Overall and Pairwise p-values"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("datasrc", "Data source",
                   c("Use example: ToothGrowth" = "ex",
                     "Upload CSV/Excel/Stata" = "up",
                     "Upload RData" = "rdata",
                     "Select from environment" = "env"),
                   inline = TRUE),
      
      conditionalPanel("input.datasrc == 'up'",
                       fileInput("file_upload", "Upload file",
                                 accept = c(".csv", ".xlsx", ".xls", ".dta")),
                       checkboxInput("header", "Header", TRUE),
                       radioButtons("sep", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                       radioButtons("quote", "Quote", c(None = "", "Double Quote" = "\"", "Single Quote" = "'"), selected = "\"")
      ),
      
      conditionalPanel("input.datasrc == 'rdata'",
                       fileInput("file_rdata", "Upload RData (.RData/.rda)")
      ),
      
      conditionalPanel("input.datasrc == 'env'",
                       selectInput("env_dataset", "Select dataset from R environment", choices = ls(envir = .GlobalEnv)),
                       actionButton("load_env", "Load")
      ),
      
      hr(),
      uiOutput("box_ui"),
      hr(),
      selectInput("test", "Test type",
                  choices = c("t-test / ANOVA" = "t", "Wilcoxon / Kruskal" = "w")),
      selectInput("padj", "P-adjust:", choices = c("holm","bonferroni","BH","none"), selected = "holm"),
      checkboxInput("show_points", "Jitter points", TRUE),
      checkboxInput("violin", "Add violin", FALSE),
      checkboxInput("flip", "Flip axes", FALSE),
      checkboxInput("logy", "Log-10 Y", FALSE),
      checkboxInput("show_pbars", "Show pairwise p-value bars (>2 groups only)", TRUE),
      numericInput("pdecimals", "P-value decimals:", 3, 1, 5),
      checkboxInput("show_stars", "Significance stars", TRUE),
      hr(),
      textInput("plot_title", "Custom plot title", value = "My Boxplot"),
      numericInput("p_x", "Overall p-value X position", value = NA, step = 0.1),
      numericInput("p_y", "Overall p-value Y position", value = NA, step = 0.1),
      hr(),
      textInput("save_name_box", "Word file name (no ext)", value = "Boxplot"),
      downloadButton("download_box", "Download Word File", class = "btn-success")
    ),
    mainPanel(
      plotOutput("plt", height = "700px"),
      hr(),
      DTOutput("pairwise_tbl")
    )
  )
)

server <- function(input, output, session) {
  
  dat <- reactiveVal(NULL)
  
  # Example dataset
  observe({ if (input$datasrc == "ex") dat(ToothGrowth %>% mutate(dose = factor(dose))) })
  
  # Upload CSV/Excel/Stata
  observeEvent(input$file_upload, {
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    df <- if (ext == "csv") {
      read.csv(input$file_upload$datapath, header = input$header, sep = input$sep, quote = input$quote)
    } else if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file_upload$datapath) %>% as.data.frame()
    } else if (ext %in% c("dta")) {
      haven::read_dta(input$file_upload$datapath) %>% as.data.frame()
    } else NULL
    if (!is.null(df)) dat(df)
  })
  
  # Upload RData
  observeEvent(input$file_rdata, {
    req(input$file_rdata)
    e <- new.env()
    load(input$file_rdata$datapath, envir = e)
    dat(as.data.frame(e[[ls(e)[1]]]))
  })
  
  # Load from environment
  observeEvent(input$load_env, {
    req(input$env_dataset)
    obj <- get(input$env_dataset, envir = .GlobalEnv)
    if (is.data.frame(obj)) dat(as.data.frame(obj))
  })
  
  # Variable pickers
  output$box_ui <- renderUI({
    df <- dat()
    req(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    tagList(
      selectInput("yvar", "Numeric response (Y)", choices = num_vars, selected = num_vars[1]),
      selectInput("xvar", "Grouping (X; categorical)", choices = cat_vars, selected = cat_vars[1]),
      selectInput("facet", "Facet (optional)", choices = c("None" = "none", cat_vars))
    )
  })
  
  dsel <- reactive({
    req(dat(), input$yvar, input$xvar)
    vars <- c(input$yvar, input$xvar)
    if (input$facet != "none") vars <- c(vars, input$facet)
    d <- dat() %>% select(all_of(vars)) %>% drop_na()
    names(d)[1:2] <- c("y", "x")
    if (input$facet != "none") names(d)[3] <- "f"
    d$x <- factor(d$x)
    if ("f" %in% names(d)) d$f <- factor(d$f)
    d
  })
  
  overall_test <- reactive({
    d <- dsel()
    req(nrow(d) > 0, nlevels(d$x) >= 2)
    if (nlevels(d$x) == 2) {
      if (input$test == "t") { res <- t.test(y ~ x, data = d); method <- "t-test" }
      else { res <- wilcox.test(y ~ x, data = d); method <- "Wilcoxon" }
      pval <- res$p.value
    } else {
      if (input$test == "t") { pval <- anova_test(y ~ x, data = d)$p; method <- "ANOVA" }
      else { pval <- kruskal_test(y ~ x, data = d)$p; method <- "Kruskal-Wallis" }
    }
    p_txt <- if (pval < 10^(-input$pdecimals)) "< 0.001"
    else sprintf(paste0("p = %.", input$pdecimals, "f"), pval)
    if (input$show_stars) {
      stars <- ifelse(pval <= 0.001, " ***",
                      ifelse(pval <= 0.01, " **",
                             ifelse(pval <= 0.05, " *", "")))
      p_txt <- paste0(p_txt, stars)
    }
    tibble(label = paste0(method, ": ", p_txt), p = pval)
  })
  
  overall_pos <- reactive({
    d <- dsel()
    x_default <- 1
    y_default <- max(d$y, na.rm = TRUE) * 1.05
    safe_val <- function(v, default) if (is.null(v) || is.na(v) || !is.finite(v)) default else v
    list(x = safe_val(input$p_x, x_default),
         y = safe_val(input$p_y, y_default))
  })
  
  pairwise_res <- reactive({
    d <- dsel()
    if (nlevels(d$x) < 2) return(tibble())
    test_fun <- if (input$test == "t") pairwise_t_test else pairwise_wilcox_test
    res <- d %>% test_fun(y ~ x, p.adjust.method = input$padj)
    if (nrow(res) == 0) return(tibble())
    res %>% mutate(
      label = sapply(p.adj, function(p) {
        p_txt <- if (p < 10^(-input$pdecimals)) "< 0.001"
        else sprintf(paste0("p = %.", input$pdecimals, "f"), p)
        if (input$show_stars) {
          stars <- ifelse(p <= 0.001, " ***",
                          ifelse(p <= 0.01, " **",
                                 ifelse(p <= 0.05, " *", "")))
          p_txt <- paste0(p_txt, stars)
        }
        p_txt
      })
    )
  })
  
  output$pairwise_tbl <- renderDT({
    res <- pairwise_res()
    if (nrow(res) == 0) {
      datatable(tibble(Message = "No pairwise comparisons available"), options = list(dom = 't'))
    } else {
      res_display <- res %>% select(group1, group2, p.adj, label)
      datatable(res_display, options = list(pageLength = 10, dom = 'tip'))
    }
  })
  
  # --- Boxplot reactive ---
  box_plot <- reactive({
    d <- dsel()
    ov <- overall_test()
    pos <- overall_pos()
    
    p <- ggplot(d, aes(x, y, fill = x)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
      labs(title = input$plot_title, x = input$xvar, y = input$yvar) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    if (input$violin) p <- p + geom_violin(alpha = 0.3)
    if (input$show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 1.5)
    if (input$logy) p <- p + scale_y_log10()
    if (input$flip) p <- p + coord_flip()
    if (input$facet != "none") p <- p + facet_wrap(~f)
    
    # Overall p-value annotation
    p <- p + annotate("text", x = pos$x, y = pos$y, label = ov$label,
                      hjust = 0, vjust = 0, size = 5, fontface = "bold")
    
    # Pairwise bars only if overall p < 0.05
    if (input$show_pbars && nlevels(d$x) > 2 && ov$p < 0.05) {
      res <- pairwise_res()
      if (nrow(res) > 0) {
        res_safe <- tryCatch(res %>% add_xy_position(x = "x", fun = "max"), error = function(e) NULL)
        if (!is.null(res_safe) && nrow(res_safe) > 0) {
          p <- p + stat_pvalue_manual(res_safe, label = "label",
                                      tip.length = 0.02, size = 4,
                                      bracket.nudge.y = 0.05 * max(d$y, na.rm = TRUE))
        }
      }
    }
    
    p
  })
  
  output$plt <- renderPlot({ box_plot() })
  
  # --- Download Word file ---
  output$download_box <- downloadHandler(
    filename = function() paste0(input$save_name_box, ".docx"),
    content = function(file) {
      req(box_plot())
      p <- box_plot()
      doc <- read_docx() %>% body_add_gg(p, width = 7, height = 6)
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)

