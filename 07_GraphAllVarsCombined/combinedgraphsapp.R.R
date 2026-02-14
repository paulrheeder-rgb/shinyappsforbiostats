# ============================================================
# Unified Graph App – FINAL WORKING VERSION – Jan 07, 2026
# • All plots display correctly
# • All tabs download to Word (.docx) via browser dialog
# • No fixed save folders; shinyFiles removed
# ============================================================
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(officer)
library(rvg)
library(haven)
library(readxl)
library(tidyr)
library(ggpubr)        # for as_ggplot()
library(ggrepel)
library(rstatix)
library(forcats)
library(DT)
library(fs)
library(ggpmisc)
library(conflicted)

conflicts_prefer(ggplot2::annotate)

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Unified Graph App: Explore Variables"),
  
  fluidRow(
    column(4,
           radioButtons("data_source", "Data source:",
                        choices = c("Upload file" = "upload", "R Environment" = "env"),
                        inline = TRUE)
    ),
    column(4,
           conditionalPanel(
             condition = "input.data_source == 'upload'",
             fileInput("file", "Upload Excel / Stata / RData / CSV",
                       accept = c(".xlsx", ".xls", ".dta", ".RData", ".rda", ".rds", ".csv"))
           ),
           conditionalPanel(
             condition = "input.data_source == 'env'",
             uiOutput("env_data_ui")
           )
    )
  ),
  hr(),
  
  tabsetPanel(
    tabPanel("Continuous Distributions",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("numeric_ui"),
                 actionButton("plot_cont_btn", "Generate Plots", class = "btn-primary"),
                 hr(),
                 textInput("save_name_cont", "Word file name (no ext)", value = "Continuous_Plots"),
                 downloadButton("download_cont", "Download Word File", class = "btn-success")
               ),
               mainPanel(plotOutput("combined_cont_plot", height = "1200px"))
             )),
    
    tabPanel("Categorical Distributions",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("factor_ui"),
                 radioButtons("cat_plot_type", "Plot type:",
                              choices = c("Count" = "count", "Percent" = "percent")),
                 actionButton("plot_cat_btn", "Generate Plots", class = "btn-primary"),
                 hr(),
                 textInput("save_name_cat", "Word file name (no ext)", value = "Categorical_Plots"),
                 downloadButton("download_cat", "Download Word File", class = "btn-success")
               ),
               mainPanel(plotOutput("combined_cat_plot", height = "800px"))
             )),
    
    tabPanel("Boxplots by Group",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("box_ui"),
                 hr(),
                 textInput("save_name_box", "Word file name (no ext)", value = "Boxplot"),
                 downloadButton("download_box", "Download Word File", class = "btn-success")
               ),
               mainPanel(
                 plotOutput("boxplot", height = "700px"),
                 hr(),
                 DTOutput("pairwise_tbl")
               )
             )),
    
    tabPanel("Scatterplots (Continuous vs Continuous)",
             sidebarLayout(
               sidebarPanel(
                 h4("Select variables"),
                 selectInput("xvar_s", "X variable:", choices = NULL),
                 selectInput("yvar_s", "Y variable:", choices = NULL),
                 selectInput("label_var", "Label variable (optional):",
                             choices = c("None"), selected = "None"),
                 checkboxInput("use_loess", "Add LOESS smoother", TRUE),
                 checkboxInput("add_lm", "Add linear fit", FALSE),
                 checkboxInput("add_ci", "Show confidence interval", FALSE),
                 checkboxInput("show_labels", "Show row labels", FALSE),
                 hr(),
                 h4("Correlation display"),
                 checkboxInput("show_corr", "Show correlation on plot", TRUE),
                 numericInput("corr_x_val", "X position (data units):", value = NA, step = 0.1),
                 numericInput("corr_y_val", "Y position (data units):", value = NA, step = 0.1),
                 numericInput("pdecimals_scatter", "P-value decimals:", 3, 1, 6),
                 checkboxInput("show_stars_scatter", "Significance stars", TRUE),
                 hr(),
                 h4("Save Scatterplot"),
                 textInput("wordfile_scatter", "Word file name (no ext)", "Scatterplot"),
                 downloadButton("download_scatter", "Download Word File", class = "btn-success")
               ),
               mainPanel(plotOutput("scatterplot", height = "600px"))
             ))
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Data loading
  output$env_data_ui <- renderUI({
    env_dfs <- Filter(function(x) is.data.frame(get(x, .GlobalEnv)), ls(.GlobalEnv))
    if (length(env_dfs) == 0) return(helpText("No data frames in environment."))
    selectInput("env_df", "Select dataset:", choices = env_dfs)
  })
  
  df <- reactive({
    if (is.null(input$data_source)) return(NULL)
    if (input$data_source == "upload") {
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      path <- input$file$datapath
      if (ext %in% c("xlsx", "xls")) readxl::read_excel(path) %>% as.data.frame()
      else if (ext == "dta") haven::read_dta(path) %>% as.data.frame()
      else if (ext %in% c("rdata", "rda")) { e <- new.env(); load(path, e); get(ls(e)[1], e) }
      else if (ext == "rds") readRDS(path)
      else if (ext == "csv") read.csv(path, stringsAsFactors = FALSE)
      else { showNotification("Unsupported file", type = "error"); NULL }
    } else {
      req(input$env_df)
      get(input$env_df, .GlobalEnv)
    }
  })
  
  observe({
    req(df())
    nums <- names(df())[sapply(df(), is.numeric)]
    cats <- names(df())[sapply(df(), function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "xvar_s", choices = nums)
    updateSelectInput(session, "yvar_s", choices = nums)
    updateSelectInput(session, "label_var", choices = c("None", names(df())))
  })
  
  output$numeric_ui <- renderUI({
    req(df())
    selectInput("numeric_vars", "Numeric variables:", choices = names(df())[sapply(df(), is.numeric)], multiple = TRUE)
  })
  
  output$factor_ui <- renderUI({
    req(df())
    selectInput("factor_vars", "Categorical variables:",
                choices = names(df())[sapply(df(), function(x) is.factor(x) || is.character(x))], multiple = TRUE)
  })
  
  # Helper functions
  make_normality_panels <- function(data, var) {
    x <- na.omit(as.numeric(data[[var]]))
    if (length(x) < 3) return(list(ggplot() + ggtitle(paste(var, ": too few values"))))
    m <- mean(x); s <- sd(x)
    sw <- shapiro.test(x)
    txt <- paste0("W=", round(sw$statistic, 3), ", p=", format.pval(sw$p.value, 3))
    p1 <- ggplot(data.frame(x), aes(x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "blue") +
      stat_function(fun = dnorm, args = list(mean = m, sd = s), color = "red") +
      labs(title = paste(var, ": Histogram + Normal"), subtitle = txt) + theme_bw()
    p2 <- ggplot(data.frame(x), aes(sample = x)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = paste(var, ": Q-Q Plot"), subtitle = txt) + theme_bw()
    list(p1, p2)
  }
  
  make_bar_plot <- function(data, var, type = "count") {
    if (type == "count") {
      ggplot(data, aes(.data[[var]])) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(title = paste("Bar Plot:", var)) + theme_bw()
    } else {
      data %>% count(.data[[var]]) %>%
        mutate(pct = 100 * n / sum(n)) %>%
        ggplot(aes(.data[[var]], pct)) +
        geom_col(fill = "skyblue", color = "black") +
        geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.3) +
        labs(title = paste("Percent Bar Plot:", var)) + theme_bw()
    }
  }
  
  # Continuous plots
  cont_combined_plot <- reactive({
    req(df(), input$numeric_vars)
    plots <- list()
    for (v in input$numeric_vars) plots <- c(plots, make_normality_panels(df(), v))
    if (length(plots) == 0) return(NULL)
    as_ggplot(arrangeGrob(grobs = plots, ncol = 2))
  })
  
  output$combined_cont_plot <- renderPlot({ cont_combined_plot() })
  
  # Categorical plots
  cat_combined_plot <- reactive({
    req(df(), input$factor_vars)
    plots <- list()
    for (v in input$factor_vars) plots <- c(plots, list(make_bar_plot(df(), v, input$cat_plot_type)))
    if (length(plots) == 0) return(NULL)
    as_ggplot(arrangeGrob(grobs = plots, ncol = 2))
  })
  
  output$combined_cat_plot <- renderPlot({ cat_combined_plot() })
  
  # ======================== BOXPLOTS ========================
  output$box_ui <- renderUI({
    req(df())
    nums <- names(df())[sapply(df(), is.numeric)]
    cats <- names(df())[sapply(df(), function(x) is.factor(x) || is.character(x))]
    tagList(
      selectInput("yvar", "Y (numeric):", choices = nums, selected = nums[1]),
      selectInput("xvar", "X (group):", choices = cats, selected = cats[1]),
      selectInput("facet", "Facet (optional):", choices = c("None" = "none", cats)),
      selectInput("test", "Test type:",
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
      h4("Overall Test Position (data units)"),
      numericInput("corr_x_val", "X position:", value = NA, step = 0.1),
      numericInput("corr_y_val", "Y position:", value = NA, step = 0.1)
    )
  })
  
  dsel <- reactive({
    req(df(), input$yvar, input$xvar)
    vars <- c(input$yvar, input$xvar)
    if (input$facet != "none") vars <- c(vars, input$facet)
    d <- df() %>% select(all_of(vars)) %>% drop_na()
    names(d)[1:2] <- c("y", "x")
    if (input$facet != "none") names(d)[3] <- "f"
    d$x <- factor(d$x)
    if ("f" %in% names(d)) d$f <- factor(d$f)
    d
  })
  
  overall_pos <- reactive({
    req(dsel())
    d <- dsel()
    xvec <- as.numeric(d$x)
    yvec <- d$y
    x_default <- quantile(xvec, probs = 0.1, na.rm = TRUE)
    y_default <- max(yvec, na.rm = TRUE) * 1.05
    safe_val <- function(v, default) if (is.null(v) || is.na(v) || !is.finite(v)) default else v
    list(x = safe_val(input$corr_x_val, x_default),
         y = safe_val(input$corr_y_val, y_default))
  })
  
  overall_test <- reactive({
    d <- dsel()
    req(nrow(d) > 0, nlevels(d$x) >= 2)
    
    if (nlevels(d$x) == 2) {
      if (input$test == "t") {
        test_res <- t.test(y ~ x, data = d)
        method_name <- "t-test"
      } else {
        test_res <- wilcox.test(y ~ x, data = d)
        method_name <- "Wilcoxon"
      }
      pval <- test_res$p.value
    } else {
      if (input$test == "t") {
        pval <- anova_test(y ~ x, data = d)$p
        method_name <- "ANOVA"
      } else {
        pval <- kruskal_test(y ~ x, data = d)$p
        method_name <- "Kruskal-Wallis"
      }
    }
    
    p_txt <- if (pval < 10^(-input$pdecimals)) "< 0.001"
    else sprintf(paste0("p = %.", input$pdecimals, "f"), pval)
    
    if (input$show_stars) {
      stars <- ifelse(pval <= 0.001, " ***",
                      ifelse(pval <= 0.01, " **",
                             ifelse(pval <= 0.05, " *", "")))
      p_txt <- paste0(p_txt, stars)
    }
    
    label <- paste0(method_name, ": ", p_txt)
    tibble(label = label, p = pval, method = method_name)
  })
  
  pairwise_res <- reactive({
    d <- dsel()
    if (nlevels(d$x) < 2) return(tibble())
    
    test_fun <- if (input$test == "t") pairwise_t_test else pairwise_wilcox_test
    
    res <- d %>%
      test_fun(y ~ x, p.adjust.method = input$padj)
    
    if (nrow(res) == 0) return(tibble())
    
    res %>%
      mutate(
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
      res_display <- res %>%
        select(group1, group2, p.adj, label) %>%
        mutate(p.adj = round(p.adj, input$pdecimals + 1))
      datatable(res_display, options = list(pageLength = 10, dom = 'tip'))
    }
  })
  
  # REACTIVE BOXPLOT – FULLY FIXED
  box_plot <- reactive({
    req(dsel(), nrow(dsel()) > 0, nlevels(dsel()$x) >= 2)
    
    d <- dsel()
    ov <- overall_test()
    pos <- overall_pos()
    
    p <- ggplot(d, aes(x = x, y = y, fill = x)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") +
      labs(title = paste(input$yvar, "by", input$xvar), x = input$xvar, y = input$yvar)
    
    if (input$violin) p <- p + geom_violin(alpha = 0.3)
    if (input$show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 1.5)
    if (input$logy) p <- p + scale_y_log10()
    if (input$flip) p <- p + coord_flip()
    if (input$facet != "none") p <- p + facet_wrap(~f)
    
    p <- p + annotate("text", x = pos$x, y = pos$y, label = ov$label,
                      hjust = 0, vjust = 0, size = 5, fontface = "bold")
    
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
    
    p <- p + theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
    
    p
  })
  
  # CRITICAL: Render the boxplot
  output$boxplot <- renderPlot({
    box_plot()
  })
  
  # ======================== SCATTERPLOT ========================
  scatter_plot <- reactive({
    req(df(), input$xvar_s, input$yvar_s)
    
    data <- df()
    x <- input$xvar_s
    y <- input$yvar_s
    lbl <- if (input$label_var != "None") input$label_var else NULL
    
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point(alpha = 0.7, color = "#0072B2", size = 2)
    
    if (input$use_loess) {
      p <- p + geom_smooth(method = "loess", se = input$add_ci, color = "orange")
    }
    
    if (input$add_lm) {
      p <- p + geom_smooth(method = "lm", se = input$add_ci, color = "red", linetype = "dashed")
    }
    
    if (input$show_labels && !is.null(lbl)) {
      p <- p + geom_text_repel(aes_string(label = lbl), size = 3)
    }
    
    if (input$show_corr && !input$add_lm) {
      ctest <- cor.test(data[[x]], data[[y]], method = "pearson")
      r <- round(ctest$estimate, 3)
      pval <- ctest$p.value
      ptxt <- if (pval < 0.001) "p < 0.001"
      else sprintf(paste0("p = %.", input$pdecimals_scatter, "f"), pval)
      if (input$show_stars_scatter) {
        ptxt <- paste0(ptxt,
                       ifelse(pval <= 0.001, " ***",
                              ifelse(pval <= 0.01, " **",
                                     ifelse(pval <= 0.05, " *", ""))))
      }
      corr_label <- paste0("r = ", r, ", ", ptxt)
      
      safe_val <- function(v, default) if (is.null(v) || is.na(v) || !is.finite(v)) default else v
      xpos <- safe_val(input$corr_x_val, mean(range(data[[x]], na.rm = TRUE)))
      ypos <- safe_val(input$corr_y_val, max(data[[y]], na.rm = TRUE) * 0.95)
      
      p <- p + annotate("text", x = xpos, y = ypos, label = corr_label,
                        hjust = 0, vjust = 1, size = 5, fontface = "bold")
    } else if (input$add_lm) {
      lm_mod <- lm(as.formula(paste(y, "~", x)), data = data)
      coefs <- coef(lm_mod)
      b0 <- round(coefs[1], 3)
      b1 <- round(coefs[2], 3)
      r2 <- round(summary(lm_mod)$r.squared, 3)
      eq_txt <- sprintf("y = %.3f + %.3f·%s\nR² = %.3f", b0, b1, x, r2)
      
      safe_val <- function(v, default) if (is.null(v) || is.na(v) || !is.finite(v)) default else v
      xpos <- safe_val(input$corr_x_val, mean(range(data[[x]], na.rm = TRUE)))
      ypos <- safe_val(input$corr_y_val, max(data[[y]], na.rm = TRUE) * 0.95)
      
      p <- p + annotate("text", x = xpos, y = ypos, label = eq_txt,
                        hjust = 0, vjust = 1, size = 5, fontface = "bold")
    }
    
    p <- p + labs(x = x, y = y) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
    
    p
  })
  
  # CRITICAL: Render the scatterplot
  output$scatterplot <- renderPlot({
    scatter_plot()
  })
  
  # ======================== DOWNLOAD HANDLERS ========================
  output$download_cont <- downloadHandler(
    filename = function() {
      paste0(input$save_name_cont, ".docx")
    },
    content = function(file) {
      req(cont_combined_plot())
      p <- cont_combined_plot()
      doc <- read_docx() %>% body_add_gg(p, width = 10, height = 10)
      print(doc, target = file)
    }
  )
  
  output$download_cat <- downloadHandler(
    filename = function() paste0(input$save_name_cat, ".docx"),
    content = function(file) {
      req(cat_combined_plot())
      p <- cat_combined_plot()
      doc <- read_docx() %>% body_add_gg(p, width = 10, height = 8)
      print(doc, target = file)
    }
  )
  
  output$download_box <- downloadHandler(
    filename = function() paste0(input$save_name_box, ".docx"),
    content = function(file) {
      req(box_plot())
      p <- box_plot()
      doc <- read_docx() %>% body_add_gg(p, width = 7, height = 6)
      print(doc, target = file)
    }
  )
  
  output$download_scatter <- downloadHandler(
    filename = function() paste0(input$wordfile_scatter, ".docx"),
    content = function(file) {
      req(scatter_plot())
      p <- scatter_plot()
      doc <- read_docx() %>% body_add_gg(p, width = 7, height = 6)
      print(doc, target = file)
    }
  )
}

# ============================================================
# Run App
# ============================================================
shinyApp(ui = ui, server = server)

