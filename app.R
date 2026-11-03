library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)

# ---------------------------------------------------------
# 1. Define the UI (All Tabs Upgraded to Premium Layout)
# ---------------------------------------------------------
ui <- page_navbar(
  title = "It Depends",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  
  # --- TAB 1: Confounding ---
  nav_panel("Confounding",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header(class = "bg-primary text-white", "Simpson's Paradox"),
                             card_body(
                               h4("Coffee vs. Heart Risk"),
                               p("Observe how an unmeasured variable creates a spurious positive correlation between coffee and heart risk."),
                               hr(),
                               div(class = "bg-light p-3 rounded border",
                                   checkboxInput("conf_adjust", strong("View: Stratified (Adjust for Smoking)"), value = FALSE)
                               ),
                               hr(),
                               h5("Analysis View:"),
                               uiOutput("conf_dynamic_text")
                             )
                           ),
                           card(card_body(plotOutput("plot_confounding", height = "550px")))
            )
  ),
  
  # --- TAB 2: Berkson's Paradox ---
  nav_panel("Berkson's Paradox",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header(class = "bg-primary text-white", "The Elite Selection Bias"),
                             card_body(
                               h4("Academic vs. Extracurriculars"),
                               p("In the general population, these two traits are completely independent. See what happens when we only look at admitted students."),
                               hr(),
                               div(class = "bg-light p-3 rounded border",
                                   sliderInput("berk_thresh", strong("Elite University Admissions Cutoff:"), 
                                               min = 50, max = 150, value = 50, step = 5)
                               ),
                               hr(),
                               h5("Analysis View:"),
                               uiOutput("berk_dynamic_text")
                             )
                           ),
                           card(card_body(plotOutput("plot_berkson", height = "550px")))
            )
  ),
  
  # --- TAB 3: Collider Bias ---
  nav_panel("Collider Bias",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header(class = "bg-primary text-white", "The Hollywood Illusion"),
                             card_body(
                               h4("Talent vs. Looks"),
                               p("Talent and looks have zero correlation at birth. But what if we only analyze successful actors?"),
                               hr(),
                               div(class = "bg-light p-3 rounded border",
                                   checkboxInput("col_adjust", strong("Condition on Collider (View Actors Only)"), value = FALSE)
                               ),
                               hr(),
                               h5("Analysis View:"),
                               uiOutput("col_dynamic_text")
                             )
                           ),
                           card(card_body(plotOutput("plot_collider", height = "550px")))
            )
  ),
  
  # --- TAB 4: Measurement Error ---
  nav_panel("Measurement Error",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header(class = "bg-primary text-white", "Attenuation to the Null"),
                             card_body(
                               h4("Blood Pressure vs. Stroke Risk"),
                               p("There is a real causal effect here. Watch how clinical measurement noise hides the truth."),
                               hr(),
                               div(class = "bg-light p-3 rounded border",
                                   sliderInput("meas_error", strong("Clinical Measurement Error (Noise):"), 
                                               min = 0, max = 40, value = 0, step = 5)
                               ),
                               hr(),
                               h5("Analysis View:"),
                               uiOutput("meas_dynamic_text")
                             )
                           ),
                           card(card_body(plotOutput("plot_measurement", height = "550px")))
            )
  )
)

# ---------------------------------------------------------
# 2. Define the Server Logic
# ---------------------------------------------------------
server <- function(input, output) {
  
  # ==========================================
  # TAB 1: CONFOUNDING BIAS
  # ==========================================
  set.seed(42)
  df_conf <- rbind(
    data.frame(coffee = rnorm(250, 2, 0.5), risk = rnorm(250, 30, 5), group = "Non-Smoker"),
    data.frame(coffee = rnorm(250, 5, 0.5), risk = rnorm(250, 60, 5), group = "Smoker")
  )
  
  output$conf_dynamic_text <- renderUI({
    if (input$conf_adjust) {
      r_ns <- cor(subset(df_conf, group == "Non-Smoker")$coffee, subset(df_conf, group == "Non-Smoker")$risk)
      HTML(paste0("<div style='color: #27ae60;'><strong>TRUE CAUSAL EFFECT</strong><br><strong>Zero Correlation (r = ", round(r_ns, 2), ")</strong><br><br>Once stratified, the trendlines flatten. Coffee has no effect on risk.</div>"))
    } else {
      r_all <- cor(df_conf$coffee, df_conf$risk)
      HTML(paste0("<div style='color: #c0392b;'><strong>NAÏVE CORRELATION</strong><br><strong>Strong Positive (r = +", round(r_all, 2), ")</strong><br><br>Without adjusting for smoking, coffee seems highly dangerous. This is entirely confounded.</div>"))
    }
  })
  
  output$plot_confounding <- renderPlot({
    p <- ggplot(df_conf, aes(x = coffee, y = risk)) + theme_minimal(base_size = 16) + labs(x = "Coffee (Cups/Day)", y = "Heart Risk")
    if (input$conf_adjust) {
      p + geom_point(aes(color = group), size = 3, alpha = 0.7) + geom_smooth(aes(color = group), method = "lm", se = FALSE, linewidth = 1.5) + scale_color_manual(values = c("Non-Smoker" = "#27ae60", "Smoker" = "#c0392b")) + theme(legend.position = "bottom")
    } else {
      p + geom_point(color = "#7f8c8d", size = 3, alpha = 0.7) + geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", linewidth = 1.5)
    }
  })
  
  # ==========================================
  # TAB 2: BERKSON'S PARADOX
  # ==========================================
  # Reactive dataset based on threshold
  berk_data <- reactive({
    set.seed(123)
    df <- data.frame(academic = rnorm(500, 50, 15), extra = rnorm(500, 50, 15))
    df$admitted <- ifelse((df$academic + df$extra) > input$berk_thresh, "Admitted", "Rejected")
    df
  })
  
  output$berk_dynamic_text <- renderUI({
    df <- berk_data()
    admitted_df <- subset(df, admitted == "Admitted")
    
    if(nrow(admitted_df) > 5) {
      r_admitted <- cor(admitted_df$academic, admitted_df$extra)
      HTML(paste0("<div style='color: #2980b9;'><strong>SELECTION BIAS VISIBLE</strong><br><strong>Admitted Correlation: r = ", round(r_admitted, 2), "</strong><br><br>By filtering for high combined scores, we artificially create a negative correlation. To be admitted with a lower academic score, a student <i>must</i> have an exceptionally high extracurricular score.</div>"))
    } else {
      HTML("<div style='color: #7f8c8d;'>Threshold too high. No students admitted.</div>")
    }
  })
  
  output$plot_berkson <- renderPlot({
    df <- berk_data()
    ggplot(df, aes(x = academic, y = extra)) +
      geom_point(aes(color = admitted), size = 3, alpha = 0.6) +
      geom_smooth(data = subset(df, admitted == "Admitted"), method = "lm", se = FALSE, color = "#2980b9", linewidth = 1.5) +
      scale_color_manual(values = c("Admitted" = "#3498db", "Rejected" = "#bdc3c7")) +
      theme_minimal(base_size = 16) +
      labs(x = "Academic Score", y = "Extracurricular Score") +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) + theme(legend.title = element_blank())
  })
  
  # ==========================================
  # TAB 3: COLLIDER BIAS
  # ==========================================
  set.seed(99)
  df_col <- data.frame(talent = rnorm(1000, 50, 15), looks = rnorm(1000, 50, 15))
  df_col$is_actor <- ifelse((df_col$talent + df_col$looks) > 130, "Actor", "General Public")
  
  output$col_dynamic_text <- renderUI({
    if (input$col_adjust) {
      r_actor <- cor(subset(df_col, is_actor == "Actor")$talent, subset(df_col, is_actor == "Actor")$looks)
      HTML(paste0("<div style='color: #d35400;'><strong>SPURIOUS CORRELATION</strong><br><strong>Actors Only (r = ", round(r_actor, 2), ")</strong><br><br>We conditioned on a collider (Actor status). Because you need either extreme talent or extreme looks to make it, the two traits now appear negatively correlated.</div>"))
    } else {
      r_gen <- cor(df_col$talent, df_col$looks)
      HTML(paste0("<div style='color: #27ae60;'><strong>TRUE CAUSAL EFFECT</strong><br><strong>General Population (r = ", round(r_gen, 2), ")</strong><br><br>At birth, talent and looks are completely independent.</div>"))
    }
  })
  
  output$plot_collider <- renderPlot({
    p <- ggplot(df_col, aes(x = talent, y = looks)) + theme_minimal(base_size = 16) + labs(x = "Acting Talent", y = "Physical Attractiveness") + coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
    if (input$col_adjust) {
      ggplot(subset(df_col, is_actor == "Actor"), aes(x = talent, y = looks)) + geom_point(color = "#f39c12", size = 3, alpha = 0.8) + geom_smooth(method = "lm", se = FALSE, color = "#d35400", linewidth = 1.5) + theme_minimal(base_size = 16) + labs(x = "Acting Talent", y = "Physical Attractiveness") + coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
    } else {
      p + geom_point(color = "#95a5a6", size = 3, alpha = 0.5) + geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", linewidth = 1.5)
    }
  })
  
  # ==========================================
  # TAB 4: MEASUREMENT ERROR
  # ==========================================
  meas_data <- reactive({
    set.seed(101)
    true_bp <- rnorm(500, 120, 15)
    true_risk <- 0.5 * true_bp + rnorm(500, 0, 10) 
    measured_bp <- true_bp + rnorm(500, 0, input$meas_error)
    data.frame(measured_bp, true_risk)
  })
  
  output$meas_dynamic_text <- renderUI({
    df <- meas_data()
    model <- lm(true_risk ~ measured_bp, data = df)
    slope <- coef(model)[2]
    
    if (input$meas_error == 0) {
      HTML(paste0("<div style='color: #27ae60;'><strong>PERFECT MEASUREMENT</strong><br><strong>Observed Effect: +", round(slope, 2), "</strong><br><br>With a perfect blood pressure cuff, we see the true steep causal relationship.</div>"))
    } else {
      HTML(paste0("<div style='color: #8e44ad;'><strong>ATTENUATED EFFECT</strong><br><strong>Observed Effect: +", round(slope, 2), "</strong><br><br>As noise increases, the slope flattens out (attenuates toward the null). Poor measurement hides the true danger.</div>"))
    }
  })
  
  output$plot_measurement <- renderPlot({
    df <- meas_data()
    ggplot(df, aes(x = measured_bp, y = true_risk)) +
      geom_point(color = "#9b59b6", size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", linewidth = 1.5) +
      theme_minimal(base_size = 16) +
      labs(x = "Measured Blood Pressure (with error)", y = "Stroke Risk Score") +
      coord_cartesian(xlim = c(60, 180), ylim = c(20, 100))
  })
}

shinyApp(ui = ui, server = server)