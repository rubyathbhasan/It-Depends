pacman::p_load(shiny, ggplot2, bslib, dplyr, tidyr, waiter)

# 1. Define the UI
# ---------------------------------------------------------
ui <- page_navbar(
  title = "It Depends",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  fillable = FALSE, # single global scrollbar
  
  # octopus cursor follower
  header = tagList(
    use_waiter(),
    waiter_show_on_load(
      html = tagList(
        spin_fading_circles(), 
        h4("Please wait, it takes some time when opening for the first time...", 
           style = "color: white; margin-top: 20px;")
      ),
      color = "#2C3E50" 
    ),
    tags$head(
      tags$style(HTML("
        #cursor-follower {
          position: absolute;
          pointer-events: none;
          font-size: 40px;
          z-index: 9999;
          transition: top 0.05s, left 0.05s;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          $(document).mousemove(function(e) {
            $('#cursor-follower').css({
              left: e.pageX + 15 + 'px',
              top:  e.pageY - 15 + 'px'  /* Changed from + 15 to - 15 to raise it */
            });
          });
        });
      "))
    ),
    div(id = "cursor-follower", "🦖")
  ),
  
  # ==========================================
  # TAB 1: CONFOUNDING BIAS
  # ==========================================
  nav_panel("Confounding",
            navset_card_underline(
              title = "Confounding bias",
              
              # -- SUB-TAB 1: Confounding example --
              nav_panel("Confounding example",
                        layout_columns(col_widths = c(4, 8),
                                       card(
                                         card_header(class = "bg-primary text-white", "What is a Confounder?"),
                                         card_body(
                                           HTML("
                                            <h5>A Confounder needs to fulfill 3 criterea:</h5>
                                            <ol style='font-size: 0.9em;'>
                                              <li>It is associated with the <b>Exposure</b>.</li>
                                              <li>It is an independent cause, risk factor, or protective factor of the <b>Outcome</b>.</li>
                                              <li>It is <em>not</em> a mediator (not on the causal pathway) between them. </li>
                                            </ol>
                                            <hr>
                                            <div style='text-align:center; font-size: 1.1em; margin: 15px 0;'>
                                              <strong>Smoking</strong> (Confounder)<br>
                                              &#8601; &nbsp; &nbsp; &nbsp; &nbsp; &#8600;<br>
                                              <strong>Coffee</strong> (Exp) &nbsp; &nbsp; &nbsp; <strong>Heart Disease</strong> (Out)
                                            </div>
                                            <hr>
                                            "),
                                           p("It is showing how failing to adjust for smoking creates a massive spurious Odds Ratio between coffee and heart disease."),
                                           div(class = "bg-light p-3 rounded border",
                                               checkboxInput("conf_adjust", strong("Stratify Data (Adjust for Smoking)"), value = FALSE)
                                           ),
                                           hr(),
                                           h5("Analysis Insight:"),
                                           uiOutput("conf_verdict"),
                                           hr(),
                                           HTML("<p style='font-size: 0.8em; color: #7f8c8d;'><em>*Note: This is a fictional dataset simulated for educational purposes. Inspired by classic examples in Gordis Epidemiology (6th Ed).</em></p>")
                                         )
                                       ),
                                       card(
                                         fillable = FALSE, 
                                         card_header("2x2 Tables & Mathematical Adjustment"),
                                         card_body(
                                           uiOutput("conf_tables"),
                                           hr(),
                                           plotOutput("plot_confounding", height = "400px") 
                                         )
                                       )
                        )
              ),
              
              # -- SUB-TAB 2: Risk of Overadjustment--
              nav_panel("Risk of Overadjustment",
                        tags$head(
                          tags$style(HTML("
                            .sim-container { background-color: #ffffff; max-width: 700px; margin: 0 auto; padding: 20px; border-radius: 10px; }
                            .diagram { position: relative; height: 250px; margin: 40px 0; background: #f8fbff; border: 2px solid #e0e6ed; border-radius: 8px; }
                            .node { position: absolute; padding: 15px 20px; background: #34495e; color: white; border-radius: 8px; font-weight: bold; text-align: center; z-index: 2; }
                            #node-bmi { top: 100px; left: 20px; }
                            #node-bp { top: 20px; left: 250px; background: #e74c3c; }
                            #node-esrd { top: 100px; right: 20px; }
                            .sim-svg { position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 1; }
                            .sim-line { stroke: #2c3e50; stroke-width: 4; transition: all 0.4s ease; }
                            .sim-arrow { fill: #2c3e50; transition: all 0.4s ease; }
                            .blocked-path { stroke: #bdc3c7; stroke-dasharray: 5, 5; }
                            .blocked-arrow { fill: #bdc3c7; }
                            .block-x { position: absolute; top: 50px; left: 280px; font-size: 40px; color: #c0392b; font-weight: bold; z-index: 3; }
                            .controls-box { text-align: center; margin: 20px 0; padding: 20px; background: #ecf0f1; border-radius: 8px; }
                            .stats-box { display: flex; justify-content: space-around; padding: 20px; font-size: 1.2em; border-top: 2px solid #eee; }
                            .stat-box { text-align: center; }
                            .stat-value { font-size: 1.8em; font-weight: bold; display: block; margin-top: 5px; transition: color 0.3s; }
                          "))
                        ),
                        layout_columns(col_widths = c(12),
                                       card(
                                         card_header(class = "bg-primary text-white", "Direct vs. Indirect Pathways (BMI -> BP -> ESRD)"),
                                         card_body(
                                           div(class = "sim-container",
                                               div(class = "controls-box",
                                                   checkboxInput("adjustToggle", 
                                                                 tags$b("Apply Statistical Adjustment for Blood Pressure", style="color: #c0392b; font-size: 1.1em;"), 
                                                                 value = FALSE),
                                                   p("Watch how the model changes when we mathematically 'control' for the mediator.", style = "color: #555; margin-top: 5px;")
                                               ),
                                               uiOutput("diagramUI"),
                                               uiOutput("statsUI")
                                           )
                                         )
                                       )
                        )
              )
            )
  ),

  # ==========================================
  # TAB 2: BERKSON'S PARADOX & SELECTION BIAS
  # ==========================================
  nav_panel("Berkson's / Selection Bias",
            navset_card_underline(
              title = "Selection Bias Scenarios",
              
              # -- SUB-TAB 1: Smoking & Melanoma (Flipped OR) --
              nav_panel("Scenario 1: Flipped OR (Smoking & Melanoma)",
                        layout_columns(col_widths = c(4, 8),
                                       card(
                                         card_header(class = "bg-primary text-white", "Smoking vs. Melanoma"),
                                         card_body(
                                           p("Smoking does not cause melanoma (True OR = 1.0). But smoking causes COPD and heart attacks, which drive hospital admissions. Watch how hospital controls artificially make smoking look protective."),
                                           hr(),
                                           div(class = "bg-light p-3 rounded border",
                                               sliderInput("s1_smoke_rate", "General Public Smoking Rate (%):", min = 5, max = 50, value = 20, step = 5),
                                               sliderInput("s1_hosp_smoke", "Admission Rate for Smokers (due to COPD, etc) (%):", min = 10, max = 80, value = 60, step = 5),
                                               sliderInput("s1_hosp_non", "Admission Rate for Non-Smokers (%):", min = 5, max = 30, value = 10, step = 5)
                                           ),
                                           hr(),
                                           h5("Epidemiological Impact:"),
                                           uiOutput("s1_dynamic_text"),
                                           hr(),
                                           HTML("<p style='font-size: 0.8em; color: #7f8c8d;'><em>*Note: Fictional simulation demonstrating Berkson's Bias.</em></p>")
                                         )
                                       ),
                                       card(
                                         fillable = FALSE,
                                         card_header("Study Data & Learning Details"),
                                         card_body(
                                           uiOutput("s1_tables"),
                                           hr(),
                                           plotOutput("plot_s1", height = "350px"), 
                                           hr(),
                                           accordion(
                                             accordion_panel("The Logic (Artificial Inflation)", 
                                                             HTML("<p>When you select hospital controls, you are sampling people admitted for 'other diseases'. Because smoking is a massive driver of 'other diseases' (like heart attacks), your hospital control group is packed with smokers.</p><p>When comparing normal melanoma cases to a control group that is 60% smokers, the math spits out a <strong>spurious negative association</strong>, making smoking look protective.</p>"))
                                           )
                                         )
                                       )
                        )
              ),
              
              # -- SUB-TAB 2: Coffee & Pancreatic Cancer (Heightened OR) --
              nav_panel("Scenario 2: Heightened OR (Coffee & Cancer)",
                        layout_columns(col_widths = c(4, 8),
                                       card(
                                         card_header(class = "bg-primary text-white", "Coffee vs. Pancreatic Cancer"),
                                         card_body(
                                           p("Coffee does not cause pancreatic cancer (True OR = 1.0). In 1981, researchers used GI ulcer patients as hospital controls. Because ulcer patients avoid coffee, watch how the risk is artificially inflated."),
                                           hr(),
                                           div(class = "bg-light p-3 rounded border",
                                               sliderInput("s2_coffee_norm", "Coffee Drinking in General Public (%):", min = 30, max = 90, value = 60, step = 5),
                                               sliderInput("s2_coffee_ulcer", "Coffee Drinking in Ulcer Patients (Controls) (%):", min = 0, max = 50, value = 10, step = 5),
                                               sliderInput("s2_hosp_ulcer", "Hospitalization Rate for Ulcers (%):", min = 10, max = 90, value = 80, step = 5)
                                           ),
                                           hr(),
                                           h5("Epidemiological Impact:"),
                                           uiOutput("s2_dynamic_text"),
                                           hr(),
                                           HTML("<p style='font-size: 0.8em; color: #7f8c8d;'><em>*Note: Simulation based on the historical design flaw in MacMahon et al. (NEJM 1981).</em></p>")
                                         )
                                       ),
                                       card(
                                         fillable = FALSE,
                                         card_header("Study Data & Learning Details"),
                                         card_body(
                                           uiOutput("s2_tables"),
                                           hr(),
                                           plotOutput("plot_s2", height = "350px"), 
                                           hr(),
                                           accordion(
                                             accordion_panel("The Logic (Artificial Deflation)", 
                                                             HTML("<p>This mimics the infamous 1981 MacMahon study. The researchers used gastrointestinal (GI) clinic patients as controls. GI ulcer patients actively avoid coffee because it hurts their stomachs.</p><p>By comparing normal cancer cases (who drink regular amounts of coffee) to a control group that drinks almost no coffee, the cases looked like extreme addicts. This created a <strong>spurious positive association</strong>, falsely labeling coffee as a carcinogen.</p>"))
                                           )
                                         )
                                       )
                        )
              ),
              
              # -- SUB-TAB 3: Low Birth Weight (Continuous Collider) --
              nav_panel("Scenario 3: Low Birth Weight (LBW)",
                        layout_columns(col_widths = c(4, 8),
                                       card(
                                         card_header(class = "bg-primary text-white", "The Maternal Smoking Paradox"),
                                         card_body(
                                           HTML("
                                           <h5>Analysis Goal:</h5>
                                           <p style='font-size: 0.9em;'>We are analyzing whether <strong>Maternal Smoking (Exposure)</strong> increases the risk of <strong>Lethal Genetic Defects (Outcome)</strong>.</p>
                                           <p style='font-size: 0.9em;'>In reality, they are completely independent. However, both smoking and genetic defects independently cause a baby to be born small (Low Birth Weight). Watch what happens when we filter our analysis to look ONLY at LBW babies.</p>
                                           <hr>
                                           "),
                                           div(class = "bg-light p-3 rounded border",
                                               checkboxInput("lbw_filter", strong("Condition on Collider (View ONLY LBW Babies)"), value = FALSE)
                                           ),
                                           hr(),
                                           h5("Analysis View:"),
                                           uiOutput("lbw_dynamic_text"),
                                           hr(),
                                           HTML("<p style='font-size: 0.8em; color: #7f8c8d;'><em>*Note: Fictional simulation demonstrating the LBW Collider Paradox (Rothman, Modern Epidemiology).</em></p>")
                                         )
                                       ),
                                       card(
                                         fillable = FALSE,
                                         card_header("Scatter Plot: Smoking vs. Lethal Risk"),
                                         card_body(
                                           plotOutput("plot_lbw", height = "450px"),
                                           hr(),
                                           accordion(
                                             accordion_panel("Why does this happen?", 
                                                             HTML("<p>If a mother <strong>did not smoke</strong>, but her baby is still Low Birth Weight, the baby must have a severe unmeasured condition (like a devastating genetic defect) to explain the small size.</p><p>By filtering to only LBW babies, we accidentally compare 'otherwise healthy babies of smokers' to 'severely ill babies of non-smokers'. This forces a spurious negative correlation, creating the dangerous illusion that smoking protects against lethal defects.</p>"))
                                           )
                                         )
                                       )
                        )
              )
            )
  ),
  
  # ==========================================
  # TAB 3: COLLIDER BIAS
  # ==========================================
  nav_panel("Collider Bias",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header(class = "bg-primary text-white", "What is a Collider?"),
                             card_body(
                               HTML("
                                <h5>The Core Rule:</h5>
                                <p style='font-size: 0.9em;'>A Collider is a variable that is <strong>caused by</strong> two other independent variables. If you condition your analysis on a collider (e.g., filtering your dataset), you open a backdoor path and force a spurious correlation between the two original variables.</p>
                                <hr>
                                <div style='text-align:center; font-size: 1.1em; margin: 15px 0;'>
                                  <strong>Talent</strong> (Var A) &nbsp; &nbsp; &nbsp; &nbsp; <strong>Looks</strong> (Var B)<br>
                                  &#8600; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &#8601;<br>
                                  <strong>Actor Status</strong> (Collider)
                                </div>
                                <hr>
                                "),
                               p("Talent and looks have zero correlation at birth. Watch what happens when we only analyze successful actors."),
                               div(class = "bg-light p-3 rounded border",
                                   checkboxInput("col_adjust", strong("Condition on Collider (View Actors Only)"), value = FALSE)
                               ),
                               hr(),
                               h5("Analysis View:"),
                               uiOutput("col_dynamic_text"),
                               hr(),
                               HTML("<p style='font-size: 0.8em; color: #7f8c8d;'><em>*Note: Fictional dataset simulating M-bias / Collider bias.</em></p>")
                             )
                           ),
                           card(card_body(fillable = FALSE, plotOutput("plot_collider", height = "550px")))
            )
  ),
  
  # ==========================================
  # TAB 4: MEASUREMENT ERROR
  # ==========================================
  nav_panel("Measurement Error",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header(class = "bg-primary text-white", "Attenuation to the Null"),
                             card_body(
                               h4("Blood Pressure vs. Stroke Risk"),
                               p("Real-world data collection is rarely perfect. 'Clinical measurement noise' refers to random errors, such as a slightly loose blood pressure cuff, a nervous patient, or a nurse rounding the numbers."),
                               p("Watch how this random noise spreads the data out and hides the true strength of the causal relationship."),
                               hr(),
                               div(class = "bg-light p-3 rounded border",
                                   sliderInput("meas_error", strong("Clinical Measurement Error (Noise/Random Error):"), 
                                               min = 0, max = 40, value = 0, step = 5)
                               ),
                               hr(),
                               h5("Analysis View:"),
                               uiOutput("meas_dynamic_text"),
                               hr(),
                               HTML("<p style='font-size: 0.8em; color: #7f8c8d;'><em>*Note: Fictional simulation of non-differential measurement error.</em></p>")
                             )
                           ),
                           card(card_body(fillable = FALSE, plotOutput("plot_measurement", height = "550px")))
            )
  )
)

# ---------------------------------------------------------
# 2. Define the Server Logic
# ---------------------------------------------------------
server <- function(input, output) {
  Sys.sleep(4) 
  
  # Hide the loading screen
  waiter_hide()
  
  # ==========================================
  # TAB 1: CONFOUNDING BIAS
  # ==========================================
  conf_data <- reactive({
    smokers <- data.frame(
      Smoking = rep("Smoker", 3000),
      Coffee = c(rep("Heavy Drinker", 2700), rep("Low/None", 300)),
      HeartDisease = c(rep("Case", 540), rep("Control", 2160), rep("Case", 60), rep("Control", 240))
    )
    non_smokers <- data.frame(
      Smoking = rep("Non-Smoker", 7000),
      Coffee = c(rep("Heavy Drinker", 700), rep("Low/None", 6300)),
      HeartDisease = c(rep("Case", 35), rep("Control", 665), rep("Case", 315), rep("Control", 5985))
    )
    
    df <- rbind(smokers, non_smokers)
    df$Coffee <- factor(df$Coffee, levels = c("Heavy Drinker", "Low/None"))
    df$HeartDisease <- factor(df$HeartDisease, levels = c("Case", "Control"))
    df
  })
  
  conf_calc <- reactive({
    df <- conf_data()
    
    calc_metrics <- function(data) {
      tbl <- table(data$Coffee, data$HeartDisease)
      a <- tbl[1,1]; b <- tbl[1,2]
      c <- tbl[2,1]; d <- tbl[2,2]
      
      or <- (a * d) / (b * c)
      risk_exp <- a / (a + b)
      risk_unexp <- c / (c + d)
      
      list(tbl = tbl, a=a, b=b, c=c, d=d, or=or, r_exp=risk_exp, r_unexp=risk_unexp)
    }
    
    list(
      crude = calc_metrics(df),
      smokers = calc_metrics(subset(df, Smoking == "Smoker")),
      non = calc_metrics(subset(df, Smoking == "Non-Smoker"))
    )
  })
  
  output$conf_verdict <- renderUI({
    res <- conf_calc()
    if (input$conf_adjust) {
      HTML(paste0("<div style='color: #27ae60;'><strong>CONFOUNDING REVEALED</strong><br>Once we stratify by smoking status, the association disappears entirely. The True Stratified OR is exactly <strong>", round(res$smokers$or, 2), "</strong>. Coffee is innocent!</div>"))
    } else {
      HTML(paste0("<div style='color: #c0392b;'><strong>SPURIOUS ASSOCIATION</strong><br>Looking at the naive data, coffee appears highly dangerous with a <strong>Crude OR of ", round(res$crude$or, 2), "</strong>. This is because coffee drinkers are heavily skewed toward being smokers.</div>"))
    }
  })
  
  output$conf_tables <- renderUI({
    res <- conf_calc()
    
    build_html_table <- function(title, m, color_class) {
      HTML(paste0(
        "<div style='font-size: 0.85em; margin-bottom: 15px;'><strong>", title, "</strong><br>",
        "<table class='table table-sm table-bordered text-center'>",
        "<tr class='table-light'><th></th><th>Case</th><th>Control</th><th>Total</th><th>Risk (Case/Total)</th></tr>",
        "<tr><th class='text-start'>Exp (Heavy)</th><td>", m$a, "</td><td>", m$b, "</td><td>", m$a + m$b, "</td><td><strong>", round(m$r_exp * 100, 1), "%</strong></td></tr>",
        "<tr><th class='text-start'>Unexp (Low)</th><td>", m$c, "</td><td>", m$d, "</td><td>", m$c + m$d, "</td><td><strong>", round(m$r_unexp * 100, 1), "%</strong></td></tr>",
        "<tr class='table-secondary'><th class='text-start'>Total</th><td>", m$a + m$c, "</td><td>", m$b + m$d, "</td><td>", m$a+m$b+m$c+m$d, "</td><td></td></tr>",
        "</table>",
        "<p style='font-size: 0.9em; margin-bottom: 5px; color: #7f8c8d;'><em>Legend: <strong>a</strong>=Exp Case, <strong>b</strong>=Exp Ctrl, <strong>c</strong>=Unexp Case, <strong>d</strong>=Unexp Ctrl</em></p>",
        "<p class='", color_class, " mb-0'><strong>Odds Ratio = (a &times; d) / (b &times; c)</strong><br>",
        "(", m$a, " &times; ", m$d, ") / (", m$b, " &times; ", m$c, ") = <strong>", round(m$or, 2), "</strong></p></div>"
      ))
    }
    
    if (!input$conf_adjust) {
      build_html_table("CRUDE ANALYSIS (Unadjusted)", res$crude, "text-danger")
    } else {
      HTML(paste0(
        "<div class='row'>",
        "<div class='col-6'>", build_html_table("STRATUM 1: SMOKERS", res$smokers, "text-success"), "</div>",
        "<div class='col-6'>", build_html_table("STRATUM 2: NON-SMOKERS", res$non, "text-success"), "</div>",
        "</div>"
      ))
    }
  })
  
  output$plot_confounding <- renderPlot({
    df <- conf_data()
    
    if (!input$conf_adjust) {
      plot_df <- df %>% count(Coffee, HeartDisease) %>%
        group_by(Coffee) %>% mutate(Risk = n / sum(n)) %>% filter(HeartDisease == "Case")
      
      ggplot(plot_df, aes(x = Coffee, y = Risk, fill = Coffee)) +
        geom_bar(stat = "identity", width = 0.5, color="black") +
        scale_fill_manual(values = c("Heavy Drinker" = "#e74c3c", "Low/None" = "#bdc3c7")) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.25)) +
        theme_minimal(base_size = 14) +
        labs(title = "Crude Heart Disease Risk (Unadjusted)", y = "Absolute Risk (%)", x = "Coffee Consumption") +
        theme(legend.position = "none")
    } else {
      plot_df <- df %>% count(Smoking, Coffee, HeartDisease) %>%
        group_by(Smoking, Coffee) %>% mutate(Risk = n / sum(n)) %>% filter(HeartDisease == "Case")
      
      ggplot(plot_df, aes(x = Coffee, y = Risk, fill = Coffee)) +
        geom_bar(stat = "identity", position = "dodge", color="black", width = 0.6) +
        facet_wrap(~Smoking) +
        scale_fill_manual(values = c("Heavy Drinker" = "#3498db", "Low/None" = "#95a5a6")) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.25)) +
        theme_minimal(base_size = 14) +
        labs(title = "Stratified Heart Disease Risk (Adjusted for Smoking)", y = "Absolute Risk (%)", x = "Coffee Consumption") +
        theme(legend.position = "none", strip.background = element_rect(fill="#2c3e50", color=NA), strip.text = element_text(color="white", face="bold"))
    }
  })
  
  # ==========================================
  # TAB 1 - SUB-TAB 2: OVERADJUSTMENT 
  # ==========================================
  output$diagramUI <- renderUI({
    is_adj <- input$adjustToggle
    
    path_class <- ifelse(is_adj, "sim-line blocked-path", "sim-line")
    arrow_class <- ifelse(is_adj, "blocked-arrow", "sim-arrow")
    
    HTML(paste0('
      <div class="diagram">
          <svg class="sim-svg">
              <defs>
                  <marker id="arrowhead-', is_adj, '" markerWidth="10" markerHeight="7" refX="9" refY="3.5" orient="auto">
                      <polygon points="0 0, 10 3.5, 0 7" class="', arrow_class, '" />
                  </marker>
                  <marker id="arrowhead-direct" markerWidth="10" markerHeight="7" refX="9" refY="3.5" orient="auto">
                      <polygon points="0 0, 10 3.5, 0 7" class="sim-arrow" />
                  </marker>
              </defs>
              
              <!-- Direct Path (Always active) -->
              <line x1="80" y1="125" x2="520" y2="125" class="sim-line" marker-end="url(#arrowhead-direct)" />
              <text x="300" y="145" font-family="sans-serif" font-size="14" fill="#555" text-anchor="middle">Direct Effect</text>
  
              <!-- Indirect Paths -->
              <line x1="80" y1="105" x2="250" y2="45" class="', path_class, '" marker-end="url(#arrowhead-', is_adj, ')" />
              <line x1="370" y1="45" x2="520" y2="105" class="', path_class, '" marker-end="url(#arrowhead-', is_adj, ')" />
              <text x="300" y="15" font-family="sans-serif" font-size="14" fill="#c0392b" text-anchor="middle">Indirect Effect via BP</text>
          </svg>
          
          <!-- Conditional Visual Block X -->
          ', ifelse(is_adj, '<div class="block-x">✖</div>', ''), '
  
          <!-- Nodes -->
          <div id="node-bmi" class="node">Exposure<br>(BMI)</div>
          <div id="node-bp" class="node">Mediator<br>(Blood Pressure)</div>
          <div id="node-esrd" class="node">Outcome<br>(ESRD)</div>
      </div>
    '))
  })
  
  output$statsUI <- renderUI({
    is_adj <- input$adjustToggle
    
    state_text <- ifelse(is_adj, "Adjusted (Blocked)", "Unadjusted (Total)")
    state_color <- ifelse(is_adj, "#c0392b", "#27ae60")
    rr_value <- ifelse(is_adj, "4.68", "6.12")
    
    HTML(paste0('
      <div class="stats-box">
          <div class="stat-box">
              Model State:
              <span class="stat-value" style="color: ', state_color, ';">', state_text, '</span>
          </div>
          <div class="stat-box">
              Observed Relative Risk:
              <span class="stat-value" style="color: #2980b9;">', rr_value, '</span>
          </div>
      </div>
    '))
  })
  
  # ==========================================
  # TAB 2 - SCENARIO 1: SMOKING & MELANOMA
  # ==========================================
  s1_data <- reactive({
    set.seed(111)
    n <- 20000 
    p_smoke <- input$s1_smoke_rate / 100
    smoking <- rbinom(n, 1, p_smoke)
    melanoma <- rbinom(n, 1, 0.05) 
    
    p_hosp_smoke <- input$s1_hosp_smoke / 100
    p_hosp_non <- input$s1_hosp_non / 100
    prob_hosp <- ifelse(melanoma == 1, 1.0, ifelse(smoking == 1, p_hosp_smoke, p_hosp_non))
    
    hospitalized <- rbinom(n, 1, prob_hosp)
    data.frame(
      Smoking = factor(smoking, levels = c(1, 0), labels = c("Smoker", "Non-Smoker")),
      Melanoma = factor(melanoma, levels = c(1, 0), labels = c("Case", "Control")),
      Hospitalized = factor(hospitalized, levels = c(1, 0), labels = c("Hospital", "Community"))
    )
  })
  
  s1_calc <- reactive({
    df <- s1_data()
    tbl_comm <- table(df$Smoking, df$Melanoma)
    or_comm <- (tbl_comm[1,1] * tbl_comm[2,2]) / (tbl_comm[1,2] * tbl_comm[2,1])
    
    df_hosp <- subset(df, Hospitalized == "Hospital")
    tbl_hosp <- table(df_hosp$Smoking, df_hosp$Melanoma)
    or_hosp <- (tbl_hosp[1,1] * tbl_hosp[2,2]) / (tbl_hosp[1,2] * tbl_hosp[2,1])
    
    list(tbl_comm = tbl_comm, tbl_hosp = tbl_hosp, or_comm = or_comm, or_hosp = or_hosp)
  })
  
  output$s1_dynamic_text <- renderUI({
    res <- s1_calc()
    HTML(paste0(
      "<div style='color: #2c3e50;'><strong>Community True OR: </strong>", round(res$or_comm, 2), " <em>(Independent)</em></div>",
      "<div style='color: #c0392b; margin-top: 10px; font-size: 1.1em;'><strong>Hospital Sample OR: </strong>", round(res$or_hosp, 2), " (Spurious Protective)</div>"
    ))
  })
  
  output$s1_tables <- renderUI({
    res <- s1_calc()
    HTML(paste0(
      "<div style='font-size: 0.85em;'><div class='row'>",
      "<div class='col-6'><strong>Community (True Truth)</strong><br>",
      "<table class='table table-sm table-bordered'>",
      "<tr class='table-light'><th></th><th>Case (a/c)</th><th>Control (b/d)</th></tr>",
      "<tr><th>Smoker (Exp)</th><td>", res$tbl_comm[1,1], "</td><td>", res$tbl_comm[1,2], "</td></tr>",
      "<tr><th>Non (Unexp)</th><td>", res$tbl_comm[2,1], "</td><td>", res$tbl_comm[2,2], "</td></tr>",
      "</table></div>",
      "<div class='col-6'><strong class='text-danger'>Hospital (Biased)</strong><br>",
      "<table class='table table-sm table-bordered'>",
      "<tr class='table-light'><th></th><th>Case (a/c)</th><th>Control (b/d)</th></tr>",
      "<tr><th>Smoker (Exp)</th><td>", res$tbl_hosp[1,1], "</td><td>", res$tbl_hosp[1,2], "</td></tr>",
      "<tr><th>Non (Unexp)</th><td>", res$tbl_hosp[2,1], "</td><td>", res$tbl_hosp[2,2], "</td></tr>",
      "</table></div></div></div>"
    ))
  })
  
  output$plot_s1 <- renderPlot({
    df <- s1_data()
    plot_df <- df %>% count(Hospitalized, Melanoma, Smoking) %>% group_by(Hospitalized, Melanoma) %>% mutate(prop = n / sum(n))
    ggplot(plot_df, aes(x = Melanoma, y = prop, fill = Smoking)) + geom_bar(stat = "identity", position = "dodge", color="black", linewidth=0.2) +
      facet_wrap(~Hospitalized) + scale_fill_manual(values = c("Smoker" = "#e74c3c", "Non-Smoker" = "#ecf0f1")) + scale_y_continuous(labels = scales::percent) + theme_minimal(base_size = 14) +
      labs(title = "Smoking Rate in Melanoma Cases vs Controls", y = "% Smokers", x = "") + theme(legend.position = "bottom", strip.background = element_rect(fill="#34495e", color=NA), strip.text = element_text(color="white", face="bold"))
  })
  
  # ==========================================
  # TAB 2 - SCENARIO 2: COFFEE & CANCER
  # ==========================================
  s2_data <- reactive({
    set.seed(222)
    n <- 20000 
    cancer <- rbinom(n, 1, 0.02)
    ulcer <- rbinom(n, 1, 0.10) 
    p_norm <- input$s2_coffee_norm / 100
    p_ulcer <- input$s2_coffee_ulcer / 100
    coffee <- ifelse(ulcer == 1, rbinom(n, 1, p_ulcer), rbinom(n, 1, p_norm))
    p_hosp_ulcer <- input$s2_hosp_ulcer / 100
    prob_hosp <- ifelse(cancer == 1, 1.0, ifelse(ulcer == 1, p_hosp_ulcer, 0.05)) 
    hospitalized <- rbinom(n, 1, prob_hosp)
    data.frame(Coffee = factor(coffee, levels = c(1, 0), labels = c("Drinker", "Non-Drinker")), Cancer = factor(cancer, levels = c(1, 0), labels = c("Case", "Control")), Hospitalized = factor(hospitalized, levels = c(1, 0), labels = c("Hospital", "Community")))
  })
  
  s2_calc <- reactive({
    df <- s2_data()
    tbl_comm <- table(df$Coffee, df$Cancer)
    or_comm <- (tbl_comm[1,1] * tbl_comm[2,2]) / (tbl_comm[1,2] * tbl_comm[2,1])
    df_hosp <- subset(df, Hospitalized == "Hospital")
    tbl_hosp <- table(df_hosp$Coffee, df_hosp$Cancer)
    or_hosp <- (tbl_hosp[1,1] * tbl_hosp[2,2]) / (tbl_hosp[1,2] * tbl_hosp[2,1])
    list(tbl_comm = tbl_comm, tbl_hosp = tbl_hosp, or_comm = or_comm, or_hosp = or_hosp)
  })
  
  output$s2_dynamic_text <- renderUI({
    res <- s2_calc()
    HTML(paste0(
      "<div style='color: #2c3e50;'><strong>Community True OR: </strong>", round(res$or_comm, 2), " <em>(Independent)</em></div>",
      "<div style='color: #c0392b; margin-top: 10px; font-size: 1.1em;'><strong>Hospital Sample OR: </strong>", round(res$or_hosp, 2), " (Spurious Harmful)</div>"
    ))
  })
  
  output$s2_tables <- renderUI({
    res <- s2_calc()
    HTML(paste0(
      "<div style='font-size: 0.85em;'><div class='row'>",
      "<div class='col-6'><strong>Community (True Truth)</strong><br>",
      "<table class='table table-sm table-bordered'>",
      "<tr class='table-light'><th></th><th>Case (a/c)</th><th>Control (b/d)</th></tr>",
      "<tr><th>Coffee (Exp)</th><td>", res$tbl_comm[1,1], "</td><td>", res$tbl_comm[1,2], "</td></tr>",
      "<tr><th>Non (Unexp)</th><td>", res$tbl_comm[2,1], "</td><td>", res$tbl_comm[2,2], "</td></tr>",
      "</table></div>",
      "<div class='col-6'><strong class='text-danger'>Hospital (Biased)</strong><br>",
      "<table class='table table-sm table-bordered'>",
      "<tr class='table-light'><th></th><th>Case (a/c)</th><th>Control (b/d)</th></tr>",
      "<tr><th>Coffee (Exp)</th><td>", res$tbl_hosp[1,1], "</td><td>", res$tbl_hosp[1,2], "</td></tr>",
      "<tr><th>Non (Unexp)</th><td>", res$tbl_hosp[2,1], "</td><td>", res$tbl_hosp[2,2], "</td></tr>",
      "</table></div></div></div>"
    ))
  })
  
  output$plot_s2 <- renderPlot({
    df <- s2_data()
    plot_df <- df %>% count(Hospitalized, Cancer, Coffee) %>% group_by(Hospitalized, Cancer) %>% mutate(prop = n / sum(n))
    ggplot(plot_df, aes(x = Cancer, y = prop, fill = Coffee)) + geom_bar(stat = "identity", position = "dodge", color="black", linewidth=0.2) +
      facet_wrap(~Hospitalized) + scale_fill_manual(values = c("Drinker" = "#8e44ad", "Non-Drinker" = "#ecf0f1")) + scale_y_continuous(labels = scales::percent) + theme_minimal(base_size = 14) +
      labs(title = "Coffee Drinking in Cancer Cases vs Controls", y = "% Coffee Drinkers", x = "") + theme(legend.position = "bottom", strip.background = element_rect(fill="#34495e", color=NA), strip.text = element_text(color="white", face="bold"))
  })
  
  # ==========================================
  # TAB 2 - SCENARIO 3: LOW BIRTH WEIGHT
  # ==========================================
  lbw_data <- reactive({
    set.seed(888)
    n <- 1000
    smoking <- runif(n, 0, 20) 
    lethal_defect <- rnorm(n, 50, 15) 
    birth_weight <- 3800 - (30 * smoking) - (15 * lethal_defect) + rnorm(n, 0, 200)
    is_lbw <- ifelse(birth_weight < 2500, "Low Birth Weight (<2.5kg)", "Normal Weight")
    data.frame(smoking, lethal_defect, birth_weight, is_lbw)
  })
  
  output$lbw_dynamic_text <- renderUI({
    df <- lbw_data()
    if (input$lbw_filter) {
      df_lbw <- subset(df, is_lbw == "Low Birth Weight (<2.5kg)")
      r_lbw <- cor(df_lbw$smoking, df_lbw$lethal_defect)
      HTML(paste0("<div style='color: #c0392b;'><strong>SPURIOUS NEGATIVE CORRELATION</strong><br><strong>LBW Babies Only (r = ", round(r_lbw, 2), ")</strong><br><br>Among small babies, higher smoking correlates with <em>lower</em> severe genetic risk. This falsely suggests smoking protects against lethal defects!</div>"))
    } else {
      r_gen <- cor(df$smoking, df$lethal_defect)
      HTML(paste0("<div style='color: #27ae60;'><strong>TRUE INDEPENDENCE</strong><br><strong>All Births (r = ", round(r_gen, 2), ")</strong><br><br>In the total population, a mother's smoking status has no relation to random genetic defects.</div>"))
    }
  })
  
  output$plot_lbw <- renderPlot({
    df <- lbw_data()
    p <- ggplot(df, aes(x = smoking, y = lethal_defect)) + theme_minimal(base_size = 15) + labs(x = "Maternal Smoking (Cigarettes/Day)", y = "Unmeasured Lethal Defect (Severity)") + coord_cartesian(xlim = c(0, 20), ylim = c(0, 100))
    if (input$lbw_filter) {
      ggplot(subset(df, is_lbw == "Low Birth Weight (<2.5kg)"), aes(x = smoking, y = lethal_defect)) + geom_point(color = "#e74c3c", size = 3, alpha = 0.7) + geom_smooth(method = "lm", se = FALSE, color = "#c0392b", linewidth = 1.5) + theme_minimal(base_size = 15) + labs(x = "Maternal Smoking (Cigarettes/Day)", y = "Unmeasured Lethal Defect (Severity)", title = "View: ONLY Low Birth Weight Babies") + coord_cartesian(xlim = c(0, 20), ylim = c(0, 100))
    } else {
      p + geom_point(aes(color = is_lbw), size = 2, alpha = 0.5) + geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", linewidth = 1.5) + scale_color_manual(values = c("Low Birth Weight (<2.5kg)" = "#e74c3c", "Normal Weight" = "#bdc3c7")) + theme(legend.position = "bottom", legend.title = element_blank()) + labs(title = "View: All Babies (General Population)")
    }
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
      HTML(paste0("<div style='color: #d35400;'><strong>SPURIOUS CORRELATION</strong><br><strong>Actors Only (r = ", round(r_actor, 2), ")</strong><br><br>Conditioning on a collider (Actor status) forces the traits to become negatively correlated.</div>"))
    } else {
      r_gen <- cor(df_col$talent, df_col$looks)
      HTML(paste0("<div style='color: #27ae60;'><strong>TRUE INDEPENDENCE</strong><br><strong>General Population (r = ", round(r_gen, 2), ")</strong><br><br>At birth, talent and looks are completely independent.</div>"))
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
      HTML(paste0("<div style='color: #8e44ad;'><strong>ATTENUATED EFFECT</strong><br><strong>Observed Effect: +", round(slope, 2), "</strong><br><br>As noise increases, the slope flattens out (attenuates toward the null).</div>"))
    }
  })
  
  output$plot_measurement <- renderPlot({
    df <- meas_data()
    ggplot(df, aes(x = measured_bp, y = true_risk)) + geom_point(color = "#9b59b6", size = 3, alpha = 0.6) + geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", linewidth = 1.5) + theme_minimal(base_size = 16) + labs(x = "Measured Blood Pressure (with error)", y = "Stroke Risk Score") + coord_cartesian(xlim = c(60, 180), ylim = c(20, 100))
  })
}

shinyApp(ui = ui, server = server)