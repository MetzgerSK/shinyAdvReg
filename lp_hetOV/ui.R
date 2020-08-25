# UI START
ui <- fluidPage(
    # Load up CSS: 
    includeCSS("style.css"),
    includeCSS("bsCallout.css"),
    
    # Initialize 
    useShinyjs(),   
    withMathJax(), 
    
    # Title, theme
    titlePanel("Binary Models: Heteroskedasticity, Omitted Variables"), 
    theme=shinythemes::shinytheme("simplex"),
    
    # Start the tabs
    tabsetPanel(
        # Main ====
        tabPanel("Main", value="main",
            sidebarLayout(
                sidebarPanel(
                    numericInput("seed", label = "Random Seed", value = 170320, min = 1),
                    br(),
                    sliderInput("nObs", label = "# of Subjects \\(\\left(n \\right) \\)", 
                                min = 50, max = 1000, step = 50, value = 300),
                    sliderInput("reps", label = "Number of Simulations", 
                                min = 50, max = 1000, step = 50, value = 200), 
                    br(),
                    
                    # To break up the monotony
                    div(style="border-radius:5px; background-color:#e0e0e0;
                               padding: 5px 10px 10px;",
                        
                        # Min/max for upper right, to make model controls easier to get at
                        div(id="dgpMin", class="iconBx", icon("window-minimize")),
                        bsTooltip("dgpMin", "Minimize", "right"),
                        div(id="dgpMax", class="iconBx", icon("window-maximize")),
                        bsTooltip("dgpMax", "Maximize", "right"),
                        
                        h4("DGP Options"),
                        div(id="dgpChunk",
                            fluidRow(
                                column(5,
                                    radioButtons("dgp", "Link Function:",
                                                 c("Logit"= "l_dgp",
                                                   "Probit" = "pr_dgp"),
                                                 selected = "l_dgp")
                                ),
                                column(7,
                                    selectInput("het_dgp", "Heteroskedasticity?", 
                                                choices = c("Yes" = "het_dgp", 
                                                            "No" = "non_dgp"),
                                                selected= "non_dgp")
                                )
                            ),
                            
                            # The set of sliders to always show
                            sliderInput("aHat", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                                        min = -1, max = 1, step = .1, value = .2),
                            sliderInput("bHat", label = HTML("<em>x</em>'s Slope \\(\\left(\\beta_1 \\right) \\)"), 
                                        min = -1, max = 1, step = .1, value = .5),
                            sliderInput("b2Hat", label = HTML("<em>z</em>'s Slope \\(\\left(\\beta_2 \\right) \\)"), 
                                        min = -1, max = 1, step = .1, value = .3),
                            
                            #Only show this panel if Heteroskedastic
                            conditionalPanel(
                                condition = "input.het_dgp == 'het_dgp'",
                                p(style="margin:0px -3px 2px;",
                                  strong("Heteroskedasticity Equation")),
                                sliderInput("het_err", label = HTML("<em>x</em>'s Scale \\(\\left(\\gamma \\right) \\)"), 
                                            min = 0.1, max = 2, step = 0.1, value = 0.9),
                            )
                        )
                    ),
                        
                    br(), br(),
                    
                    h4("Model Options"),
                    fluidRow(
                        column(5,
                            radioButtons("model", "Models:",
                                         c("Logit" = "l_model",
                                           "Probit" = "pr_model"))
                        ),
                        column(7,
                            selectInput("het_model", "Heteroskedastic?",
                                        choices = c("Yes" = "het_model",
                                                    "No" = "non_model"),
                                        selected="non_model"),
                            selectInput("cov_model", "Omit Covariate?",
                                        choices = c("Yes" = "omit_model",
                                                    "No" = "cov_model"),
                                        selected="cov_model")
                        )
                    ),
                    
                    br(),
                    actionButton("goButton", "Run Simulation")
                ),
                
                mainPanel(
                    div(class="bs-callout bs-callout-info",
                        div(class="bs-close", icon("times-circle")),
                        HTML("<h4>No Correlation with <em>x</em> or <em>y</em></h4>"),
                        HTML("<strong>Can also demonstrate effect of including an irrelevant variable.  
                              Set (1) \\( \\beta_2 \\) to 0 \\( \\left( \\text{Corr}(x,z) = 0 \\right.\\) 
                              already for this scenario\\(\\left. \\right)\\) and (2) under Model Options, \"Omit Covariate?\"
                              to \"No\".</strong>")
                    ),
                    conditionalPanel(condition="input.goButton == 0", 
                        br(),br(),
                        p("Set values at left, click 'Run Simulation' button, and wait 15-90 seconds for results to appear.")
                    ),
                    conditionalPanel(condition="input.goButton > 0", 
                        h4("True DGP"),
                        uiOutput("eq_trueDGP"),
                        br(),
                        
                        h4("Estimated Model"),
                        uiOutput("eq_estmMod"),
                        br(),
                        
                        h4("Simulation Results"),
                        tableOutput("simRslts"),
                        
                        br(), br(),
                        downloadButton("datDwn", "Download a Fake Dataset", icon("file-download"),
                                       style="color:#EEE; background-color:#718C6A;"
                        )
                    )
                )
            )
        ),
        # Raw sim results ====
        tabPanel("Raw Simulation Output",
            h4(class="simFyiHdr", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            br(),
            dataTableOutput("raw")
        ),            
        
        # Sampling distros ====
        tabPanel("Estimates: Distribution Plots",
            sidebarPanel(
                selectInput("params", "Estimate",
                          c("aHat", "b1Hat"))   # Updates in server.R
           
            ),
            mainPanel(
                h4(class="simFyiHdr", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
                
                plotOutput("dist")
            )
        ) #,
        
        ## TO DO IN FUTURE
        # What should I see? ====
        # tabPanel("What should I see?", value="expl",   
        #     uiOutput("wsis")
        # )
        #  
                
    )
)









