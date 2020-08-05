# UI START
ui <- fluidPage(
    # CSS: make h4 bolded
    tags$head(tags$style("
        h4{
          font-weight:bold;
        }
    ")),
    
    # Initialize
    useShinyjs(),   
    withMathJax(), 
    
    # Title, theme
    titlePanel("Count Models"), 
    theme=shinythemes::shinytheme("simplex"),
    
    # Start the tabs
    tabsetPanel(
        # Main ====
        tabPanel("Main", value="main",
            sidebarLayout(
                sidebarPanel(
                    numericInput("seed", label = "Random Seed", value = 201920, min = 1),
                    br(),
                    sliderInput("nObs", label = "# of Subjects \\(\\left(n \\right) \\)", 
                                min = 50, max = 1000, step = 50, value = 300),
                    sliderInput("reps", label = "# of Simulations", 
                                min = 50, max = 1000, step = 50, value = 200), 
                    br(),
                    
                    # To break up the monotony
                    div(style="border-radius:5px; background-color:#e0e0e0;
                               padding: 5px 10px 10px;",
                        h4("DGP Options:"),
                        fluidRow(
                            column(6,
                                radioButtons("dgp", "Count's Distribution:",
                                             c("Poisson"= "p_dgp",
                                               "Negative Binomial" = "nb_dgp"),
                                             selected = "p_dgp")
                            ),
                            column(6,
                                selectInput("z_dgp", "Zero-Inflated Component?", 
                                            choices = c("Yes" = "zi_dgp", 
                                                        "No" = "non_dgp"),
                                            selected= "non_dgp")
                            )
                        ),
                    
                        #Show this panel regardless
                        sliderInput("aHat", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                                    min = -1, max = 1, step = .1, value = .2),
                        sliderInput("bHat", label = HTML("<em>x</em>'s Slope \\(\\left(\\beta_x \\right) \\)"), 
                                    min = -1, max = 1, step = .1, value = .5),
                            
                        #Only show this panel if Negative Binomial Distribution
                        conditionalPanel(
                            condition = "input.dgp == 'nb_dgp'",
                            
                            sliderInput("dispers", label = "Dispersion \\(\\left(\\theta\\right)\\)", 
                                        min=.1, max=3, step=.25, value =.5) 
                        ),
                        #Only show this panel if Zero-Inflated
                        conditionalPanel(
                            condition = "input.z_dgp == 'zi_dgp'",
                            
                            sliderInput("b0z", label = "Inflation: Intercept", 
                                        min = -1, max =1, step = .1, value = .3),
                            sliderInput("b1z", label = HTML("Inflation: <em>z</em>'s Slope"), 
                                        min = -1, max =1, step = .1, value = .8)
                        )
                    ),
                    
                    br(), br(),
                    
                    h4("Model Options:"),
                    fluidRow(
                        column(6,
                            radioButtons("model", "Models:",
                                         c("Poisson" = "p_model",
                                           "Negative Binomial" = "nb_model"))
                        ),
                        column(6,
                            selectInput("z_model", "Zero-Inflated?",
                                        choices = c("Yes" = "zi_model",
                                                    "No" = "non_model"),
                                        selected="non_model")
                        )
                    ),
                    
                    br(),
                    actionButton("goButton", "Run Simulation")
                ),
                
                mainPanel(
                    conditionalPanel(condition="input.goButton > 0", 
                        h4("Overview"),
                        uiOutput("selections"),             
                                     
                        br(),
                        h4("True DGP"),
                        uiOutput("eqTrueDGP"),
                        
                        br(),
                        h4("Estm. Model"),
                        uiOutput("eqEstmMod"),
                        
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
                          c("aHat", "b1Hat"))
           
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









