# UI START
ui <- fluidPage(
    # Load up CSS: 
    includeCSS("style.css"),
    
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
                    div(id="dgpAll",
                        # Min/max for upper right, to make model controls easier to get at
                        div(id="dgpMin", class="iconBx", icon("window-minimize")),
                        bsTooltip("dgpMin", "Minimize", "right"),
                        div(id="dgpMax", class="iconBx", icon("window-maximize")),
                        bsTooltip("dgpMax", "Maximize", "right"),
                        
                        h4("DGP Options"),
                        div(id="dgpChunk",
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
                        )
                    ),
                    
                    br(),
                    
                    h4("Model Options"),
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
                    conditionalPanel(condition="input.goButton == 0", 
                        br(),br(),
                        p("Set values at left, click 'Run Simulation' button, and wait 15-90 seconds for results to appear.")
                    ),
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
        ),
        
        # Rootograms ====
        tabPanel("Model Fit: Rootograms",
            sidebarPanel(
                selectInput("rooto_mods", "Model",
                            c("Poisson"=1   , "Neg. Binomial"=2,
                              "ZI Poisson"=3, "ZI Neg. Binomial"=4)
                )
            ),
            mainPanel(style="margin-top:10px;",
                h4(class="simFyiHdr", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
                conditionalPanel("input.goButton>0",
                    fluidRow(
                        column(4,
                            sliderInput("selectSim_rooto", label = "Select Simulation Draw", 
                                        min = 1, max = 1000, step = 1, value = 1)
        						# ^ The max value for this updates in server.R, 
        						# right above the rootogram chunk based on the 
        						# value of input$reps for last goButton press
                        ),
    					column(8,
    					    HTML("
    					        <ul style='margin:0px;padding-left: 18px;'>
        					        <li><span style='font-weight:bold;color:#B61A51;'>Red Line</span>: Predicted counts (by model)
        					        <li><span style='font-weight:bold;color:#888888;'>Gray bars</span>: Actual counts (in dataset)
    					        </ul>
    					        <hr style='margin-top:5px;margin-bottom:5px;'>
    					        <em>y</em>-axis: number of observations whose DV equals this count value (= <em>x</em>-axis), square rooted"
    					   )
    					)
                    ),
                    plotOutput("rootogram"),
    					
    				h5(strong("Fit Interpretation")),
    				HTML("<ul>
        				    <li>If bars <em>touch</em> <em>y</em> = 0 line: model predicts <strong>correct</strong> number of observations with this count value ⇒ ideal scenario
        			        <li>If bars <em>above</em> <em>y</em> = 0 line: model predicts <strong>more</strong> observations with this count value than there actually are in the dataset
        			        <li>If bars <em>below</em> <em>y</em> = 0 line: model predicts <strong>fewer</strong> observations with this count value than there actually are in the dataset
    				     </ul>"
    				),
                )
            )
        )#,
        
        ## TO DO IN FUTURE
        # What should I see? ====
        # tabPanel("What should I see?", 
        #     h4(class="simFyiHdr", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
        #     uiOutput("wsis")
        # )
        #         
    )
)









