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
    titlePanel("Ordered vs. Multinomial Models"), 
    theme = shinythemes::shinytheme("simplex"),
    
    # Start the tabs
    tabsetPanel(
        # Main ====
        tabPanel("Main", value="main",
            sidebarLayout(
                sidebarPanel(
                    numericInput("seed", label = "Random Seed", value = 101120, min = 1),
                    br(),
					sliderInput("n", label = "# of Subjects \\(\\left(n \\right) \\)", 
                                min = 50, max = 1000, step = 50, value = 300),
                    sliderInput("reps", label = "# of Simulations", 
                                min = 50, max = 1000, step = 50, value = 200), 
                    br(),
                    
					# To break up the monotony
                    div(style="border-radius:5px; background-color:#e0e0e0;
                               padding: 5px 10px 10px;",
						h4("DGP Options:"),
						radioButtons("dgp", "Observations:",
									 c("Ordered"= "ord_dgp",
									   "Unordered" = "mnl_dgp"),
									 selected = "ord_dgp"),
						#Only show this panel if Ordered
						conditionalPanel(
							condition = "input.dgp == 'ord_dgp'",
							
							disabled(sliderInput("aHat", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
												 min = -1, max = 1, step = .1, value = 0)),
							sliderInput("bHat", label = "Slope \\(\\left(\\beta \\right) \\)", 
										min = -1, max = 1, step = .1, value = .5),
							
							bsTooltip("aHat", "Fixed at 0 for identifiability.")
						),
						#Only show this panel if Unordered
						conditionalPanel(
							condition = "input.dgp == 'mnl_dgp'",
							
							sliderInput("A_aHat", label = "Intercept CatA (compared to CatC) \\(\\left(\\alpha_A \\right) \\)", 
										min = -1, max = 1, step = .1, value = .2),
							sliderInput("A_bHat", label = "Slope CatA (compared to CatC) \\(\\left(\\beta_A \\right) \\)", 
										min = -1, max = 1, step = .1, value = .5),
							sliderInput("B_aHat", label = "Intercept CatB (compared to CatC) \\(\\left(\\alpha_B \\right) \\)", 
										min = -1, max = 1, step = .1, value = -.2),
							sliderInput("B_bHat", label = "Slope CatB (compared to CatC) \\(\\left(\\beta_B \\right) \\)", 
										min = -1, max = 1, step = .1, value = .75)
							
						)
					),
                    
                    br(), br(),
                    
                    h4("Model Options:") ,
                    radioButtons("model", "Models:",
                                 c("Ordered Logit" = "ord_model",
                                   "Unordered Logit" = "mnl_model")),
                    
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
                        h4("Simulation Results"),
                        tableOutput("ord_only"),
                        tableOutput("mnl_only"),
                        tableOutput("mdta_omod"),
                        
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
                          c("aHat", "b1Hat", "b2Hat", "Shape (pHat)"))
           
            ),
            mainPanel(
                h4(class="simFyiHdr", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
                plotOutput("dist")
            )
                        
        ),
        
        # What should I see? ====
        tabPanel("What should I see?", value="expl",   
            uiOutput("wsis")
        )
    )
)