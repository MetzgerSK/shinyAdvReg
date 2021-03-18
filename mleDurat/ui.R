# UI START
ui <- fluidPage(
    
    # Loading up the CSS
    includeCSS("style.css"),
    includeCSS("bsCallout.css"),
    
    # To force the MathJax to wrap (correctly, this time)
    tags$head(tags$script(type = "text/x-mathjax-config", 
                            'MathJax.Hub.Config({
      "HTML-CSS": { linebreaks: { automatic: true } },
             SVG: { linebreaks: { automatic: true } }
    });')),

    # Title, theme
    titlePanel(HTML("MLE: The Intuition - Duration Models" )), 
    theme = shinythemes::shinytheme("sandstone"),  
    
    # -- Any "enable this thing" functions --
    useShinyjs(),   
    withMathJax(),  
    
    # Add the FYI about double-clicking on n
    tags$script("
        $(document).ready(function() {
            setTimeout(function() {
                shinyBS.addTooltip(\'nObs\', \'tooltip\', 
                    {\'placement\': \'bottom\', \'trigger\': \'hover\', \'title\': 
                     \'Double click to change min/max.\'
                })
            }
            , 500)
        });
    "),
    
    tabsetPanel(     
        tabPanel("Main", value="main_true",
            sidebarLayout(
                sidebarPanel(
                   # Generate fake data
                    h3("Start by generating some fake data", style="margin-top:0px;"),
                    numericInput("seed", label = "Random Seed", 
                                 min=1, value = 18),
                    sliderInput("nObs", label = "# of Subjects \\(\\left(n \\right) \\)",
                                min = 25, max = 500, step = 25, value = 150),
                    actionButton("dataGenButton", "Generate Fake Data", icon("table"), 
                                 style=""),
                    
                    # Layout the victory conditions
                    conditionalPanel("input.dataGenButton != 0",
                        helpText("Refresh page to reset (can then set seed and generate new dataset)"),        
                        div(id="bHalf",
                            hidden(h3(id="inf_instrText", "Now: find the slope and intercept that gives you the largest \"point\" value")),
                            h3(id="formal_instrText", "Now: find the slope and intercept that gives you the largest log-likelihood value"),
                            sliderInput("aHat", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                                        min = -4, max = 4, step = 0.05, value = 1.5),
                            sliderInput("b1Hat", label = "x's Coeff \\(\\left(\\beta_1 \\right) \\)",
                                        min = -4, max = 4, step = 0.05, value = -0.75),
                            sliderInput("shape", label = "Shape Parameter \\(\\left(p \\right) \\)",
                                        min = 0.1, max = 3, step = 0.05, value = 1.1),
                            
                            # Reset/giving up
                            actionButton("resetButton", "Reset Sliders", icon("repeat"), 
                                         style="color: #000; background-color: #999; margin-bottom: 5px;"),
                            br(),
                            actionButton("solnButton", "Show Answer", icon("exclamation-triangle"), 
                                         style="color: #e5e5e5; background-color: #960000;")
                        )
                    )
                ),

        
                mainPanel(
                    absolutePanel(top = -50, right = 75, width = 75,
                        switchInput("lang", "Prose Style", value = TRUE, 
                                    onLabel="Informal", offLabel="Formal",  
                                    size="small", inline=TRUE)    
                    ),

                    conditionalPanel("input.dataGenButton != 0",
                        h4("Proposed line's equation:"),
                        uiOutput("eq_weib_lnT"),

                        div(id="wrapper_llhInfo",
                            conditionalPanel("input.solnButton==0",
                                div(id="inside_llh", class="ptTotal_llh",
                                    h4("Current point total:", id="inf_ptTotal"),
                                    h4("Current log-likelihood value:", id="formal_ptTotal"),
                                    uiOutput("llh")
                                )
                            ),
                            conditionalPanel("input.solnButton !=0",
                                br(),
                                HTML("<span class='resJump', style='font-size:1.3em;'>
                                        <i class='fas fa-angle-double-right'></i> Jump to 
                                        <a href='#wrapper_rslts'>actual results table</a>
                                     </span>")
                            ),
                            div(id="inside_restore", class="llhBtn",
                                circleButton("fullLLHButton", class="pulse", icon = icon("eye"), status = "default", size = "sm",
                                            style="background-color:#eee; color:#000;"),
                                bsTooltip("fullLLHButton", "<span> Full LLH expression <br/> (NOTE: math incoming) </span>",   
                                             placement = "right", trigger = "hover", 
                                             options = list(
                                                container = "body"
                                            )
                                          ),
                                
                                # Load up the model expl
                                source("ui_llh__popup.R", local=TRUE)$value
                            )
                        ),
                        hr(),
                        div(id="wrapper_llhInfo",                
                            div(id="inside_llh", class="allTime",
                                hidden(h4("Current all-time best guess:", id="inf_bestGuess", class="bestGuess")),
                                hidden(h4("Last best guess:", id="inf_bestGuess_ans", class="bestGuess")),  
                                h4("Maximum log-likelihood value (so far):", id="formal_bestGuess", class="bestGuess"),
                                hidden(h4("Best guess for maximum log-likelihood value:", id="formal_bestGuess_ans", class="bestGuess_ans")), 
                                uiOutput("bestGuess"),
                                
                                conditionalPanel("input.solnButton!=0",
                                    uiOutput("bestGuess_ests"),
                                    hidden(h4("Actual answer:", id="inf_actual", style="padding-top:3px;")),
                                    h4("Actual log-likelihood value:", id="formal_actual"),
                                    uiOutput("trueLLH"),
                                    uiOutput("trueEsts")
                                )
                            ),
                            div(id="inside_restore", style="margin-left: 5px;",
                                actionButton("restoreButton", "Restore", icon("upload"))
                            )
                        ),
                        br(),
                        fluidRow(
                            column(6,
                                div(id="step1", class="steps arrowRight",
                                    span(class="numberCircle", "1"),
                                    "Adjust line to data",
                                    br(),
                                    
                                ),
                                bsTooltip("step1", "Only once fit is decent, proceed to #2", placement="top"),
                                plotOutput("gph")
                            ),
                            column(6,
                                div(class="steps arrowRightInv",
                                    span(class="numberCircle", "2"),
                                    "Adjust widths to match"     
                                ),
                                plotOutput("gph.sigmaHat")
                            )
                        ),
                        br(),
                        
                        div(id="inside_explain",
                            actionButton("explainButton", "What's going on here?", icon("question-circle")),
                            
                            # Pop-up window with explanation about what's going on
                            source("ui_expl__popup.R", local=TRUE)$value
                        ),
                        
                        br(),

                        hidden(
                            div(id="wrapper_rslts",
                                verbatimTextOutput("modObj")
                            )
                        )
                    )
                )
            )
        ),
        tabPanel("Data", value="data_true",
                        h4(class="simFyiHdr", "NOTE: Must generate data first on 'Main' tab."),
            div(class="inline-block-center rawRes", 
                div(
                    shinyjs::hidden(downloadButton("downloadData", "Download Dataset"))
                )
            ),
            div(class="ibcOuter", # (keeping as sep ibc divs in case you revert DT in future)
                div(class="ibcInner", style="width:60%;",
                    DT::dataTableOutput("data_table")
                )
            )
        )
    )
)