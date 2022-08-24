ui <- fluidPage(

    # Load CSS
    includeCSS("style.css"),
    
    # Title, theme define
    titlePanel(HTML("Esarey and Pierce's Heatmap Fit Statistic")), 
    theme = shinythemes::shinytheme("cosmo"),  
    
    # Any "enable this thing" functions 
    useShinyjs(),   
    withMathJax(),  
    
    # Start content
    sidebarLayout(
        sidebarPanel(
            # Upload dataset
            fileInput("fileUpload", "Upload File (as .dta or .csv)",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".dta")
            ),
            
            checkboxInput("header", "Varnames in First Row?", TRUE),
            
            # Things to show after user's uploaded dataset
            shinyjs::hidden(div(id="postUpload", 
                # Dropdown for predicted quant
                varSelectInput("predQOI", label="Predicted DV \\((\\widehat{ \\Pr \\left( y=1 \\right) })\\)", 
                               data=NULL, multiple=FALSE),
                shinyjs::hidden(
                    div(id="predPrWarn", class="varWarn", style="",
                        HTML("<strong>Warning</strong>: invalid probability
                             values detected, will throw error.")
                    )
                ),

                # Dropdown for observed DV
                varSelectInput("obsvY", label="Observed DV \\((y)\\)", 
                               data=NULL, multiple=FALSE),
                shinyjs::hidden(
                    div(id="dvWarn", class="varWarn", style="",
                        HTML("<strong>Warning</strong>: non-binary DV detected, will throw error.")
                    )
                ),
                
                # Options for graph
                HTML('<span style="margin-left:-10px;"><i class="fas fa-list-alt" style="font-size:135%"></i> 
                     <span style="padding-left:3px;font-size:19px; font-weight:700;">Options</span></span>'),
                numericInput("seed", "Random Seed", 
                             min=1, max = 2^19937 - 1 , step=1, value=7609),
                numericInput("nBoots", "Number of Bootstraps", 
                             min=1, max = 20000 , step=1, value=1000),
                fluidRow(
                    column(6,
                        div(class="inline-block-center",
                            strong("Graph in color?"),
                            switchInput("color",
                                        onLabel="Yes", offLabel="No",
                                        size="small", value=FALSE)
                        )
                    ),
                    column(6,
                        div(class="inline-block-center",
                            strong("Show legend?"),
                            switchInput("legend",
                                        onLabel="Yes", offLabel="No",
                                        size="small", value=TRUE)
                        )
                    )
                ),
                br(),
                
                # Go button to generate
                div(class="inline-block-center",
                    actionButton("runBtn", "Generate", icon("chart-line"),
                                 style="color: #fff; background-color: #BB0000; border-color: #960000 !important;")
                )
            ))
        ),
        mainPanel(
            shinyjs::hidden(
            div(id="mainRes",
                
                # Raw output from heatmapFit 
                verbatimTextOutput("rawOutpt"), 

                # The graph
                plotOutput("gph.heat") %>% withSpinner(type = 6),
                
                # Export button for graph
                br(),
                div(class="inline-block-center",
                    downloadButton('imgExport', 'Download (PNG)')
                )
            ))
        )
    )

)