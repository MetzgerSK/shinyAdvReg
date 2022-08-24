# > SERVER FUNCTION ----
server <- function(input, output, session){
    
    # Dataset upload
    data <- reactive({
        req(input$fileUpload)
        
        if(regexpr("excel", input$fileUpload$type)>0){
            read.csv(input$fileUpload$datapath,
                 header = input$header,
                 sep = ",")
        } else {
            read_stata(input$fileUpload$datapath)   
        }
    })
    
    # Update var selection dropdowns, once dataset uploaded
    observe({
        updateVarSelectInput(session, "predQOI", data = data(),
                selected = NULL)
        
        updateVarSelectInput(session, "obsvY", data = data(),
                selected = NULL)
        
        shinyjs::showElement("postUpload")
    }, priority=20)
    
    # Warning about non-0/1 DV 
    observe({
        req(!is.null(input$obsvY))
        
        # Get currently selected variable
        obsvY <- paste0("data()$", input$obsvY) %>% parse(text=.) %>% eval
        
        # Show warning if selected var isn't 0/1 (yoinks heatmap.fit's validation code)
        shinyjs::toggleElement("dvWarn", 
                               condition=min(obsvY == 1 | obsvY == 0, na.rm=TRUE) == 0)
    })
    
    # Warning about pred values >1 or <0
    observe({
        req(!is.null(input$predQOI))
        
        # Get currently selected variable
        pred <- paste0("data()$", input$predQOI) %>% parse(text=.) %>% eval
        
        # Show warning if preds aren't valid pred probs (yoinks heatmap.fit's validation code)
        shinyjs::toggleElement("predPrWarn", 
                               condition=max(pred > 1 | pred < 0, na.rm=TRUE) == 1)
    })
    
    
    # STARTS HERE
    cmdOutpt <- eventReactive(input$runBtn, {
        set.seed(input$seed)
        
        dat <- data()
        
        obsvY <- paste0("dat$", isolate(input$obsvY))
        pred  <- paste0("dat$", isolate(input$predQOI))
        
        # Remove any NA rows, given selected obsvY, pred vars
        dat <- dat[complete.cases(eval(parse(text=obsvY)), 
                                  eval(parse(text=pred))  ) , ]
        
        # Empty the output div
        shinyjs::html(id = "rawOutpt", html = "")
        
        # Run the command, print the output live w/wCH
        withCallingHandlers({
            hmf.env <- heatmap.fitMod(eval(parse(text=obsvY)), 
                        eval(parse(text=pred)), reps=input$nBoots, 
                        color=input$color, legend=input$legend)
                # Stores function's entire environment @ execution's end
                    
            },
            message = function(m) {
              shinyjs::html(id = "rawOutpt", html = m$message, add = TRUE)
            }
        )

        # Return the environment
        return(hmf.env)
    })
   
    # Graph 
    output$gph.heat <- renderPlot({
        req(cmdOutpt())
        hmfPlot(cmdOutpt())
    })
    
    # Graph image export
    output$imgExport <- downloadHandler(
        filename = function(){
          paste("heatmapFit.png")
        },
        content = function(file){
          png(file)
          hmfPlot(cmdOutpt())
          dev.off()
        }
    )
   
    # Show hidden div once button clicked, since conditionalPanel being difficult
    observeEvent(input$runBtn, {
       shinyjs::showElement(id="mainRes")
    })
}