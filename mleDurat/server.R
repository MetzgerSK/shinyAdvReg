# Graph font 
font <- "Roboto"


# > SERVER FUNCTION ----
server <- function(input, output, session){

    # So that the values persist
    bestSoFar <- reactiveValues(llh = NULL, # best guess for LLH value so far
                                alG = NULL, # intercept corresponding to best guess so far
                                b1G = NULL, # slope corresponding to best guess so far
                                pG  = NULL) # shape corresponding to best guess so far
    
    
    # > !!!! STARTS HERE (triggered by button press) !!!! < ----
    all <- eventReactive(input$dataGenButton, {
        # disable the data gen button (force user to reload the entire page for a fresh dataset)
        disable("dataGenButton")
        disable("sigmaMatch")
        disable("nObs")
        removeTooltip(session, "nObs")
        disable("seed")
        shinyjs::show("downloadData")
        shinyjs::hide(selector="h4.simFyiHdr")
        
        # Gen data
        dat <- dataGen()
        
        # Estimate
        mod <- survreg(Surv(t, fail) ~ x1, dist="weibull", dat=dat)
            
        # Pull truth
        coeffs <- c(coef(mod), shape=1/mod$scale)   # betas
        truth <- as.numeric(logLik(mod))            # true LLH
        
        # Return
        list(data.frame(dat), coeffs, mod, truth)
    })
    
    # > DATA-RELATED ----
    ## Generate data ====
    dataGen <- function() {
        set.seed(input$seed)
        
        aHatTrue  <- sample(-16:16, 1)/4      # intercept
        b1HatTrue <- sample(-16:16, 1)/4	  # for x
        shapeTrue <- sample(1:25, 1)/10		  # shape parameter
        censPerc  <- 1 - sample(0:25, 1)/100  # for amount of censoring

        x1 <- rnorm(input$nObs)
         
        e <- log(-log(1-runif(input$nObs)))              # TIEV (min) error
        t <- exp(aHatTrue + b1HatTrue*x1 + e/shapeTrue)  # supposes shapeTrue is inputted as p, meaning log-linear form needs to be 1/p
         
        # Censoring
        fail <- as.numeric(t<=quantile(t, censPerc))
        t[t>quantile(t, censPerc)] <- quantile(t, censPerc) 
        
        # Return the data frame
        data.frame(t, fail, x1)
    }
    
    ## LLH for current guess ====
    obsLLH <- reactive({
        # Get linear combo: use sliders if solution button's not 
        # pressed, but use actual model estimates if it is
        if(input$solnButton==0){
            xb <- (input$aHat + input$b1Hat*all()[[1]]$x1)
            sh <- input$shape
        } else {
            xb <- predict(all()[[3]], type="lp")
            sh <- 1/all()[[3]]$scale
        }
        
        obs <- ifelse(all()[[1]]$fail==1, 
                      dweibull(all()[[1]]$t, sh, exp(xb), log=TRUE), 
                      pweibull(all()[[1]]$t, sh, exp(xb), lower.tail=FALSE, log.p=TRUE))
        
        c(sum(obs), data.frame(obs))
    })  
    
    ## All-time Max LLH ====
    ## KEEP TRACK OF ALL-TIME MAX LLH
    allTime <- reactive({
        if(!is.null(bestSoFar$llh) & input$solnButton==0){
            if(max(bestSoFar$llh, obsLLH()[[1]])==obsLLH()[[1]]){
                bestSoFar$llh <- obsLLH()[[1]]
                bestSoFar$alG <- input$aHat
                bestSoFar$b1G <- input$b1Hat
                bestSoFar$pG  <- input$shape
            }
        } else if(is.null(bestSoFar$llh) & input$solnButton==0){
            bestSoFar$llh <- obsLLH()[[1]]
            bestSoFar$alG <- input$aHat
            bestSoFar$b1G <- input$b1Hat
            bestSoFar$pG  <- input$shape
        }
    })


    
#***********************************************************************
# > OUTPUT CODE ----
#***********************************************************************  
      
    # Eqs, CURRENT LINE EQ ====
    ## (in terms of ln(t)
    output$eq_weib_lnT <- renderUI({
        linCombCalc <- paste0(input$aHat, 
                              ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, "x + ",
                              input$shape, "^{-1} u")
        
        withMathJax(
            paste(
                '\\( \\ln{\\left(t \\mid x\\right)}= ', linCombCalc, 
                ' \\text{, where } u \\sim \\text{Type I Extreme Value (min.)} \\)'
            )
        )
        
    })
    
    # LLH MJ helper functs ====
    source("serverPt_llhHelpersMJ.R", local=TRUE)
    
    # Eqs, GENERIC LH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLH <- renderUI({
        
        # do in chunks
        linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, 'x_i')
        lambda <- paste0('\\exp \\left(', linComb, '\\right)')

        withMathJax(
            paste(
                '\\( L\\left( \\alpha, \\beta_1, p \\mid t_i, \\delta_i, x_i \\right) =',
                '\\prod\\limits_{i=1}^{', input$nObs, '} \\left\\{ 
                    \\underbrace{\\left[',  
                        weibDenMJ(shape=input$shape, scale=lambda, t="t_i"), 
                        '\\right]^{\\delta_i}}_',
                        '{f{(t)} \\text{, observed failure } (\\delta_i=1)} *
                    \\underbrace{\\left[', 
                        weibSurvMJ(shape=input$shape, scale=lambda, t="t_i"), 
                        '\\right]^\\left( 1 - \\delta_i \\right)}_',
                        '{S{(t)} \\text{, right censored } ( \\delta_i=0)} \\right\\} \\)'
            )
        )
    })
    
    # Eqs, GENERIC LLH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLLH <- renderUI({
        
        linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, 'x_i')
        lambda <- paste0('\\exp \\left(', linComb, '\\right)')
    
        withMathJax(
            paste(
                '\\( \\ln L\\left( \\alpha, \\beta_1, p \\mid t_i, \\delta_i, x_i \\right) = 
                \\sum\\limits_{i=1}^{', input$nObs, '} \\left\\{
                    \\underbrace{ \\left[ \\delta_i* \\ln \\left(', 
                        weibDenMJ(shape="p", scale=lambda, t="t_i"), '\\right) \\right]}_', 
                        '{f{(t)} \\text{, observed failure } (\\delta_i=1)} +
                    \\underbrace{ \\left[ \\left( 1 - \\delta_i \\right) * \\ln \\left(', 
                        weibSurvMJ(shape="p", scale=lambda, t="t_i"), ' \\right) \\right]}_',
                        '{S{(t)} \\text{, right censored } ( \\delta_i=0)} \\right\\}\\)'
            )

        )
    })

    # Eqs, SPECIFIC LLH (SIMPL, EACH OBS) ====
    output$eq_fullLLH_all <- renderUI({
        # LHS line openers
        intro <- paste0('\\ln L\\left( \\alpha='   , input$aHat, 
                                    ',~\\beta_1 = ', input$b1Hat, 
                                    ', p ='        , input$shape, 
                                    '\\mid t_i, \\delta_i, x_i \\right) =')
        
        equation <- paste0('\\(  ', intro)
   
        # Loop through every single observation      
        for(i in 1:input$nObs){
            # rounded t
            tRound <- round(all()[[1]][i,"t"], 3)
            
            # do linear combo, numerator, denominator (in parts, to make syntax errors easier to spot)
            lambda <- lambdaMJ(round(all()[[1]][i,"x1"], 3))

            # insert either density or survivor, depending on whether fail or censored
            equation <- paste0(equation, ifelse(i!=1, "+", ""), ' \\left. \\ln \\left( ', 
                                ifelse(all()[[1]][i,"fail"]==1, 
                                    weibDenMJ(scale=lambda, shape=input$shape, t=tRound), 
                                   weibSurvMJ(scale=lambda, shape=input$shape, t=tRound)
                                ), ' \\right) \\right.') 
        }
            
        # Insert alignment break
        equation <- paste0(equation, ' \\\\~')
        
        # Insert LLH value
        llh <- ifelse(input$solnButton==0, round(obsLLH()[[1]], 5), round(all()[[4]],5))
        equation <- paste0(equation, intro, llh)

        # Put MathJax closer tag
        equation <- paste0(equation, '  \\)')
    
        # Print it
        withMathJax(
            equation
        )
    })
    
    # Eqs, SPECIFIC LLH (UNSIMPL, EACH OBS) ====
    output$eq_fullLLH_all_unsimp <- renderUI({
        # LHS line openers
        intro <- paste0('\\ln L\\left( \\alpha='   , input$aHat, 
                                    ',~\\beta_1 = ', input$b1Hat, 
                                    ', p ='        , input$shape, 
                                    '\\mid t_i, \\delta_i, x_i \\right) =')
        
        equation <- paste0('\\(  ', intro)
   
        # Loop through every single observation      
        for(i in 1:input$nObs){
            # rounded t
            tRound <- round(all()[[1]][i,"t"], 3)
            
            # do linear combo, numerator, denominator (in parts, to make syntax errors easier to spot)
            lambda <- lambdaMJ(round(all()[[1]][i,"x1"], 3))
            
            # get proper sign depending on whether the DV for this obsv is a 0 or 1 with ifelses
            equation <- paste0(equation, ifelse(i!=1, "+", " \\\\ ~~ "), 
                               '\\left\\{', all()[[1]][i,"fail"], '* \\ln\\left[ ', weibDenMJ(scale=lambda, shape=input$shape, t=tRound, breaks=TRUE), ' \\right] 
                               + \\left(1-', all()[[1]][i,"fail"], '\\right) * \\ln\\left[ ', weibSurvMJ(scale=lambda, shape=input$shape, t=tRound), ' \\right]  \\right\\} ') 
        }
            
        # Insert alignment break
        equation <- paste0(equation, ' \\\\~')
        
        # Insert LLH value
        llh <- ifelse(input$solnButton==0, round(obsLLH()[[1]], 5), round(all()[[4]],5))
        equation <- paste0(equation, intro, llh)

        # Put MathJax closer tag
        equation <- paste0(equation, '  \\)')
    
        # Print it
        withMathJax(
            equation
        )
    })
    
    # MAIN GRAPH ====
    output$gph<- renderPlot({   

        # Orig data 
        dat <- all()[[1]]
        
        # Do pred graph quick (b/c ln(t) = XB)
        tHat <- data.frame(x1=dat$x1) %>%
                    mutate(tHat=(input$aHat + input$b1Hat*x1))        
        
        # graph
        ggplot(data=dat) +
                geom_point(aes(x=x1, y=log(t)), color="blue", alpha=0.9) +
                geom_line(data=tHat, aes(x=x1, y=tHat), color="red") +
            labs(title="x vs. ln(t)",
                 x="x", 
                 y="ln(t)") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", family = paste0(font))
                 ) 

    })
    
    # SHAPE GRAPH ====
    output$gph.sigmaHat <- renderPlot({
        # Get residuals for all points, based on current proposed line
        xb <- (input$aHat + input$b1Hat*all()[[1]]$x1)
        obs <- (log(all()[[1]]$t) - xb)

        # Plot these points
        ggplot(mapping=aes(x=obs)) + 
            geom_histogram(aes(y=..density..), fill = "gray", color = "black") +
            geom_density(alpha=.2, size = 1.05, color="#104E8B") + 
            geom_rug(alpha=0.5) +  
            stat_function(fun = dgumbmin, args = list(location = 0, scale = 1/input$shape),
                          color = "#FF4040", size = 1, linetype="dashed",
                          inherit.aes=FALSE) + 
            labs(title = "Distance between ln(tHat) and ln(t)",
                 x     = "Distance (uHat)") + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold", family = paste0(font))
                 )
            })
    
    # OUTPUT: Print the actual model    
    output$modObj <- renderPrint({ 
        summary(all()[[3]]) 
    })
    
    ## OUTPUT: PRINT CURRENT GUESS' LLH VALUE
    output$llh <- renderUI({
        withMathJax(
            paste0('\\(', round(obsLLH()[[1]], 5), '\\)')
        )
    })
    
    # OUTPUT: PRINT ALL-TIME MAX LLH
    output$bestGuess <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\(', round(bestSoFar$llh, 5), '\\)')
        )
    })
    
    # OUTPUT: PRINT ESTIMATES FOR ALL-TIME MAX LLH
    output$bestGuess_ests <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\( \\alpha =',  bestSoFar$alG, 
                     ',~\\beta_1 =', bestSoFar$b1G, 
                     ',~p =',        bestSoFar$pG, ' \\)')
        )
    })
    
    # OUTPUT: PRINT TRUE SLOPE + INTERCEPT + SHAPE (BASED ON ESTIMATED MODEL) ====
    output$trueEsts <- renderUI({
        withMathJax(
            paste0('\\( \\alpha =', round(all()[[2]][1], 3), 
                     ',~\\beta_1 =', round(all()[[2]][2], 3), 
                     ',~p =', round(all()[[2]][3], 3), '\\)')
        )
    })
    
    # OUTPUT: PRINT ANSWER + TRUE LLH ====
    output$trueLLH <- renderUI({ 
        withMathJax(
            paste0('\\(', round(all()[[4]], 5), ' \\)')
        )
    })
    
    # DT: ACTUAL DATASET ====
    output$data_table <- DT::renderDataTable({
        dat <- all()[[1]]
        llh <- round(obsLLH()[[1]], 3)
        dat$logLH_i <- obsLLH()[[2]]
        n  <- input$nObs # avoid subset error
        
        # Mod a table 
        sketch <- htmltools::withTags(table(
            class = 'display',
            
            # header
            DT::tableHeader(dat), 
            
            # footer
            tfoot(
                tr(
                    th(colspan="4", paste0("TOTAL (across all ", n, " observations): ", 
                                           round(sum(dat$logLH_i),5) )) 
                )
            )
        ))
        
        # Print it
        DT::datatable(dat,
                      escape=TRUE,
                      rownames=FALSE,
                      filter = "none",
                      container = sketch, 
                          options = list(
                              pageLength = ifelse(n==25, 25, 15),
                              dom = ifelse(n<=25, 't', 'ltp'), # hide length + page selector if less than 25, show othw (though see also autoHideNavigation)
                              columnDefs = list(
                                  list(className = 'dt-center', targets = c(0))
                              )
                          )
            ) %>% 
              DT::formatRound(c(1, 3:ncol(dat)), 4) # for t, x1, logLH_i
              
    })
    
    # Downloadable csv of selected dataset ====
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("artificial_data.csv", sep = "")
        },
        content = function(file) {
            write.csv(all()[[1]], file, row.names = FALSE)
        }
    )
    
    
#***********************************************************************
# > MISC HELPER CODE ----
#***********************************************************************     
    # Obs slider range change + updateSlider ====
    source("serverPt_nObsDblClk.R", local=TRUE)
    
    # Close callout divs ====
    runjs('$(".bs-close").click(function() {
        $(this).parent().fadeOut("slow");
    });')
    
    # Reset button ====
    observeEvent(input$resetButton, {
        reset("aHat")
        reset("b1Hat")
        reset("shape")
    })
    
    # Results div - red outline kick ====
    ## MISC: put temporary red outline around results, 
    ## if user's navigated from anchor link (then toss
    ## outline once user's moused over table)
    runjs('
        $(\'a[href="#wrapper_rslts"]\').click(function(){ // when the anchor is clicked
            
            $("#wrapper_rslts").css({"boxShadow" : "0 0 20px 5px #d9534f"});   
            
            // Change everything back once you mouseover the results
            $("#wrapper_rslts").mouseover(function() { 
                $("#wrapper_rslts").css({"boxShadow" : "0 0 20px 5px rgba(255, 0, 0, 0)" })   
            });
        });
    ')
    
    # Informal/formal language toggle ====
    observeEvent(c(input$lang, input$solnButton), {

        # Informal language 
        if(input$lang==TRUE){
            shinyjs::show(id = "inf_instrText")
            shinyjs::show(id = "inf_ptTotal")
            shinyjs::show(id = "inf_bestGuess")
            if(input$solnButton!=0){
                shinyjs::show(id = "inf_actual")
                shinyjs::show(id = "inf_bestGuess_ans")
                
                hide(selector = "div.ptTotal_llh")
                hide(id = "inf_ptTotal")
                hide(id = "inf_bestGuess")
                hide(id = "formal_actual")
                hide(id = "restoreButton")
            }
            
            hide(id = "fullLLHButton")
            hide(id = "formal_instrText")
            hide(id = "formal_ptTotal")
            hide(id = "formal_bestGuess")
            hide(id = "formal_bestGuess_ans")
            hide(id = "formal_actual")
        
        # Formal language    
        } else{
            hide(id = "inf_instrText")
            hide(id = "inf_ptTotal")
            hide(id = "inf_bestGuess")
            hide(id = "inf_bestGuess_ans")
            
            shinyjs::show(id = "fullLLHButton")
            shinyjs::show(id = "formal_instrText")
            shinyjs::show(id = "formal_ptTotal")
            shinyjs::show(id = "formal_bestGuess")
            
            if(input$solnButton!=0){
                shinyjs::show(id = "formal_actual")
                shinyjs::show(id = "formal_bestGuess_ans")

                hide(selector = "div.ptTotal_llh")
                hide(id = "formal_ptTotal")
                hide(id = "formal_bestGuess")
                hide(id = "inf_actual")
                hide(id = "restoreButton")
            }
        }
    })
    
    # "Show Answer" actions ====
    ## The answer (+ disable sliders)
    observeEvent(input$solnButton, {
        aMin <- ifelse(all()[[2]][1]< -6, floor(all()[[2]][1])  , -6)
        aMax <- ifelse(all()[[2]][1]>  6, ceiling(all()[[2]][1]),  6)
            
        bMin <- ifelse(all()[[2]][2]< -6, floor(all()[[2]][2])  , -6)
        bMax <- ifelse(all()[[2]][2]>  6, ceiling(all()[[2]][2]),  6)
        
        shapeMin <- ifelse(all()[[2]][3]< 0.1, floor(all()[[2]][3])  , 0.1)
        shapeMax <- ifelse(all()[[2]][3]>   3, ceiling(all()[[2]][3]), 3)
        
        updateSliderInput(session, "aHat" , value=list(all()[[2]][1]),
                          min=list(aMin), max=list(aMax) )
        updateSliderInput(session, "b1Hat", value=list(all()[[2]][2]),
                          min=list(bMin), max=list(bMax) )
        updateSliderInput(session, "shape", value=list(all()[[2]][3]),
                          min=list(shapeMin), max=list(shapeMax) )
        
        disable("aHat")
        disable("b1Hat")
        disable("shape")
        disable("resetButton")

        showElement("wrapper_rslts")
    })
    
    # Restore best guess ====
    observeEvent(input$restoreButton, {
        updateSliderInput(session, "aHat",  value=list(bestSoFar$alG))
        updateSliderInput(session, "b1Hat", value=list(bestSoFar$b1G))
        updateSliderInput(session, "shape", value=list(bestSoFar$pG))
    })
    
    # Housekeeping
    session$onSessionEnded(stopApp)  
}    