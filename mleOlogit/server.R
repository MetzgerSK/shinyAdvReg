# > GRAPH HELPER FUNCTS ----
## Graph font 
font <- "Roboto"
        
# > SERVER FUNCTION ----
server <- function(input, output, session){
    
    # So that the values persist
    bestSoFar <- reactiveValues(llh = NULL, # best guess for LLH value so far
                                b1G = NULL, # slope corresponding to best guess so far
                                tau1G = NULL, # cutpt 1 corresponding to best guess so far
                                tau2G = NULL) # cutpt 2 corresponding to best guess so far
                    
                        # No intercept b/c held at 0, or else model not identified 
                        # w/2 cutpts and a 3-category y var
    
    # > !!!! STARTS HERE (triggered by button press) !!!! < ----
    all <- eventReactive(input$dataGenButton, {
        # disable the data gen button (force user to reload the entire page for a fresh dataset)
        disable("dataGenButton")
        disable("nObs")
        removeTooltip(session, "nObs")
        disable("seed")
        shinyjs::show("downloadData")
        shinyjs::hide(selector="h4.simFyiHdr")
        
        # Gen data
        dat <- dataGen()
        
        # Estimate
        mod <- polr(as.factor(y) ~ x1, data=dat, method="logistic", Hess = TRUE) 
            # Keep the Hessian here only for the final stargazer table, so we have SEs
        
        # Pull truth
        coeffs <- mod %>% summary %>% coef %>% .[,1]    # betas
        truth <- as.numeric(logLik(mod))                # true LLH
        
        # Return
        list(data.frame(dat), coeffs, mod, truth)
    })
    
    # > DATA-RELATED ----
    ## Generate data ====
    dataGen <- function() {
        set.seed(input$seed)
        
        n <- input$nObs
        b1HatTrue <- sample(seq(-5,5,0.25), 1)      # for x
        
        x1 <- rnorm(n)*2
        
        XB <- b1HatTrue*x1
        Y.star <- rlogis(input$nObs, XB, 1)
        
        # Randomly pick quantile values between 0 and 1, in increments of 0.025
        q1 <- sample(seq(0.1, 0.7, 0.025), 1)
        q2 <- q1 + sample(seq(0.2, max(0.2,min(0.5,1-q1-0.1)), 0.025), 1) 
         
        # If the larger cutpt ends up larger than 1, subtract 0.05 and call it a day
        if(max(q1,q2)>1){
            if(max(q1,q2)==q1) q1 <- q1 -0.05
            if(max(q1,q2)==q2) q2 <- q2 -0.05
        }
        
        # Get the true taus, based on those quantile values
        tau1 <- quantile(XB, min(q1,q2))
        tau2 <- quantile(XB, max(q1,q2))
       
        # Translate to observed y
        y <- rep(NA, n)

        y[Y.star < tau1] <- 1
        y[Y.star >= tau1 & Y.star < tau2] <- 2
        y[Y.star >= tau2] <- 3
        
        # Return dataset
        data.frame(y, x1)
    }
    
    ## LLH for current guess ====
    obsLLH <- reactive({
        input$solnButton
        
        dat <- all()[[1]] %>%
                mutate(xb=input$b1Hat*x1,
                       obs=NA)

        tau1 <- input$tauHats[[1]]
        tau2 <- input$tauHats[[2]]

        dat <- dat %>%
                mutate(obs=if_else(y==1, plogis(tau1 - xb, log=TRUE),
                           if_else(y==2, log(plogis(tau2 - xb) - plogis(tau1 - xb)), 
                                         plogis(tau2 - xb, lower.tail=FALSE, log=TRUE)))
                )

        c(sum(dat$obs), data.frame(dat$obs))
    })  
    
    ## All-time Max LLH ====
    ## KEEP TRACK OF ALL-TIME MAX LLH
    allTime <- reactive({
        # If it's not the first guess the user's made
        if(!is.null(bestSoFar$llh) & input$solnButton==0){
            if(max(bestSoFar$llh, obsLLH()[[1]])==obsLLH()[[1]]){
                bestSoFar$llh  <- obsLLH()[[1]]
                bestSoFar$b1G  <- input$b1Hat
                bestSoFar$tau1G <- input$tauHats[[1]]
                bestSoFar$tau2G <- input$tauHats[[2]]
            }
        # If it's the first guess, then by definition, it'll be the best one     
        } else if(is.null(bestSoFar$llh) & input$solnButton==0){
            bestSoFar$llh  <- obsLLH()[[1]]
            bestSoFar$b1G  <- input$b1Hat
            bestSoFar$tau1G <- input$tauHats[[1]]
            bestSoFar$tau2G <- input$tauHats[[2]]
        }
    })
    
    
#***********************************************************************
# > OUTPUT CODE ----
#***********************************************************************       
    # Eqs, CURRENT LINE EQ ====
    output$eq_lm <- renderUI({
        linCombCalc <- paste0(input$b1Hat, "x",
                              "\\text{, with } \\tau_1 = ", input$tauHats[[1]],
                              "\\text{ and } \\tau_2 = ", input$tauHats[[2]])
        
        withMathJax(
            paste(
                '\\( y^*=', linCombCalc, ' \\)'
            )
        )
    })
        
    # Eq HELPER: Getting proper expression given y, linC value  ----
    llhTerm <- function(linComb, y){
        
        # All relv terms
        for(i in 1:2){
            # Numerator
            num1 <- paste0('\\exp \\left( ', input$tauHats[[i]], ' - \\left(', linComb, '\\right) \\right)')

            # Denominator
            den1 <- paste0('1 + ', num1)
                   
            # Full piece   
            assign(paste0("y",i,"_piece"), paste0('\\frac{', num1, '}{', den1,'}'))
        }
            
        # Get the specific piece of relevance, given input
        if(y==1)        term <- paste0(y1_piece)
        else if(y==2)   term <- paste0(y2_piece, ' - ', y1_piece) 
        else if(y==3)   term <- paste0('1- ', y2_piece)
          # (^ If not 1 or 2, must be 3, with how the app's written, 
          #    but just to be explicit about it.)
        
        # Return the relv term
        return(term)
    }
    
    # Eqs, GENERIC LH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLH <- renderUI({

        # do in chunks - generic's line 1
        linComb <- paste0(input$b1Hat, 'x_i')
        num <- paste0('\\exp \\left( \\tau_{DINO} - \\left(', linComb, '\\right) \\right)')
        den <- paste0('1 + ', num) 
        
        frac1 <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("DINO", "j-1", .)
        frac2 <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("DINO", "j", .)

        # Start building string:
        equation <- paste('\\(
            \\begin{align}
                  L\\left( \\beta_1, \\tau_1, \\tau_2 ~|~y_i, x_i\\right) &=
                  \\prod \\limits_{j=1}^{3}   \\prod  \\limits_{y_i=j}^{N_{y=j}} 
                    \\left\\{ ', frac2, ' - ', frac1,' \\right\\} \\\\')

        # Generic's line 2 - insert actual tau values, counts for y value subsets
        equation <- paste0(equation,
                           ' &=  \\prod  \\limits_{y_i=1}^{', yCounts()[[1]], '} 
                                    \\left\\{ ', llhTerm(linComb, 1), '\\right\\} * \\\\
                                    
                  &\\hphantom{= }~~~~\\prod  \\limits_{y_i=2}^{',yCounts()[[2]], '} 
                                    \\left\\{ ', llhTerm(linComb, 2), '\\right\\} * \\\\
                                    
                  &\\hphantom{= }~~~~\\prod  \\limits_{y_i=3}^{', yCounts()[[3]], '} 
                                    \\left\\{ ', llhTerm(linComb, 3), '\\right\\} ')
        
        # Finish 
        equation <- paste0(equation, '\\end{align} 
            \\)')
        
        # Return
        withMathJax(
            equation
        )
    })
    
    # Eqs, GENERIC LLH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLLH <- renderUI({
        
        # do in chunks - generic's line 1
        linComb <- paste0(input$b1Hat, 'x_i')
        num <- paste0('\\exp \\left( \\tau_{DINO} - \\left(', linComb, '\\right) \\right)')
        den <- paste0('1 + ', num) 
        
        frac1 <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("DINO", "j-1", .)
        frac2 <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("DINO", "j", .)

        # Start building string
        equation <- paste('\\(
            \\begin{align}
                  \\ln L\\left( \\beta_1, \\tau_1, \\tau_2 ~|~y_i, x_i\\right) &=
                  \\sum \\limits_{j=1}^{3}   \\sum  \\limits_{y_i=j}^{N_{y=j}} 
                    \\left\\{ \\ln \\left(', frac2, ' - ', frac1,'\\right) \\right\\} \\\\')
            
        # Generic's line 2 - insert actual tau values, counts for y value subsets
        equation <- paste0(equation,
                           ' &=  \\sum  \\limits_{y_i=1}^{', yCounts()[[1]], '} 
                                    \\left\\{ \\ln \\left( ', llhTerm(linComb, 1), '\\right) \\right\\} + \\\\
                                    
                  &\\hphantom{= }~~~~\\sum  \\limits_{y_i=2}^{', yCounts()[[2]], '} 
                                    \\left\\{ \\ln \\left( ', llhTerm(linComb, 2), '\\right) \\right\\} + \\\\
                                    
                  &\\hphantom{= }~~~~\\sum  \\limits_{y_i=3}^{', yCounts()[[3]], '} 
                                    \\left\\{ \\ln \\left( ', llhTerm(linComb, 3), '\\right) \\right\\} ')
                
        # Finish
        equation <- paste0(equation, '\\end{align} 
            \\)')
        
        # Return
        withMathJax(
            equation
        )
    })

    # Eqs, SPECIFIC LLH (SIMPL, EACH OBS) ====
    output$eq_fullLLH_all <- renderUI({
        # Beginning of first line
        opener <- paste0('
                         \\ln L\\left( \\beta_1 = ', input$b1Hat,
                                     ',~\\tau_1 = ', input$tauHats[[1]],
                                     ',~\\tau_2 = ', input$tauHats[[2]], '
                                                        ~|~y_i, x_i \\right)')
        equation <- paste0('\\( ', opener, ' = \\\\ ')

        # Loop through every single observation
        for(i in 1:input$nObs){
            # do linear combo
            linComb <- paste0(input$b1Hat, '*', round(all()[[1]][i,2], 3))

            # insert proper term, sign + hard return at end of each term (to
            # help with readability)
            equation <- paste0(equation, ifelse(i!=1, "+", "\\phantom{+}"), ' \\left.
                                \\ln \\left( ', 
                                        llhTerm(linComb, all()[[1]][i,1]), 
                                    '\\right) \\right. \\\\~ ')
        }

        # Insert alignment break
        equation <- paste0(equation, '  \\\\~ \\\\~')

        # Insert LLH value.
        llh <- ifelse(input$solnButton==0, round(obsLLH()[[1]], 5), round(all()[[4]],5))
        equation <- paste0(equation, opener, ' = ', llh)

        # Put MathJax closer tag
        equation <- paste0(equation, ' \\)')

        # Print it
        withMathJax(
            equation
        )
    })
    
    # MAIN GRAPH: Pr(y>1) ====
    output$gph_k1<- renderPlot({   
        
        # Create adjusted y: =0 if y=1, 1 if y>1
        dat <- all()[[1]] %>%
                mutate(yAdj = y) %>%
                mutate(yAdj = recode(yAdj, `1`=0, `2`=1, `3`=1))
        
        # Generate predicted value, given this expression.
        yHat <- plogis(seq(-5,5,0.05)*input$b1Hat-input$tauHats[[1]])
        
        # graph
        gg <- ggplot() +
                geom_point(aes(x=dat$x1, y=dat$yAdj), color="dodgerblue3", alpha=0.6) +
                geom_line(aes(x=seq(-5,5,0.05), y=yHat), col = "red") +
            labs(x="x", 
                 y=expression("Pr("*italic("y")~"> 1)")
                ) +
            coord_cartesian(ylim=c(0,1), xlim=c(-5,5)) + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold", family = paste0(font))
                 )
        
        # If there's more than 125 observations, add rugs
        if(nrow(dat)>=125){
            gg <- gg + 
                    geom_rug(aes(x=filter(dat, yAdj==1)$x1), sides="t", alpha=0.2) +
                    geom_rug(aes(x=filter(dat, yAdj==0)$x1), sides="b", alpha=0.2)
        }
        
        # Return plot
        gg 
        
    })
    
    # MAIN GRAPH: Pr(y>2) ====
    output$gph_k2<- renderPlot({   
        
        # Create adjusted y: =0 if y=1, 1 if y>1
        dat <- all()[[1]] %>%
                mutate(yAdj = y) %>%
                mutate(yAdj = recode(yAdj, `1`=0, `2`=0, `3`=1))
        
        # Generate predicted value, given this expression.
        yHat <- plogis(input$tauHats[[2]] - seq(-5,5,0.05)*input$b1Hat, lower.tail=FALSE)
        
        # graph
        gg<- ggplot() +
                geom_point(aes(x=dat$x1, y=dat$yAdj), color="dodgerblue3", alpha=0.6) +
                geom_line(aes(x=seq(-5,5,0.05), y=yHat), col = "red") +
            labs(x="x", 
                 y=expression("Pr("*italic("y")~"> 2)")
                ) +
            coord_cartesian(ylim=c(0,1), xlim=c(-5,5)) + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold", family = paste0(font))
                 )
        
        # If there's more than 125 observations, add rugs
        if(nrow(dat)>=125){
            gg <- gg + 
                    geom_rug(aes(x=filter(dat, yAdj==1)$x1), sides="t", alpha=0.2) +
                    geom_rug(aes(x=filter(dat, yAdj==0)$x1), sides="b", alpha=0.2)
        }
        
        # Return plot
        gg 
        
        
        # You did verify this in Stata--it's plotting correctly.
        
    })
    
    # RAW MODEL ====
    ## Print the actual results    
    output$modObj <- renderPrint({ 
        summary(all()[[3]])
    })
    
    # STARGAZER TABLE ====
    output$sgzTable <- renderUI({
    
        # number of places to round to
        places <- 2
        
        # Will need to do all this manually, so get model in shorter-to-ref obj
        mod <- all()[[3]]
        mod.sum <- mod %>% summary %>% coef 
        
        # Fake mod to trick stargazer
        dat <- all()[[1]] 
        dat <- dat %>%
                mutate(fake1 = rnorm(n()))
        modFake <- lm(y ~ x1 + fake1, data=dat)
        
        # get the raw HTML
        tab <- stargazer(modFake, # to trick stargazer into working
                    type="html", 
                    covariate.labels = c("<em>x</em>", "Cutpt. 1", "Cutpt. 2"),
                    coef = list(c(mod.sum[1,1], mod.sum[2,1], mod.sum[3,1])),
                    se   = list(c(mod.sum[1,2], mod.sum[2,2], mod.sum[3,2])),  
                    title="Model Results",
                    dep.var.labels  = "DV: <em>y</em>",
                    dep.var.caption = "",
                    intercept.bottom=FALSE,
                    add.lines = list(
                                     c("ln<em>L</em>", 
                                        round(logLik(mod), places))
                                ),
                    keep.stat = c("N"),
                    report="vcs",               # just for you, Neal!
                    omit.table.layout="n",
                    column.sep.width = "2pt",
                    digits=places,
                    digits.extra=4
            )
        
        # Nuke final table line
        tab <- gsub('<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr></table>',
                    '<tr><td colspan="2" style="border-bottom: 0px solid black"></td></tr></table>',
                    tab)
        
        # Change wording to n (brute forcing, rather than arguing with stargazer) 
        # + line between lnL and n
        tab <- gsub('<tr><td style="text-align:left">Observations</td>',
                    '<tr style="border-top: 1px solid black"><td style="text-align:left"><em>n</em></td>',
                    tab)
        
        # Bump width of table
        tab <- gsub('<table style="text-align:center">',
                    '<table style="text-align:center; width:10em;">',
                    tab)
        
        # return as wrapped HTML
        HTML(tab)
    })
    
    # CURRENT GUESS' LLH VALUE ====
    output$llh <-  renderUI({
        withMathJax(
            paste0('\\(', round(obsLLH()[[1]], 5), '\\)')
        )
    })

    # ALL-TIME MAX LLH ====
    output$bestGuess <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\(', round(bestSoFar$llh, 5), '\\)')
        )
    })
    
    # PRINT ESTIMATES FOR ALL-TIME MAX LLH ====
    output$bestGuess_ests <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\( \\beta_1 =', bestSoFar$b1G, 
                     ',~\\tau_1  =', bestSoFar$tau1G, 
                     ',~\\tau_2  =', bestSoFar$tau2G, 
                   ' \\)')
        )
    })
    
    # PRINT TRUE SLOPE + INTERCEPT + RMSE ====
    ## (BASED ON ESTIM MODEL)
    output$trueEsts <- renderUI({
        
        # Grab model for easier tau extraction
        mod.sum <- all()[[3]]%>% summary %>% coef 

        # print
        withMathJax(
            paste0('\\( 
                        \\beta_1 =' , round(mod.sum[1,1], 3), 
                     ',~\\tau_1  =' , round(mod.sum[2,1], 3), 
                     ',~\\tau_2  =' , round(mod.sum[3,1], 3), 
                   '\\)')
        )
    })
       
    # PRINT ANSWER/TRUE LLH ====
    output$trueLLH <- renderUI({ # Otherwise, this evaluates from the get-go, before the button's even pushed.
        withMathJax(
            paste0('\\(', round(all()[[4]], 5), ' \\)')
        )
    })
    
    # DT: ACTUAL DATASET ====
    output$data_table <- DT::renderDataTable({
        # To ensure things will render, in case the user clicks the "Data" tab
        # before gening dataset
        req(input$dataGenButton)
        
        dat <- all()[[1]]
        llh <- round(obsLLH()[[1]], 3)
        dat$logLH_i <- obsLLH()[[2]]
        n  <- input$nObs # avoid subset error
        
        # Custom head/footer
        sketch <- htmltools::withTags(table(
            class = 'display',
            
            # header
            DT::tableHeader(cbind("ID"=NA, dat)),
            
            # footer
            tfoot(
                tr(
                    th(colspan="4", 
                       paste0("TOTAL (across all ", n, " observations): ", 
                              round(sum(dat$logLH_i),5) )) 
                )
            )
        ))
        
        # Print it
        DT::datatable(dat,
                      escape=TRUE,
                      rownames=TRUE,
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
          DT::formatRound(c(2:ncol(dat)), 5) 
    })
    
    # OUTPUT: making dataset downloadable, if people want to fiddle
    # Downloadable csv of selected dataset ----
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
    # Return category counts ====
    yCounts <- reactive({
        # Get how many from each cat we actually have
        for(i in 1:3){
            assign(paste0("n_y", i), sum(all()[[1]]$y==i))
        }
        # Return it
        return(c(n_y1, n_y2, n_y3))
    })
    
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
        reset("tauSlider")
    })
    
    # Checking the cutpoint sliders, ensuring the two aren't equal
    observeEvent(input$tauHats, {
        # If they're equal...
        if(input$tauHats[[1]]==input$tauHats[[2]]){
    
            # ...add 0.25 to the second slider, provided it's not at the max.
            if(input$tauHats[[2]]<4){
                tau1 = input$tauHats[[1]]
                tau2 = input$tauHats[[2]] + 0.25
            
            # If it's at the max, then subtract 0.25 from the first.    
            } else {
                tau1 = input$tauHats[[1]] - 0.25
                tau2 = input$tauHats[[2]] 
        
            }
             
            # Update the slider's value
            updateSliderInput(session, "tauHats", value=c(tau1,tau2))
            
        }
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
    
    # Language toggle ====
    ## MISC: toggle between informal and formal language
    ## (officially shifted from toggles to have more control over the 
    ##  headers, once "Show Answer" is clicked)
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
        # Min/max for slider
        mm <- 8
        mmTau <- 4  # tau slider has diff range than rest
        
        # Update min/maxes, if needed
        bMin <- ifelse(all()[[2]][1]< -mm, floor(all()[[2]][1])  , -mm)
        bMax <- ifelse(all()[[2]][1]>  mm, ceiling(all()[[2]][1]),  mm)
        
        tauMin <- ifelse(all()[[2]][2]< -mmTau, floor(all()[[2]][2])  , -mmTau)
        tauMax <- ifelse(all()[[2]][3]>  mmTau, ceiling(all()[[2]][3]),  mmTau)

        updateSliderInput(session, "b1Hat", value=list(all()[[2]][1]),
                          min=list(bMin), max=list(bMax) )
        updateSliderInput(session, "tauHats", value=list(all()[[2]][2], all()[[2]][3]),
                          min=list(tauMin), max=list(tauMax) )

        # Disable the sliders
        disable("b1Hat")
        disable("tauHats")
        disable("resetButton")

        showElement("wrapper_rslts")
    }, priority=999)
    
    # Restore best guess ====
    observeEvent(input$restoreButton, {
        updateSliderInput(session, "b1Hat"  , value=list(bestSoFar$b1G))
        updateSliderInput(session, "tauHats", value=c(bestSoFar$tau1G, 
                                                      bestSoFar$tau2G))
    })
    
    # Housekeeping
    session$onSessionEnded(stopApp)  
}    