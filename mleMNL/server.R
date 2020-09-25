# > GRAPH HELPER FUNCTS ----
## Graph font 
font <- "Roboto"
        
# > SERVER FUNCTION ----
server <- function(input, output, session){
    
    # So that the values persist
    bestSoFar <- reactiveValues(llh = NULL, # best guess for LLH value so far
                                a1G = NULL, # intercept for cat A corresponding to best guess so far
                                b1G = NULL, # slope for cat A corresponding to best guess so far
                                a2G = NULL, # intercept for cat B corresponding to best guess so far
                                b2G = NULL) # slope for cat B corresponding to best guess so far
            
    
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

        # Reshape for MNL
        datMNL <- dat %>% 
                        mutate(., id = row_number()) %>%
                        mlogit.data(.,
                                    shape = "wide",
                                    choice = "y",
                                    id.var = "id")
        
        # Estimate
        mod <- mlogit(y ~ 1 | x1, reflevel=3, data=datMNL) 
        # # Estimate
        # mod <- polr(as.factor(y) ~ x1, data=dat, method="logistic", Hess = TRUE) 
        #     # Keep the Hessian here only for the final stargazer table, so we have SEs
        
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
        x1 <- rnorm(n)*1.5
        
        # To ensure we get observations in all three cats:
        all3Cats <- FALSE
        while(all3Cats==FALSE){
            # Pull true values
            b0A <- sample(seq(-3,3,0.25), 1)  
            b1A <- sample(seq(-3,3,0.25), 1)  
            b0B <- sample(seq(-3,3,0.25), 1)  
            b1B <- sample(seq(-3,3,0.25), 1)  
                     
            # Generate probabilities
            pA <- exp(b0A + b1A*x1)/(1 + exp(b0A + b1A*x1) + exp(b0B + b1B*x1))
            pB <- exp(b0B + b1B*x1)/(1 + exp(b0A + b1A*x1) + exp(b0B + b1B*x1))
            pC <- 1 - pA - pB
            
            # Translate to observed y
            y <- rep(NA, n) 

            for(i in 1:n){
                y[i] <- sample(1:3, 1, replace = TRUE, prob = c(pA[i], pB[i], pC[i])) 
            }
            
            # Do count: if have all 3 values, break out of while
            if(sum(table(y)==0)==0)  all3Cats <- TRUE
        }
        
        # Return dataset
        data.frame(y, x1)
    }
    
    ## LLH for current guess ====
    obsLLH <- reactive({
        input$solnButton
        
        # If not the actual solution, use sliders
        if(input$solnButton==0){
            a1 <- input$A_aHat
            b1 <- input$A_b1Hat
            a2 <- input$B_aHat
            b2 <- input$B_b1Hat
        # Use actual estimates to get data tab's LLH to match true
        } else {
            coefs <- all()[[2]]
            a1 <- coefs[1]
            b1 <- coefs[3]
            a2 <- coefs[2]
            b2 <- coefs[4]
        }
        dat <- all()[[1]] %>%
                mutate(xb.A= a1 + b1*x1,
                       xb.B= a2 + b2*x1,
                       
                       pr.A=exp(xb.A)/(1 + exp(xb.A) + exp(xb.B)),
                       pr.B=exp(xb.B)/(1 + exp(xb.A) + exp(xb.B)),
                       pr.C=1/(1 + exp(xb.A) + exp(xb.B)),
                       
                       obs=NA)

        dat <- dat %>%
                mutate(obs=if_else(y==1, log(pr.A),
                           if_else(y==2, log(pr.B), 
                                         log(pr.C)
                                   ))
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
                bestSoFar$a1G  <- input$A_aHat
                bestSoFar$b1G  <- input$A_b1Hat
                bestSoFar$a2G  <- input$B_aHat
                bestSoFar$b2G  <- input$B_b1Hat
            }
        # If it's the first guess, then by definition, it'll be the best one     
        } else if(is.null(bestSoFar$llh) & input$solnButton==0){
            bestSoFar$llh  <- obsLLH()[[1]]
            bestSoFar$a1G  <- input$A_aHat
            bestSoFar$b1G  <- input$A_b1Hat
            bestSoFar$a2G  <- input$B_aHat
            bestSoFar$b2G  <- input$B_b1Hat
        }
    })
    
    
#***********************************************************************
# > OUTPUT CODE ----   
#***********************************************************************       
    # Helper: get proper coef sign for MathJax expr
    signMJ <- function(beta){
        return(
            paste0(ifelse(beta>=0, "+", ""), beta) 
        )
    }
    
    # Eqs, CURRENT LINE EQ ====
    output$eq_lm <- renderUI({
        # exp of lin combos
        expA <- paste0("\\exp \\left(", input$A_aHat, signMJ(input$A_b1Hat), "x \\right)")
        expB <- paste0("\\exp \\left(", input$B_aHat, signMJ(input$B_b1Hat), "x \\right)")
        
        # denom
        den <- paste0("1 + ", expA, " + ", expB)
        
        # probs
        prA <- paste0('\\frac{', expA, '}{', den, '}')
        prB <- paste0('\\frac{', expB, '}{', den, '}')
        prC <- paste0('\\frac{    1     }{', den, '}')

        # Print
        withMathJax(
            paste0(
                "\\( 
                    \\begin{align}
                        \\Pr \\left(y=\\text{Cat. A} \\right) &= ", prA, " \\\\
                        \\Pr \\left(y=\\text{Cat. B} \\right) &= ", prB, " \\\\
                        \\Pr \\left(y=\\text{Cat. C} \\right) &= ", prC, " \\\\
                    \\end{align}
                \\)"
            )
        )
    })
        
    # Eqs, GENERIC LH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLH <- renderUI({

        # do in chunks - generic's line 1
        num <- "\\exp \\left( \\alpha_m + \\beta_m x_i \\right)"
        den <- paste0('\\sum_{y=j}^{J} \\exp \\left(\\alpha_j + \\beta_j x_i \\right)') 

        # Start building string:
        equation <- paste('\\(
            \\begin{align}
                  L\\left( \\alpha_A, \\alpha_B \\beta_A, \\beta_B ~|~y_i, x_i\\right) &=
                  \\prod \\limits_{m=1}^{3}   \\prod  \\limits_{y_i=m}^{N_{y=m}} 
                    \\left\\{ \\frac{', num, '}{', den, '} \\right\\} \\\\')

        # Generic's line 2 - insert actual counts for y value subsets
        fracA <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("_m", "_A", .)
        fracB <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("_m", "_B", .)
        fracC <- paste0(' \\frac{    1    }{', den,'}')
        
        equation <- paste0(equation,
                           ' &=  \\prod  \\limits_{y_i=1}^{', yCounts()[[1]], '} 
                                    \\left\\{ ', fracA, '\\right\\} * \\\\
                                    
                  &\\hphantom{= }~~~~\\prod  \\limits_{y_i=2}^{',yCounts()[[2]], '} 
                                    \\left\\{ ', fracB, '\\right\\} * \\\\
                                    
                  &\\hphantom{= }~~~~\\prod  \\limits_{y_i=3}^{', yCounts()[[3]], '} 
                                    \\left\\{ ', fracC, '\\right\\} ')
        
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
        
        num <- "\\exp \\left( \\alpha_m + \\beta_m x_i \\right)"
        den <- paste0('\\sum_{y=j}^{J} \\exp \\left(\\alpha_j + \\beta_j x_i \\right)') 

        # Start building string:
        equation <- paste('\\(
            \\begin{align}
               \\ln L\\left( \\alpha_A, \\alpha_B \\beta_A, \\beta_B ~|~y_i, x_i\\right) &=
                  \\sum \\limits_{m=1}^{3}   \\sum  \\limits_{y_i=m}^{N_{y=m}} 
                    \\left\\{ \\ln \\left( \\frac{', num, '}{', den, '} \\right) \\right\\} \\\\')

        # Generic's line 2 - insert actual counts for y value subsets
        fracA <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("_m", "_A", .)
        fracB <- paste0(' \\frac{', num, '}{', den,'}') %>% gsub("_m", "_B", .)
        fracC <- paste0(' \\frac{    1    }{', den,'}')
        
        equation <- paste0(equation,
                           ' &=  \\sum  \\limits_{y_i=1}^{', yCounts()[[1]], '} 
                                    \\left\\{ \\ln \\left(', fracA, '\\right) \\right\\} + \\\\
                                    
                  &\\hphantom{= }~~~~\\sum  \\limits_{y_i=2}^{',yCounts()[[2]], '} 
                                    \\left\\{ \\ln \\left(', fracB, '\\right) \\right\\} + \\\\
                                    
                  &\\hphantom{= }~~~~\\sum  \\limits_{y_i=3}^{', yCounts()[[3]], '} 
                                    \\left\\{ \\ln \\left(', fracC, '\\right) \\right\\} ')
        
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
                         \\ln L\\left(  \\alpha_A = ', input$A_aHat,
                                     ',~\\alpha_B = ', input$B_aHat,
                                     ',~\\beta_A = ' , input$A_b1Hat,
                                     ',~\\beta_B = ' , input$B_b1Hat,
                                     ' ~|~y_i, x_i \\right)')
        equation <- paste0('\\( ', opener, ' = \\\\ ')

        # Loop through every single observation
        for(i in 1:input$nObs){
            # do linear combo
            linCombA <- paste0(input$A_aHat, signMJ(input$A_b1Hat), '*', round(all()[[1]][i,2], 3))
            linCombB <- paste0(input$B_aHat, signMJ(input$B_b1Hat), '*', round(all()[[1]][i,2], 3))
            
            den <- paste0('1 + \\exp \\left(', linCombA, '\\right)',
                            '+ \\exp \\left(', linCombB, '\\right)') 
            
            num <- ifelse(all()[[1]][i,1]==1, 
                            paste0('\\exp \\left(', linCombA, '\\right)'),     # numerator if y=1 (catA)
                            ifelse(all()[[1]][i,1]==2, 
                                paste0('\\exp \\left(', linCombB, '\\right)'), # numerator if y=2 (catB)
                                "1")                                           # numerator if y=3 (catC)
                   )
            linComb <- paste0(input$b1Hat, '*', round(all()[[1]][i,2], 3))

            # insert proper term, sign + hard return at end of each term (to
            # help with readability)
            equation <- paste0(equation, ifelse(i!=1, "+", "\\phantom{+}"), ' \\left.
                                \\ln \\left( ', 
                                        '\\frac{', num, '}{', den, '}', 
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
    
    # MAIN GRAPH: Pr(y = Cat A) ====
    output$gph_k1<- renderPlot({   
        
        # Create adjusted y: =1 if y=Cat A, =0 oth.
        dat <- all()[[1]] %>%
                mutate(yAdj = y) %>%
                mutate(yAdj = recode(yAdj, `1`=1, `2`=0, `3`=0))
        
        # Get exp(XB) terms for A, B
        pieceA <- exp(input$A_aHat + seq(-5,5,0.05)*input$A_b1Hat)
        pieceB <- exp(input$B_aHat + seq(-5,5,0.05)*input$B_b1Hat)
        
        # Generate predicted value for Cat A, given this expression.
        yHat <- pieceA/(1 + pieceA + pieceB)
        
        # graph
        gg <- ggplot() +
                geom_point(aes(x=dat$x1, y=dat$yAdj), color="dodgerblue3", alpha=0.6) +
                geom_line(aes(x=seq(-5,5,0.05), y=yHat), col = "red") +
            labs(x="x", 
                 y=expression("Pr("*italic("y")~"= Cat. A)")
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
    
    # MAIN GRAPH: Pr(y = Cat B) ====
    output$gph_k2<- renderPlot({   
        
        # Create adjusted y: =1 if y=Cat A, =0 oth.
        dat <- all()[[1]] %>%
                mutate(yAdj = y) %>%
                mutate(yAdj = recode(yAdj, `1`=0, `2`=1, `3`=0))
        
        # Get exp(XB) terms for A, B
        pieceA <- exp(input$A_aHat + seq(-5,5,0.05)*input$A_b1Hat)
        pieceB <- exp(input$B_aHat + seq(-5,5,0.05)*input$B_b1Hat)
        
        # Generate predicted value for Cat B, given this expression.
        yHat <- pieceB/(1 + pieceA + pieceB)
        
        # graph
        gg<- ggplot() +
                geom_point(aes(x=dat$x1, y=dat$yAdj), color="dodgerblue3", alpha=0.6) +
                geom_line(aes(x=seq(-5,5,0.05), y=yHat), col = "red") +
            labs(x="x", 
                 y=expression("Pr("*italic("y")~"= Cat. B)")
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
    
    # RAW MODEL ====
    ## Print the actual results    
    output$modObj <- renderPrint({ 
        summary(all()[[3]])
    })
    
    # STARGAZER TABLE ====
    output$sgzTable <- renderUI({
    
        # number of places to round to
        places <- 2
        
        # get model
        mod <- all()[[3]]
        
        # get the raw HTML
        tab <- stargazer(mod, 
                    type="html", 
                    covariate.labels = c("Intercept<sub>A</sub>",
                                         "Intercept<sub>B</sub>",
                                         "<em>x</em><sub>A</sub>",
                                         "<em>x</em><sub>B</sub>"),
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
            paste0('\\( \\alpha_A =', bestSoFar$a1G,
                     ',~\\beta_A  =', bestSoFar$b1G, 
                     ',~\\alpha_B =', bestSoFar$a2G,
                     ',~\\beta_B  =', bestSoFar$b2G, 
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
            paste0('\\( \\alpha_A =', round(mod.sum[1,1], 3),
                     ',~\\beta_A  =', round(mod.sum[3,1], 3), 
                     '\\\\
                        \\alpha_B =', round(mod.sum[2,1], 3),
                     ',~\\beta_B  =', round(mod.sum[4,1], 3), 
                   ' \\)')
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
        reset("A_aHat")
        reset("A_b1Hat")
        reset("B_aHat")
        reset("B_b1Hat")
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
        # Order that sliders are listed in coef table
        coefTabOrd <- list("A_aHat",
                           "A_b1Hat",
                           "B_aHat",
                           "B_b1Hat")
        
        # Min/max for slider
        mm <- 8

        # Loop over all the combos
        for(i in 1:length(coefTabOrd)){
            # Update min/maxes, if needed
            bMin <- ifelse(all()[[2]][i]< -mm, floor(all()[[2]][i])  , -mm)
            bMax <- ifelse(all()[[2]][i]>  mm, ceiling(all()[[2]][i]),  mm)
        
            # Update the slider's values, if needed
            updateSliderInput(session, paste0(coefTabOrd[i]), value=list(all()[[2]][i]),
                              min=list(bMin), max=list(bMax) )
        }

        # Disable the sliders
        disable("A_aHat")
        disable("A_b1Hat")
        disable("B_aHat")
        disable("B_b1Hat")
        disable("resetButton")

        showElement("wrapper_rslts")
    }, priority=999)
    
    # Restore best guess ====
    observeEvent(input$restoreButton, {
        updateSliderInput(session, "A_aHat" , value=list(bestSoFar$a1G))
        updateSliderInput(session, "A_b1Hat", value=list(bestSoFar$b1G))
        updateSliderInput(session, "B_aHat" , value=list(bestSoFar$a2G))
        updateSliderInput(session, "B_b1Hat", value=list(bestSoFar$b2G))
    })
    
    # Housekeeping
    session$onSessionEnded(stopApp)  
}    