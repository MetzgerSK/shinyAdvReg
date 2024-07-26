# SERVER START
server <- function(input, output, session) {
    
    #**********************************************
    ## >> EQUATIONS -----------------------
    #***********************
    source("serverPt_eqs.R", local=TRUE)
    
    
    #**********************************************
    ## >> EVENT REACTIVES (!! STARTS HERE) -----------------------
    #***********************
    # Main reactive ====
    allresults <- eventReactive(input$goButton, {
        n    <- input$nObs  # number of observations
        sims <- input$reps  # of simulations
        seed <- input$seed  # to set seed for each MC experiment, for replicability.
        
        # Hide the sim FYI
        shinyjs::hide(selector="h4.simFyiHdr")
        
        # Get appropriate name for DGP function, given ZI + link selections
        dgp_chosen <- 
            if(input$dgp=="p_dgp" & input$z_dgp=="non_dgp")        "p_data"    # Poisson, no ZI
            else if(input$dgp=="p_dgp" & input$z_dgp=="zi_dgp")    "pzi_data"  # Poisson, ZI
            else if(input$dgp=="nb_dgp" & input$z_dgp=="non_dgp")  "nb_data"   # NB, no ZI
            else                                                   "nbzi_data" # NB, ZI
        
        # Run the sims
        mc <- MC_easy(dgp=dgp_chosen, estimator="est_over", obs=n, reps=sims, seed=seed)  
        
        
        # Do the do.call now (to avoid having to do it ten zillion times later)
        for(i in 1:4){
            assign(paste0("mod", i),
                   lapply(mc[[1]], function(x) x[[i]]) %>% do.call(rbind,.) %>% data.frame
            )
        }
        
        list(mod1, mod2, mod3, mod4, mc[[2]], mc[[3]])
        # [1]: Poisson  (coefs/SE)
        # [2]: NB       (coefs/SE)
        # [3]: ZIP      (coefs/SE)
        # [4]: ZINB     (coefs/SE)
        # [5]: example data frame
        # [6]: model objects
        ## [1]-[input$reps], inside each:
        ### [1]: Poisson  
        ### [2]: NB       
        ### [3]: ZIP      
        ### [4]: ZINB    
        
    })
    
    # Storing sim run's DGP ====
    ## (s.t. it won't change value for any of the text output)
    simDGP <- eventReactive(input$goButton,
        list(isolate(input$dgp),    # link funct
             isolate(input$z_dgp)   # ZI component
        )
    )
    
    
    #*********************************************************************
    # >> MC SIM FUNCTION -------------------
    #***********************	
    ## Used Francis Smart's blogpost on the R simulations to get skeleton
    ## http://www.econometricsbysimulation.com/2012/12/easy-monte-carlo-sampler-command.html
    
    MC_easy <- function(dgp, estimator, obs, reps, seed) {
        withProgress(message = 'Running Simulations', value=0, min=0, max=1, {
            # Set seed
            set.seed(seed)

            # Create empty holder objects
            MC_results <- NULL
            modObjs <- NULL

            # Loop, now that MC_results object exists
            for (i in 1:reps) {
                # Pull data
                temp_data <- get(dgp)(obs)
                
                # Estimate model
                returned <- get(estimator)(temp_data)  
                
                # Store results
                MC_results[[i]] <- returned[[1]]
                modObjs[[i]] <- returned[[2]]
                
                # Advance counter
                incProgress(1/reps, detail = paste(i, " of ", reps))   
            }
            # return the results of the estimation.
            return(list(MC_results, temp_data, modObjs))
            
        })
    }
    
    
    #**********************************************
    # >> DGP FUNCTIONS -------------------
    #***********************
    source("serverPt_dgps.R", local=TRUE)
    
    
    #**********************************************
    # >> MODEL ESTM FUNCTS (OMNI + INDV) -------------------
    #***********************
    source("serverPt_estm.R", local=TRUE)

    
    #**********************************************
    # >> TABLES (all) -----------------------
    #*********************** 
    # Col builder (rows = param statistics -> building cols by stacking rows)
    colBuilder <- function(true, ptEst, se){
        row <- rbind("**True Value**"           = true,
                       "Estimated Value (Mean)" = mean(ptEst),
                       "Lower 95% CI"           = quantile(ptEst, .025),
                       "Upper 95% CI"           = quantile(ptEst, .975),
                       "Estimated SE (Mean)"    = mean(se),
                       "StDev of Estimate"      = sd(ptEst))
        
        return(row)
    }
    
    # Processed sim results ====
    output$simRslts <- renderTable({
        c(input$model,      # to get it firing if model changes
          input$goButton)   # to fire this when a new set of sims are run
        
        # Pull appropriate index value
        idx <- pullIdx_mod(input$model, input$z_model)
        ests <- allresults()[[idx]]
            
        # True values (isolated)
        b.intcTrue <- isolate(input$aHat)
        b.xTrue <- isolate(input$bHat)
            
        
        # Build table (b/c these cols are always present, regardless of model or DGP)
        text <- cbind(
                      colBuilder(b.intcTrue, ests$b.intc, ests$se.intc),
                      colBuilder(b.xTrue,    ests$b.x,    ests$se.x)
                )
        
        colnames(text)<- c("Intercept (aHat)",
                           "x's Coeff (bHat)")
         
        
        # If it's a NB DGP or model, stick in the dispersion
        if(simDGP()[[1]]=="nb_dgp" |
           input$model=="nb_model"){
            # True value (isolated)
            dispTrue <- ifelse(simDGP()[[1]]=="nb_dgp", isolate(input$dispers), 0)
            
            # Build the column: the usual if estm model is NB, modific if Poisson
            ## NB
            if(input$model=="nb_model"){
                disp <- colBuilder(dispTrue, ests$disp, ests$se.disp)    
            
            ## Poisson
            } else {
                disp <- rbind(dispTrue,
                              0,
                              NA,
                              NA,
                              NA,
                              NA
                        )
            }
            
            # Add the column
            text <- cbind(text, disp)
            
            # Add the column label
            colnames(text)[[ncol(text)]] <- "Dispers. (thetaHat)"
        }
        
        ## ZI DGP **or** model selected
        if(simDGP()[[2]]=="zi_dgp" |
           input$z_model=="zi_model"){
            
            # True values (isolated)
            infl.b.intcTrue <- ifelse(simDGP()[[2]]=="zi_dgp",isolate(input$b0z), 0)
            infl.b.zTrue <- ifelse(simDGP()[[2]]=="zi_dgp", isolate(input$b1z), 0)
            
            # If requested model has ZI component, report it
            if(input$z_model=="zi_model"){
                zi <- cbind(
                          colBuilder(infl.b.intcTrue, ests$infl.b.intc, ests$infl.se.intc),
                          colBuilder(infl.b.zTrue,    ests$infl.b.z,    ests$infl.se.z)
                      )
                        
            # Else, report back zeros
            } else {
                zi <- cbind(
                        rbind(infl.b.intcTrue,
                              0,
                              NA,
                              NA,
                              NA,
                              NA
                        ),
                        rbind(infl.b.zTrue,
                              0,
                              NA,
                              NA,
                              NA,
                              NA
                        )
                      )
            }
            
            # Add the columns
            text <- cbind(text, zi)
            
            # Add the labels
            colnames(text)[[(ncol(text)-1)]] <- "Infl.: Intercept (gHat0)"
            colnames(text)[[ncol(text)]]     <- "Infl.: z's Coeff (gHatZ)"
        }
        
        # Return
        return(text)

    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # Raw sim results ====
    output$raw <- DT::renderDataTable({
        input$goButton  # to get it to fire on reest
        
        req(allresults())
        
        mod <- input$model
        ziMod <- input$z_model
        
        # Pull corresponding index
        idx <- pullIdx_mod(mod, ziMod)
        MC_est <- allresults()[[idx]]
        
        # Print table
        DT::datatable(MC_est, rownames= FALSE,
                      options = list(
                                     dom = 'l t p' 
                                    )) %>% 
                formatRound(c(1:ncol(MC_est)), 5)
    })
    
    
    #**********************************************
    # >> GRAPHS -------------------
    #***********************
    # Update the list of parameters, depending on model
    observeEvent(c(input$model,input$z_model), {
        # Store selected model + model ZI component
        mod <- input$model
        ziMod <- input$z_model
        
        # Pull the appropriate index number, given model and model ZI
        idx <- pullIdx_mod(mod, ziMod)

        # Update the list
        updateSelectInput(session, "params", choices = chList[[idx]], selected = chList[[idx]][1])
        
    })
    
    # Histogram 
    output$dist <- renderPlot({
        input$goButton  # to get the render() to fire on reestimation
        
        # Get model selections
        mod <- input$model
        ziMod  <- input$z_model
        
        # Pull corresponding index
        idx <- pullIdx_mod(mod, ziMod)
        MC_est <- allresults()[[idx]]
        
        # Pull # sims, actual parameter of interest
        sims <- input$reps
        param <- MC_est[,input$params]
        
        # For lower bound, needs to be 0 for dispersion, but can be regular for rest
        lBound <- ifelse(input$params=="disp", 0, (mean(param) - 5*sd(param)))
                         
        # Plot
        hist(param,
             col = 'darkgray', 
             breaks = max(ceiling(sims/15),5), 
             xlim = range( lBound,(mean(param) + 5*sd(param)) ), 
             border = 'white', 
             main = paste0(gsub("infl.", "Inflation Eq.: ",
                            gsub("b.", "", input$params)
                           ), " Sampling Distribution"), 
             xlab = "Estimate Values"
        )
    })

    # Update sim draw slider for rootogram
    # Use # of reps from last goButton push
    observeEvent(input$goButton, {
        updateSliderInput(session, "selectSim_rooto", max=input$reps)
    })
    
    # Rootogram 
    output$rootogram <- renderPlot({
        input$goButton  # to get the render() to fire on reestimation
        
        # Pull corresponding index, given dropdown selection on rootogram tab
        idx <- as.numeric(input$rooto_mods) 
        
        # Pull relv model obj for the selected sim draw
        mod <- allresults()[[6]][[input$selectSim_rooto]][[idx]]

        # Do up the plot
        rootogram(mod, xlab="Event Counts", main ="", confint = FALSE)
    })       
        
    #**********************************************
    # >> EQS, SELECTION SUMMARY -------------------
    #***********************

    # Selection summary ====
    ## Printing out true DGP, estm model
    output$selections <- renderUI({
        # True DGP
        dgp <- ifelse(simDGP()[[1]]=="p_dgp", "Poisson", "negative binomial")
        dgp <- paste0(dgp, 
                      ifelse(simDGP()[[2]]=="zi_dgp", " with a zero-inflated component", 
                                                      ", no zero-inflated component")
               )
        
        # True model
        mod <- ifelse(input$model=="p_model", "Poisson", "negative binomial")
        mod <- paste0(ifelse(input$z_model=="zi_model", "zero-inflated ", ""),
                      mod)
        
        # Build string
        str <- paste0( "\\( \\begin{align}
                                \\textbf{True DGP: }        &\\vphantom{:} \\text{", dgp, "} \\\\
                                \\textbf{Estimated Model: } &\\vphantom{:} \\text{", mod, "} 
                            \\end{align}
                        \\)"
        )
        
        # Output
        return(withMathJax(str))
    })
    
    
    #**********************************************
    # >> DATASET DOWNLOAD -------------------
    #***********************
    output$datDwn <- downloadHandler(  
        filename = function(){
            req(allresults())
            paste0("fakeData.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(allresults()[[5]], file, row.names = FALSE)
        }
    )

    
    #************************************************
    ## >> MIN/MAX ----
    #*********************
    runjs('
        $("#dgpMax").hide();
        
        $("#dgpMin").click(function() {
            $("#dgpChunk").slideUp();
            $("#dgpMin").hide();
            $("#dgpMax").show();
        });
        
        $("#dgpMax").click(function() {
            $("#dgpChunk").slideDown();
            $("#dgpMax").hide();
            $("#dgpMin").show();
        });
    ')
    
    
    #******************************************
    # ((Housekeeping)) ----
    #*********************
    ## kill connection to server once app stops running 
    
    session$onSessionEnded(stopApp)   
}


