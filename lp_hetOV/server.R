# SERVER START
server <- function(input, output, session) {
    
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
        
        # Run the sims
        mc <- MC_easy(dgp="dat_generic", estimator ="est_mods", obs=n, reps=sims, seed=seed)  
    
        
        # Do the do.call now (to avoid having to do it ten zillion times later)
        for(i in 1:8){
            assign(paste0("mod", i),
                   lapply(mc[[1]], function(x) x[[i]]) %>% do.call(rbind,.) %>% data.frame
            )
        }
            
        list(mod1, mod2, mod3, mod4,
             mod5, mod6, mod7, mod8,
             mc[[2]])
        
        # 1: basic logit, x + z
        # 2: basic logit, x
        # 3: het logit, x + z
        # 4: het logit, x
        # 5: basic probit, x + z
        # 6: basic probit, x
        # 7: het probit, x + z
        # 8: het probit, x
    })
    
    # Storing sim run's DGP ====
    ## (s.t. it won't change value for any of the text output)
    simDGP <- eventReactive(input$goButton,
        list(isolate(input$dgp),     # link funct
             isolate(input$het_dgp), # heterosk
             isolate(input$cov_dgp)  # covariate omitted?
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

            # Set info re: link, het
            if(input$dgp=="l_dgp")  linkFunc <- plogis
            else                    linkFunc <- pnorm
            
            # Set info re: heterosk
            if(input$het_dgp=="het_dgp") hetero <- TRUE
            else                         hetero <- FALSE
            
            # Create empty holder object
            MC_results <- NULL

            # Loop, now that MC_results object exists
            i=1
            while(i<=reps){ 
                temp_data <- get(dgp)(obs, linkFunc, hetero)
                
                MC_results[[i]] <- get(estimator)(temp_data)  
                    # est_mods will now return NULL if there was
                    # a convergence issue anywhere
                
                # See if there was a convergence problem anywhere; 
                # toss the draw, if so; otherwise, adv. the counter
                suppressWarnings(
                    if(!is.na(MC_results[[i]])){
                        incProgress(1/reps, detail = paste(i, " of ", reps))   
                        i <- i + 1
                    }
                )
            }

            # return the results of the estimation.
            return(list(MC_results, temp_data))
            
        })
    }
    

    
    #**********************************************
    # >> DGP FUNCTIONS -------------------
    #***********************
    dat_generic <- function(n, FUN=NULL, het=FALSE){ # FUN = link funct (NULL to ensure it gets specified)
        
        # Always params
        aHat <- input$aHat
        bHat <- input$bHat
        b2Hat <- input$b2Hat
        
        # Always covars
        x <- rnorm(n)
        x2 <- rnorm(n)*5
        
        # If heterosk
        if(het==TRUE)   het_err <- exp(input$het_err*x) # if hetero, div by exp(sigma*x)
        else            het_err <- rep(1,n)             # if not hetero, just div by 1
        
        # Generate
        y <- rbinom(n, 1, FUN( (aHat + bHat*x + b2Hat*x2)/het_err) )    
        
        # Return
        return(data.frame(y, x, z=x2))
    }
    
    
    #**********************************************
    # >> MODEL ESTM FUNCTS (OMNI + INDV) ----
    #***********************
    source("serverPt_estm.R", local=TRUE)
    
    
    #**********************************************
    # >> DGP EQS (MathJax) ----
    #*********************
    source("serverPt_eqs.R", local=TRUE)
    
    
    #**********************************************
    # >> TABLES (all) -----------------------
    #*********************** 
    source("serverPt_datOut.R", local=TRUE)
    
    
    #**********************************************
    # >> GRAPHS -------------------
    #***********************
    # Update the list of parameters, depending on model
    observeEvent(c(input$model,input$het_model,input$cov_model), {
        # Store selected model + model ZI component
        mod <- input$model
        hetMod <- input$het_model
        ovMod <- input$cov_model
        
        # Pull corresponding index
        idx <- pullIdx_mod(mod, hetMod, ovMod)
        
        # Update the list
        updateSelectInput(session, "params", choices = chList[[idx]], selected = chList[[idx]][1])
        
    })
    
    # Histogram 
    output$dist <- renderPlot({
        input$goButton  # to get the render() to fire on reestimation
        
        # Get model selections
        mod <- input$model
        hetMod <- input$het_model
        ovMod <- input$cov_model
        
        # Pull corresponding index
        idx <- pullIdx_mod(mod, hetMod, ovMod)
        MC_est <- allresults()[[idx]]
        
        # Pull # sims, actual parameter of interest
        sims <- input$reps
        param <- MC_est[,input$params]
        
        # For lower bound, needs to be 0 for dispersion, but can be regular for rest
        lBound <- mean(param) - 5*sd(param)
                         
        # Plot
        hist(param,
             col = 'darkgray', 
             breaks = max(ceiling(sims/15),5), 
             xlim = range( lBound,(mean(param) + 5*sd(param)) ), 
             border = 'white', 
             main = paste0(gsub("het.", "Heteroskedasticity Eq.: ",
                            gsub("b.", "", input$params)
                           ), " Sampling Distribution"), 
             xlab = "Estimate Values"
        )
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
            write.csv(allresults()[[9]], file, row.names = FALSE)
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
    
        
    #************************************************
    ## >> CLOSE CALLOUT DIVS ----
    #*********************
    runjs('$(".bs-close").click(function() {
        $(this).parent().fadeOut("slow");
    });') 
    
    
    #******************************************
    # ((Housekeeping)) ----
    #*********************
    ## kill connection to server once app stops running 
    
    session$onSessionEnded(stopApp)   
 
}
