# SERVER START 
server <- function(input, output, session) {
    
    # Tau reactive (if true is ordered) 
    taus <- reactiveValues(tau1=NULL,
                           tau2=NULL)
						   
						   
    #**********************************************
    # >> EQUATIONS -------------------
    #***********************
	source("serverPt_eqs.R", local=TRUE)

	
    #**********************************************
    ## >> EVENT REACTIVES (!! STARTS HERE) -----------------------
    #***********************
    allresults <- eventReactive(input$goButton, {
        # Purge taus
        taus$tau1 <- NULL
        taus$tau2 <- NULL
        
        # Hide the sim FYI
        shinyjs::hide(selector="h4.simFyiHdr")
        
        # continue as normal
        nObs <- input$n  # number of observations
        sims <- input$reps  # of simulations
        seed <- input$seed  # to set seed for each MC experiment, for replicability.
        
        dgp_chosen <- ifelse(input$dgp=="ord_dgp", "o_data", "m_data") 
        
        mc <- MC_easy(dgp=dgp_chosen, estimator = "est_both", obs=nObs, reps=sims, seed=seed)  
        
        # Do the do.call now (to avoid having to do it ten zillion times later)
        mod1 <-  lapply(mc[[1]], function(x) x[[1]]) %>% do.call(rbind,.) %>% data.frame
        mod2 <-  lapply(mc[[1]], function(x) x[[2]]) %>% do.call(rbind,.) %>% data.frame
        
        list(mod1, mod2, mc[[2]])
    })
    
	#### Storing sim run's DGP ====
    ## (s.t. it won't change value for any of the text output)
    simDGP <- eventReactive(input$goButton,
        isolate(input$dgp)
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
            
            # Loop, now that MC_results object exists
            for (i in 1:reps) {
                temp_data <- get(dgp)(obs)
                
                MC_results[[i]] <- get(estimator)(temp_data) 
                
                incProgress(1/reps, detail = paste(i, " of ", reps))  
            }

            # return the results of the estimation + a token dataset
            return(list(MC_results, temp_data))
            
        })
    }
    
	
    #**********************************************
    # >> DGP FUNCTIONS -------------------
    #***********************
    # Ordered data ====
    o_data <- function(n){
        
        aHat <- input$aHat
        bHat <- input$bHat
        x <- runif(n, -1, 1)
        
        XB <- aHat + bHat*x
        Y.star <- rlogis(n, XB, 1)
        
        # If first time through, set this as the arbitrary tau across all the runs
        if(is.null(taus$tau1)){
            tau1 <- quantile(XB, .25)
            tau2 <- quantile(XB, .75)
            
            # Pass the taus back to the global reactiveValues
            taus$tau1 <<- tau1
            taus$tau2 <<- tau2
            
        # Otherwise, pull the taus from the global reactiveValues
        } else{
            tau1 <- taus$tau1
            tau2 <- taus$tau2
        }
        y <-rep(NA, n)
        #browser()
        y[Y.star < tau1] <- 1
        y[Y.star >= tau1 & Y.star < tau2] <- 2
        y[Y.star >= tau2] <- 3
        
        # Return 
        return(data.frame(y, x))
        
    }
    # Multinomial data ====
    m_data <- function(n){
        
        x <- runif(n, -1, 1)
        b0A <-input$A_aHat
        b1A <-input$A_bHat
        b0B <-input$B_aHat
        b1B <- input$B_bHat
        
        pA <- exp(b0A + b1A*x)/(1 + exp(b0A + b1A*x) + exp(b0B + b1B*x))
        pB <- exp(b0B + b1B*x)/(1 + exp(b0A + b1A*x) + exp(b0B + b1B*x))
        pC <- 1 - pA - pB
        
        y <- rep(NA, n) 
        
        for(i in 1:n){
            y[i] <- sample(1:3, 1, replace = TRUE, prob = c(pA[i], pB[i], pC[i])) 
        }
 
        return(data.frame(y, x))
    }
    
	
    #**********************************************
    # >> MODEL ESTM FUNCTS (OMNI + INDV) -------------------
    #***********************
	source("serverPt_estm.R", local=TRUE)
    
	
	#**********************************************
    # >> TABLES (processed sims) -----------------------
    # (Has both printMC() and the render()s)
    #*********************** 
	source("serverPt_datOut.R", local=TRUE)
    
        
	#**********************************************
    # >> TABLE (raw sims) -----------------------
    #*********************** 
    output$raw <- DT::renderDataTable({
        input$goButton  # to get it to fire on reest
        
        if(input$model=="ord_mod")  idx <- 1
        else                        idx <- 2
        
        MC_est <- allresults()[[idx]]
        
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
    observeEvent(input$model, {
        if(input$model=="ord_model")  chList <- c("x's Coeff (bHat)"="b.x",
                                                  "Cutpt 1 (tau1)"  ="b.tau1",
                                                  "Cutpt 2 (tau2)"  ="b.tau2")
        else                          chList <- c("Cat A: Intercept (aHat_A)" = "b.intcA",
                                                  "Cat A: x's Coeff (bHat_A)" = "b.xA",
                                                  "Cat B: Intercept (aHat_B)" = "b.intcB",
                                                  "Cat B: x's Coeff (bHat_B)" = "b.xB")
        # Update the list
        updateSelectInput(session, "params", choices = chList, selected = chList[1])
        
    })
    
    # Histogram 
    output$dist <- renderPlot({
        input$goButton  # to get render() to fire on reestimation
        
        if(input$model=="ord_model")  idx <- 1
        else                          idx <- 2
        
        MC_est <- allresults()[[idx]]
        
        sims <- input$reps
        param <- MC_est[,input$params]
        
        hist(param,
             col = 'darkgray', 
             breaks = max(ceiling(sims/15),5), 
             xlim = range( (mean(param) - 5*sd(param)),(mean(param) + 5*sd(param)) ), 
             border = 'white', 
             main = paste0(gsub("b.", "", input$params), " Sampling Distribution"), 
             xlab = "Estimate Values"
        )
    })
    
	
	#**********************************************
    # >> SELECTION SUMMARY, WSIS -------------------
    #***********************
	
	# Selection summary ====
    ## Printing out true DGP, estm model
    output$selections <- renderUI({
        dgp <- ifelse(simDGP()=="ord_dgp", "ordered", "nominal")
        mod <- ifelse(input$model=="ord_model", "ordered logit", "multinomial logit")
        
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
    
	# WSIS tab contents ====
	source("serverPt_wsis.R", local=TRUE)

    
    #**********************************************
    # >> DATASET DOWNLOAD -------------------
    #***********************
    output$datDwn <- downloadHandler(  
        filename = function(){
            paste0("fakeData.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(allresults()[[3]], file, row.names = FALSE)
        }
    )
	
	
	#******************************************
    # ((Housekeeping)) ----
    #*********************
    ## kill connection to server once app stops running 
    
    session$onSessionEnded(stopApp)  
}