
    # HELPER: colBuilder() ====
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
    
    # OUTPUT: ologit (ordered DGP)====
    output$ord_only <- renderTable({
        c(input$model,      # to get it firing if model changes
          input$goButton)   # to fire this when a new set of sims are run
        
        if(input$model == "ord_model" & 
           simDGP()=="ord_dgp"){ 
            
            mod1 <- allresults()[[1]]
            
            # b.x's true val (isolated)
            b.xTrue <- isolate(input$bHat)
            
            # Build table
            text <- cbind(colBuilder(b.xTrue,   mod1$b.x,    mod1$se.x),
                          colBuilder(taus$tau1, mod1$b.tau1, mod1$se.tau1),
                          colBuilder(taus$tau2, mod1$b.tau2, mod1$se.tau2)
                    )
            
            colnames(text)<- c("x's Coeff (bHat)",
                               "Cutpt 1 (tau1)",
                               "Cutpt 2 (tau2)")
        
            # Return
            text
            
        } else  NULL

    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # OUTPUT: ologit (MNL DGP) ====
    output$mdta_omod <- renderTable({
        c(input$model,      # to get it firing if model changes
          input$goButton)   # to fire this when a new set of sims are run
        
        if(input$model == "ord_model" & 
           simDGP()=="mnl_dgp"){ # to deal with "changing DGPs swaps tables" issue
        
            mod1 <- allresults()[[1]]
            
            # all true vals (isolated)
            bA.xTrue <- isolate(input$A_bHat)
            bB.xTrue <- isolate(input$B_bHat)
    
            # Build table
            text <- cbind(colBuilder(bA.xTrue, mod1$b.x, mod1$se.x),
                          colBuilder(bB.xTrue, mod1$b.x, mod1$se.x)
                    )
            
            colnames(text)<- c("Cat A: x's Coeff (bHat_A)",
                               "Cat B: x's Coeff (bHat_B)")
            
            # Return
            text
        
        } else  NULL
        
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)   
    
    # OUTPUT: MNL (all) ====
    output$mnl_only <- renderTable({
        c(input$model,      # to get it firing if model changes
          input$goButton)   # to fire this when a new set of sims are run
        
        if(input$model == "mnl_model"){ # to deal with "changing DGPs swaps tables" issue
            mod2 <- allresults()[[2]] 
            
            # all true vals (isolated)
            ## true = mnl
            if(simDGP() == "mnl_dgp"){
                bA.consTrue <- isolate(input$A_aHat)
                bB.consTrue <- isolate(input$B_aHat)
                bA.xTrue <- isolate(input$A_bHat)
                bB.xTrue <- isolate(input$B_bHat)
            # true = ordered
            } else{
                bA.consTrue <- isolate(input$aHat)
                bB.consTrue <- isolate(input$aHat)
                bA.xTrue <- isolate(input$bHat)
                bB.xTrue <- isolate(input$bHat)
            }
            
            # Build table
            text <- cbind(colBuilder(bA.consTrue,   mod2$b.intcA,   mod2$se.intcA),
                          colBuilder(bA.xTrue,      mod2$b.xA,      mod2$se.xA),
                          colBuilder(bB.consTrue,   mod2$b.intcB,   mod2$se.intcB),
                          colBuilder(bB.xTrue,      mod2$b.xB,      mod2$se.xB)
                    )
            
            colnames(text)<- c("Cat A: Intercept (aHat_A)",
                               "Cat A: x's Coeff (bHat_A)",
                               "Cat B: Intercept (aHat_B)",
                               "Cat B: x's Coeff (bHat_B)")
        
            # Return
            text
        
        } else NULL

    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)