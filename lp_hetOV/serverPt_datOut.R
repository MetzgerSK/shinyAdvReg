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
    idx <- pullIdx_mod(input$model, input$het_model, input$cov_model)
    ests <- allresults()[[idx]]

    # True values (isolated)
    b.intcTrue <- isolate(input$aHat)
    b.xTrue <- isolate(input$bHat)
    b.zTrue <- isolate(input$b2Hat)
    
    # Build table (b/c these cols are always present, regardless of model or DGP)
    text <- cbind(
                  colBuilder(b.intcTrue, ests$b.intc, ests$se.intc),
                  colBuilder(b.xTrue,    ests$b.x,    ests$se.x)
            )
    
    colnames(text)<- c("Intercept (aHat)",
                       "x's Coeff (b1Hat)")
     
    
    # If z's not omitted, stick it in
    if(input$cov_model=="cov_model"){
        # Get z's column
        z <- colBuilder(b.zTrue, ests$b.z, ests$se.z)  
        
        # Add z's column
        text <- cbind(text, z)
        
        # Add label
        colnames(text)[[ncol(text)]] <- c("z's Coeff (b2Hat)")
    }
    
    # If het (or estm model w/het), insert
    if(simDGP()[[2]]=="het_dgp" |
       input$het_model=="het_model"){
        # True value (isolated)
        hetTrue <- ifelse(simDGP()[[2]]=="het_dgp", isolate(input$het_err), 0)
        
        # Build the column: the usual if estm model is het, modific if basic
        ## het
        if(input$het_model=="het_model"){
            disp <- colBuilder(hetTrue, ests$b.het.x, ests$se.het.x)    
        
        ## basic
        } else {
            disp <- rbind(hetTrue,
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
        colnames(text)[[ncol(text)]] <- "Hetsk.:\nx's Scale (gHat)"
    }
    
    # Return
    return(text)

}, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)

# Raw sim results ====
output$raw <- DT::renderDataTable({
    input$goButton  # to get it to fire on reest
    
    req(allresults())
    
    mod <- input$model
    hetMod <- input$het_model
    ovMod <- input$cov_model
    
    # Pull corresponding index
    idx <- pullIdx_mod(mod, hetMod, ovMod)
    MC_est <- allresults()[[idx]]
    
    # Print table
    DT::datatable(MC_est, rownames= FALSE,
                  options = list(
                                 dom = 'l t p' 
                                )) %>% 
            formatRound(c(1:ncol(MC_est)), 5)
})