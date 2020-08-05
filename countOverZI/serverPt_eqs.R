# MISC: get proper coef sign + for MathJax expr
signMJ <- function(beta){
	return(
		paste0(ifelse(beta>=0, "+", ""), beta)
	)
}

# Output the true DGP
output$eqTrueDGP <- renderUI({
	req(allresults())
    
    # Linear combo's always present, so start there.
    linComb <- paste0("\\exp \\left(", isolate(input$aHat), signMJ(isolate(input$bHat)), "x \\right)")
    str <- paste0("E(y \\mid x) &= ", linComb, " \\\\ ")
    
    # Get dispersion
    disp <- paste0("\\text{Var}(y \\mid x) &=")
    ## Poisson
    if(simDGP()[[1]]=="p_dgp")  disp <- paste0(disp, linComb)
    ## NB
    else                        disp <- paste0(disp, linComb, " + 
                                               \\frac{ \\left[ ", linComb, "\\right]^2}
                                                     {", isolate(input$dispers), "}"
                                        )
    ## Connect
    str <- paste0("\\begin{align}", str, disp, "\\end{align}")
    
    # ZI
    if(simDGP()[[2]]=="zi_dgp"){
        linCombZ <- paste0(isolate(input$b0z), signMJ(isolate(input$b1z)), "z")
            
        zi <- paste(
                '\\Pr \\left(y>0 \\mid x \\right) = 
                    \\cfrac{    \\exp \\left(', linCombZ, ' \\right) } 
                           {1 + \\exp \\left(', linCombZ, ' \\right) } '
            )
        
        str <- paste0(str, "\\\\[3ex] \\phantom{brute forcing a larger space} \\\\
                            ~\\text{with a zero-inflated component equal to} \\\\[2ex]", zi)
    }
    
    # Insert the aligns + MJ tags
    str <- paste0("\\( ", str, " \\)")
    
    # Render and return the final MJ
    withMathJax(str)
    
})

# Output the DGP implied by estimated model
output$eqEstmMod <- renderUI({
    req(allresults())
    
    # Linear combo's always present, so start there.
    linComb <- paste0("\\exp \\left( \\hat{\\alpha} + \\hat{\\beta}x \\right)")
    str <- paste0("E(y \\mid x) &= ", linComb, " \\\\ ")
    
    # Get dispersion
    disp <- paste0("\\text{Var}(y \\mid x) &=")
    ## Poisson
    if(input$model=="p_model")  disp <- paste0(disp, linComb)
    ## NB
    else                        disp <- paste0(disp, linComb, " + 
                                               \\frac{ \\left[ ", linComb, "\\right]^2}
                                                     { \\hat{\\theta} }"
                                        )
    ## Connect
    str <- paste0("\\begin{align}", str, disp, "\\end{align}")
    
    # ZI
    if(input$z_model=="zi_model"){
        linCombZ <- paste0(" \\hat{\\gamma}_0 + \\hat{\\gamma}_z z")
    
        zi <- paste(
                '\\Pr \\left(y>0 \\mid x \\right) =
                    \\cfrac{    \\exp \\left(', linCombZ, ' \\right) }
                           {1 + \\exp \\left(', linCombZ, ' \\right) }'
            )
        
        str <- paste0(str, "\\\\[3ex] \\phantom{brute forcing a larger space} \\\\
                            ~\\text{with a zero-inflated component equal to} \\\\[2ex]", zi)
    }
    
    # Insert the aligns + MJ tags
    str <- paste0("\\( ", str, "\\)")
    
    # Render and return the final MJ
    withMathJax(str)
    
})