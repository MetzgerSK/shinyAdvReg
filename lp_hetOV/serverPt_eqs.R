# MISC: get proper coef sign + for MathJax expr ----
signMJ <- function(beta){
	return(
		paste0(ifelse(beta>=0, "+", ""), beta)
	)
}


# True DGP equation ----
output$eq_trueDGP <- renderUI({
    # Fire when the button's pushed
    input$goButton
    
    # Build linear combo 
    linCombo <- paste0(input$aHat, signMJ(input$bHat), "x", signMJ(input$b2Hat), "z")
    
    # Deal w/heterosk
    if(simDGP()[[2]]=="het_dgp"){
        linCombo <- paste0("\\frac{", linCombo, "}{ \\exp \\left(", input$het_err, "x \\right) }")
    }
    
    # Long hand for link function
    ## Logit
    if(simDGP()[[1]]=="l_dgp"){
        expr <- paste0("\\cfrac{   \\exp \\left(", linCombo, "\\right) }
                              {1 + \\exp \\left(", linCombo, "\\right) }")
    
    ## Probit
    } else {
        expr <- paste0("\\Phi \\left(", linCombo, "\\right)") 
    }
    
    # Get full expr
    str <- paste0("\\( \\Pr(y=1) = ", expr,"\\)")
    
    # Render
    withMathJax(str)
    
})


# Estimated model ----
output$eq_estmMod <- renderUI({
    
    # Whether z is included
    if(input$cov_model=="cov_model") zLC <- paste0(" + \\hat{\\beta}_2 z")
    else                             zLC <- ""

    # Build linear combo
    linCombo <- paste0("\\hat{\\alpha} + \\hat{\\beta}_1 x", zLC)

    # Deal w/heterosk
    if(input$het_model=="het_model"){
        linCombo <- paste0("\\frac{", linCombo, "}{ \\exp \\left( \\hat{\\gamma} x \\right) }")
    }

    # Long hand for link function
    ## Logit
    if(input$model=="l_model"){
        expr <- paste0("\\cfrac{    \\exp \\left(", linCombo, "\\right) }
                              {1 + \\exp \\left(", linCombo, "\\right) }")
    ## Probit
    } else {
        expr <- paste0("\\Phi \\left(", linCombo, "\\right)") 
    }
    
    # Get full expr
    str <- paste0("\\( \\widehat{\\Pr(y=1)} = ", expr,"\\)")
    
    # Render
    withMathJax(str)

})