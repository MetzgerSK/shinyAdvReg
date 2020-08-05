# MISC: get proper coef sign + for MathJax expr
signMJ <- function(beta){
	return(
		paste0(ifelse(beta>=0, "+", ""), beta, "x")
	)
}

# MISC: given choice letter, form up linear combo
linCombExpMNL <- function(let){
	intc <- eval(parse(text=paste0("isolate(input$", let,"_aHat)")))
	beta <- eval(parse(text=paste0("isolate(input$", let,"_bHat)")))
	
	return(paste0("\\exp \\left(", intc, signMJ(beta), " \\right)"))
}

# Output the true DGP
output$eqTrueDGP <- renderUI({
	req(c(allresults(), taus))
	
	# ORDINAL
	if(simDGP() == "ord_dgp"){
		yStar <- paste0("y^* = ", isolate(input$aHat), signMJ(isolate(input$bHat)), " + u 
						\\text{ where }u\\sim \\lambda(\\mu=0, \\text{ SD}=\\frac{\\pi}{\\sqrt{3}})")
		y <- paste0("y = \\begin{cases}
							1  &\\text{if } y^*<", signif(taus$tau1,3)," \\\\
							2  &\\text{if } ", signif(taus$tau1,3), "\\leq y^* <", signif(taus$tau2,3), " \\\\
							3  &\\text{if } y^* \\geq ", signif(taus$tau2, 3)," 
						 \\end{cases}")
		
		str <- paste0("\\(", yStar, "\\\\", y, "\\)")
	
	# MULTINOMIAL
	} else {
		# Denominator's the same for all, so just build once
		denom <- paste0("1 + ", linCombExpMNL("A"), " + ", linCombExpMNL("B"))
		
		# Build the prob expressions for each alternative
		str <- paste0(" \\begin{align}
		
						\\Pr(y=\\text{ Cat. A}) &= \\frac{ ", linCombExpMNL("A"), "}
										 { ", denom,              "} \\\\[2ex]
										 
						\\Pr(y=\\text{ Cat. B}) &= \\frac{ ", linCombExpMNL("B"), "}
										 { ", denom,              "} \\\\[2ex]
						 
						\\Pr(y=\\text{ Cat. C}) &= 1 - \\Pr(y=\\text{ Cat. A}) - \\Pr(y=\\text{ Cat. B})
						
						\\end{align}
					")
		
		str <- paste0("\\(", str, "\\)")
									
	}
	
	withMathJax(str)
})