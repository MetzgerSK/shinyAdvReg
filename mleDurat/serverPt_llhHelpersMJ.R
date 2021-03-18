 # Also defining functions to MathJax-print the density and survivor here
lambdaMJ <- function(data){
    res <- paste0(' \\exp \\left( ', input$aHat, 
                                ifelse(input$b1Hat>=0, "+", ""), input$b1Hat,'*', data, ' 
                          \\right)')
    return(res)
}

weibDenMJ <- function(shape, scale, t, breaks=FALSE){
    # Notice: scale is presumed to already be in MJ exp(...) form {lambdaMJ}
    res <- paste0(shape, '*', scale, '*', 
                  ifelse(isTRUE(breaks), '\\\\ \\hphantom{++}', ''),
                  '\\left[ ', t, '*', scale, '\\right] ^ {\\left( ', shape,' -1 \\right)}',
                  ifelse(isTRUE(breaks), '\\\\ \\hphantom{++}', ''),
                  '\\exp \\left( - \\left[ \\left( ', t, '*', scale, '\\right)^{ ', shape, '} \\right] \\right)'
                )       
    return(res)
}

weibSurvMJ <- function(shape, scale, t){
    res <- paste0('\\exp \\left( - \\left[ \\left( ', t, '*', scale, '\\right)^{ ', shape, '} \\right] \\right)')
    return(res)
}
    