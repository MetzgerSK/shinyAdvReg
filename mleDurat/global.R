library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(survival)
library(dplyr)
library(shinycssloaders)
library(shinyjqui)
library(ggplot2)
library(magrittr)

# Referenced with ::s
## shinythemes
#**********************************
# TIEV min function
dgumbmin <- function(x, location = 0, scale = 1){
    quant <- (x - location)/scale
    ans <- 1/scale *exp(quant) * exp(-exp(quant))
                        
    return(ans)
}
