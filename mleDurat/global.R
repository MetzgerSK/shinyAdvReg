library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(survival)
library(dplyr)
library(shinycssloaders)
library(shinyjqui)
library(ggplot2)
library(ggtext)
library(magrittr)
library(showtext)

# Install Roboto for ggplot2
font_add_google("Roboto", "Roboto")

# Referenced with ::s
## shinythemes
## msm
#**********************************
# TIEV min function
dgumbmin <- function(x, location = 0, scale = 1){
    quant <- (x - location)/scale
    ans <- 1/scale *exp(quant) * exp(-exp(quant))
                        
    return(ans)
}
