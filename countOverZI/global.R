# Original simulation code: Carsey and Harden's book, Monte Carlo Simulation and 
#                           Resampling Methods for Social Science (2013, Sage).
# Relevant C&H section: Sect. 6.4.2
#
# Converted from C&H to Shiny app by Janet Lawler, Metzger's RA
# Streamlining and spiffing up by Metzger

#*******************************************************
library(MASS)
library(nnet)
library(shiny)
library(shinyjs)

library(dplyr)
library(pscl)
library(DT)

# Referenced with ::s
## shinythemes
## msm

#*******************************************************
# Basic set of options for Poisson, NB histograms
chList_pois <- c("Intercept (aHat)" = "b.intc",
                 "x's Coeff (bHat)" = "b.x")
chList_nbreg <- c(chList_pois, 
                  "Dispersion (thetaHat)" = "disp")

chList_genZI <- c("Infl. Eq.: Intercept (zi_aHat)" = "infl.b.intc",
                  "Infl. Eq.: z's Coeff (zi_bHat)" = "infl.b.z")

# One big list (indices match MC_easy order)
chList <- list( chList_pois,    # Poisson
                chList_nbreg,   # NB
                c(chList_pois,  # ZIP
                  chList_genZI),
                c(chList_nbreg, # ZINB
                  chList_genZI)
          )

# Helper misc.: pull index (model) 
pullIdx_mod <- function(mod, ziMod){
    # Pull corresponding index
    if(mod=="p_model" & ziMod=="non_model")        idx <- 1 # Poisson, no ziMod
    else if(mod=="p_model" & ziMod=="zi_model")    idx <- 3 # Poisson, ziMod
    else if(mod=="nb_model" & ziMod=="non_model")  idx <- 2 # NB, no ziMod
    else                                           idx <- 4 # NB, ziMod
 
    return(idx)   
}
 
