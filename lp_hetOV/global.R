# Core Shiny app code written by Janet Lawler, Metzger's RA
# Streamlining and spiffing up by Metzger

#*******************************************************
library(MASS)
library(nnet)
library(shiny)
library(shinyjs)
library(shinyBS)

#library(dplyr)
library(pscl)
library(maxLik)
library(glmx)
library(DT)

# Referenced with ::s
## shinythemes

#*******************************************************
# Helper function: build names, given prefix and specification (determined by rank)
nameBuilder <- function(rank, het=FALSE, pref){
    # Will always have these two
    names <- sapply(c("intc", "x"), 
                    function(x) paste0(pref, ".", x), 
                    USE.NAMES=FALSE)
    
    # If rank 3...
    if(rank==3){
        # ...and not het, add z
        if(het==FALSE)  addNm <- "z"
        # else, add scale    
        else            addNm <- "het.x"

        names <- c(names, paste0(pref, ".", addNm))
    }
    
    # If het and over 3, add z and scale both
    if(rank>3 & het==TRUE){
        names <- c(names,
                   sapply(c("z", "het.x"), 
                     function(x) paste0(pref, ".", x), 
                     USE.NAMES=FALSE)
                 )
    }
    
    # Return
    return(names)
                                                 
}

# Basic set of options for various histograms
chList_main <- c("Intercept (aHat)" = "b.intc",
                 "x's Coeff (bHat)" = "b.x")

chList_z    <- c("z's Coeff (b2Hat)" = "b.z")

chList_het  <- c("Hetsk. Eq.: x's Coeff (gHat)" = "b.het.x")

# For a given model set:
chListModSet <- 
            list(
                c(chList_main,  # Basic, all
                  chList_z),
                
                chList_main,    # Basic, no z
                
                c(chList_main,  # Het, all
                  chList_z,
                  chList_het),
                
                c(chList_main,  # Het, no z
                  chList_het)
            )
                
# One big list (indices match MC_easy order)
chList <- c( chListModSet,
             chListModSet
           )

# Helper misc.: pull index (model) 
pullIdx_mod <- function(mod, hetMod, omMod){
    # Pull corresponding index based on sub-position within a link's set
    if(hetMod!="het_model"){
        if(omMod=="cov_model")  idx <- 0 # basic, x + z
        else                    idx <- 1 # basic, x
    
    } else {
        if(omMod=="cov_model")  idx <- 2 # het, x + z
        else                    idx <- 3 # het, x
    }
        
    # Add big model
    if(mod=="l_model")  idx <- idx + 1
    else                idx <- idx + 5
    
    return(idx)   
}