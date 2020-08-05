# Original simulation code: Carsey and Harden's book, Monte Carlo Simulation and 
#                           Resampling Methods for Social Science (2013, Sage).
# Relevant C&H section: Sect. 6.3.2
#
# Converted from C&H to Shiny app by Janet Lawler, Metzger's RA
# Streamlining and spiffing up by Metzger

#*******************************************************
library(MASS)
library(shiny)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(mlogit)
library(DT)

# Referenced with ::s
## shinythemes