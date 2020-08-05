####Creating DGP Functions###
# Poisson, no ZI ====
p_data <- function(n){
    
    aHat <- input$aHat
    bHat <- input$bHat
    x <- runif(n, -1, 1)
    z <- rnorm(n, 0, 1)
    
    y <- rpois(n, exp(aHat + bHat*x))
    
    
    pdta <- data.frame(y, x, z)
    return(pdta)
}

# NB, no ZI ====
nb_data <- function(n){
    
    x <- runif(n, -1, 1)
    z <- rnorm(n, 0, 1)
    b0 <- input$aHat #b0
    b1 <- input$bHat #b1
    
    y <- rnbinom(n, size = input$dispers, mu = exp(b0 + b1*x))
    
    nbdta <- data.frame(y, x, z)
    return(nbdta)
}

# ZIP ====
# C&H ZIP Random Number Generator Function 
rzipois <- function(n, zprob, x){ 
    ifelse(rbinom(n, 1, zprob) == 1, 0, rpois(n, exp(input$aHat + input$bHat*x)))
}

pzi_data <- function(n){
    
    x <- runif(n, -1, 1)
    z <- rnorm(n, 0, 1)
    aHat <- input$aHat
    bHat <- input$bHat
    
    b0z <- input$b0z
    b1z <- input$b1z
    
    y <- rzipois(n, zprob = plogis(b0z + b1z*z), x)
    
    
    pzidta <- data.frame(y, x, z)
    return(pzidta) 
}

# ZINB ====
# C&H ZINB Random Number Generator Function 
rzinbinom <- function(n, mu, size, zprob){ 
    ifelse(rbinom(n, 1, zprob) == 1, 0, rnbinom(n, size = size, mu = mu))
}

nbzi_data <- function(n){
    
    x <- runif(n, -1, 1)
    z <- rnorm(n, 0, 1)
    b0 <- input$aHat #b0
    b1 <- input$bHat #b1 
    
    b0z <- input$b0z
    b1z <- input$b1z
    
    y <- rzinbinom(n, mu = exp(b0 + b1*x), size = input$dispers,
                   zprob = plogis(b0z + b1z*z))
    
    
    nbzidta <- data.frame(y, x, z)
    return(nbzidta)
}