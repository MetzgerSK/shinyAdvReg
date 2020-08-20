# > WRAPPER: Define omnibus estm functions =================== 
est_over <- function(temp_data){
    mod1 <- p_model(temp_data)     
    mod2 <- nb_model(temp_data)
    mod3 <- pzi_model(temp_data)
    mod4 <- nbzi_model(temp_data)

    list( list(mod1[[1]], mod2[[1]], mod3[[1]], mod4[[1]]),  # coefs/SEs
          list(mod1[[2]], mod2[[2]], mod3[[2]], mod4[[2]])   # actual mod objs
    )
}
    
## > INDV ESTM FUNCTIONS ##################
# Poisson, no ZI ====
p_model <- function(temp_data) {
    
    p_mod <- glm(y ~ x, family = "poisson", data = temp_data) 
    
    p.coef <- coef(p_mod)
        names(p.coef) <- c("b.intc",  "b.x")
    p.se <- sqrt(diag(vcov(p_mod)))
        names(p.se)   <- c("se.intc", "se.x")
    
    list(c(p.coef, p.se), p_mod)
    
}

# NB, no ZI ====
nb_model <- function(temp_data){
    
    nb_mod <- glm.nb(y ~ x, data=temp_data) 
    
    nb.coef <- c(coef(nb_mod), nb_mod$theta)
        names(nb.coef) <- c("b.intc",  "b.x",  "disp")  
    nb.se <- c(sqrt(diag(vcov(nb_mod))), nb_mod$SE.theta)
        names(nb.se)   <- c("se.intc", "se.x", "se.disp")
    
    list(c(nb.coef, nb.se), nb_mod)
    
    # Note: R reports theta, the variance.  Stata reports alpha = 1/theta.
}

# ZIP ====
pzi_model <- function(temp_data) {
    
    pzi_mod <- zeroinfl(y ~ x|z, data = temp_data) 
    
    pzi.coef <- coef(pzi_mod)
        names(pzi.coef) <- c("b.intc",  "b.x", 
                             "infl.b.intc",  "infl.b.z")
    pzi.se <- sqrt(diag(vcov(pzi_mod)))
        names(pzi.se)   <- c("se.intc", "se.x", 
                             "infl.se.intc", "infl.se.z")
    
    list(c(pzi.coef, pzi.se), pzi_mod)

}  

# ZINB ====
nbzi_model <- function(temp_data) {
    
    nbzi_mod <- zeroinfl(y ~ x | z, dist= "negbin", data = temp_data) 

    nbzi.coef <- c(coef(nbzi_mod), nbzi_mod$theta) 
        names(nbzi.coef) <- c("b.intc",  "b.x", 
                             "infl.b.intc",  "infl.b.z",
                             "disp")
    delt.se <- msm::deltamethod(~ exp(x1), log(nbzi_mod$theta), (nbzi_mod$SE.logtheta^2))    
    nbzi.se <- c(sqrt(diag(vcov(nbzi_mod))), 
                 ifelse(is.nan(delt.se), NA, delt.se)  # if it's missing (likely the case if no true ZI), just declare NA
                 )
        names(nbzi.se)   <- c("se.intc",  "se.x", 
                             "infl.se.intc",  "infl.se.z",
                             "se.disp")
   
    list(c(nbzi.coef, nbzi.se), nbzi_mod)
    
    # Note: R reports theta, the variance.  Stata reports alpha = 1/theta.
    
}
