####Creating Estimator Function to Run both chosen and true model, need to be inside reactive####   

est_mods <- function(temp_data){
    mod1 <- mod_reg(temp_data, "logit" , "y ~ x + z")    
    mod2 <- mod_reg(temp_data, "logit" , "y ~ x") 
    mod3 <- mod_het(temp_data, "logit" , "y ~ x + z")
    mod4 <- mod_het(temp_data, "logit" , "y ~ x")
    mod5 <- mod_reg(temp_data, "probit", "y ~ x + z")
    mod6 <- mod_reg(temp_data, "probit", "y ~ x")
    mod7 <- mod_het(temp_data, "probit", "y ~ x + z")
    mod8 <- mod_het(temp_data, "probit", "y ~ x")
    
    # If anything's NA, toss the whole draw
    nonConv <- sum(is.na(c(
                    mod1, mod2,
                    mod3, mod4,
                    mod5, mod6,
                    mod7, mod8
                )))
    
    if(nonConv==0){
        output <-
                list(mod1, mod2, # basic logit, correct + omit z
                     mod3, mod4, # het logit, correct + omit z
                     mod5, mod6, # basic probit, correct + omit z
                     mod7, mod8) # het probit, correct + omit z
                    
    } else output <- NA
    
    output
    
}

# Regular L/P
mod_reg <- function(temp_data, link, spec) {
    
    mod <- glm(as.formula(spec), family = binomial(link=link), data = temp_data) 
    
    coef <- coef(mod)
        names(coef) <- nameBuilder(mod$rank, FALSE, "b")
    se <- sqrt(diag(vcov(mod)))
        names(se)   <- nameBuilder(mod$rank, FALSE, "se")
    c(coef, se)
    
}

# Het L/P
mod_het <- function(temp_data, link, spec) {
    formula <- paste0(spec, " | x") %>% as.formula
    
    mod <- hetglm(formula, family = binomial(link=link), data = temp_data) 
    
    modRank <- mod$coefficients %>% unlist %>% length
    
    # If things do converge, proceed as normal
    if(mod$optim$convergence==0){
        coef <- coef(mod)
            names(coef) <- nameBuilder(modRank, TRUE, "b")
        se <- sqrt(diag(vcov(mod)))
            names(se)   <- nameBuilder(modRank, TRUE, "se")
     
    # Otherwise, fill with NAs so they'll be easier to spot        
    } else {
        coef <- se <- rep(NA, modRank)
    }
    
    c(coef, se)
    
}