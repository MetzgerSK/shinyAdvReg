#### >> Creating Model Functions for Simulation Function####
    # ologit ====
    o_model <- function(temp_data) {
        
        temp_data <- temp_data %>% 
                        mutate(., y=factor(y, levels = c("1", "2", "3")))
        
        o_mod <- polr(as.factor(y) ~ x, data=temp_data, method="logistic", Hess = TRUE) %>%
                    summary() %>%
                    coef()
        ord_coef <- o_mod[,1] 
            names(ord_coef) <- c("b.x", "b.tau1", "b.tau2")
        ord_se <- o_mod[,2] 
            names(ord_se) <- c("se.x", "se.tau1", "se.tau2")
        
        
        c(ord_coef, ord_se)
        
    }
    
    # MNL ====
    m_model <- function(temp_data){
        
        temp_data <- temp_data %>% 
                        mutate(., id = row_number()) %>%
                        mlogit.data(.,
                                    shape = "wide",
                                    choice = "y",
                                    id.var = "id")
        
        m_mod <- mlogit(y ~ 1 | x, reflevel=3, data=temp_data) %>% 
                    summary() %>% 
                    coef()
        
        m_coef <- m_mod[,1] 
            names(m_coef) <- c("b.intcA", "b.intcB", "b.xA", "b.xB")
        
        m_se <- m_mod[,2] 
            names(m_se) <- c("se.intcA", "se.intcB", "se.xA", "se.xB")
        
        c(m_coef, m_se)
        
    }
    
    #### >> Omnibus wrapper for estimator ####   
    # (Creating Estimator Function to Run both chosen and true model, need to be inside reactive)
    est_both <- function(temp_data){
        
        mod1 <- o_model(temp_data)  
        mod2 <- m_model(temp_data)
        
        list(mod1, mod2)
    }