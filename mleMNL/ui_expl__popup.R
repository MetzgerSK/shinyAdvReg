jqui_draggable(
    
    bsModal("modal_expl", "What's going on here?", "explainButton", size = "large",
            
            
        h4("The Situation"),
        p( "You have a dataset whose points are displayed in blue. Your
        dependent variable's values are integers that represent three
        categories: Category A \\((y=1)\\), Category B \\((y=2)\\), and
        Category C \\((y=3)\\). You have one independent variable (horizontal
        axis), \\( x \\), which (arbitrarily) is distributed normally with a
        mean of 0 and standard deviation of 1.5."),
    
        
        h4("Your Oveararching Goal"),
        p( "You'd like to estimate a regression model using these data.  That
        means you'd like to use values of \\(x \\) to predict values of \\( y \\).
        Because your dependent variable is nominal, we've chosen to estimate a
        multinomial logit regression."),
    
        
        h4("What needs to happen to reach my overarching goal?"),
        
        HTML("<p>R uses maximum likelihood estimation (MLE) to find the
        best-fitting line for multinomial logit models.  
        Informally, when R uses MLE, it goes through all the possible combinations
        of \\( \\alpha_A \\), \\(\\alpha_B\\), \\(\\beta_A\\), and \\(\\beta_B\\)
        values to see which 
        combination is \"most likely\" responsible for generating the
        patterns you see in your dataset. (Note: R is not *actually* going
        through every combination.  It uses various
        optimization rules to hone in on the likely candidates
        quickly.)</p>"),
        
        p("R isn't directing the search for the best intercept and slope values, 
        though.  ", strong("YOU"), "are."),
       
        HTML("<p>You have (an artificially limited range of) intercept and slope
        combinations to check manually using the sliders in the left
        sidebar.  Multinomial logit models allow us to express the probability of
        \\(y = \\text{some category} \\), and because probabilities must sum to 1,
        determining two of the categories' predicted probabilities automatically
        gives us the third category's predicted probability
            <ul> 
                <li>Left graph: Focuses on \\(y=1\\), amounting to the
                probability that \\(y\\) is equal to Category A.  The red line
                corresponds to the logit CDF implied by your selected intercept
                and slope value for Cat. A.</li>
                <li>Right graph: Focuses on \\(y=2\\), amounting to the
                probability that \\(y\\) is equal to Category B.  The red line
                corresponds to the logit CDF implied by your selected intercept 
                and slope value for Cat. B.</li>
            </ul>
            The \"likeliness\" of seeing your data, given these intercept and  
            slope guesses, is displayed under \"Current Point Total\".
            If you have the prose slider set to \"Formal\", it'll instead read
            \"Current log-likelihood value\". In \"Formal\" prose mode, you'll
            also see a small eyeball button where you can view the actual formula
            for the calculation producing that value, if you'd like.</p>"),
        
       
        h4("Your Immediate Task"),
        HTML("<p>Because you'd like to find the \"most likely\" intercept and
        slope  values that produced your data, your task is to <strong>find
        the combination that gives you the LARGEST \"point\"/likelihood value
        possible</strong>.  Be mindful of the likelihood's sign as you hunt:
        likelihood values are almost always negative (we log them to protect
        against really, really big numbers that'll eat up all the computer's
        memory and cause the program to crash).</p>"),
          
        p( "The largest likelihood value you've found so far is recorded under
        \"Current all-time best guess\"/\"Maximum log-likelihood value (so far)\".
        The orange \"Restore\" button will set the sliders to the intercept and
        slope values associated with your best guess."),
    
        HTML("<p>After you hunt for a bit, clicking \"Show Answer\" will display the
        best \\(\\{\\alpha_A,\\alpha_B,\\beta_A,\\beta_B\\}\\) 
        combination that R found.</p>"),
    
        div(class="bs-callout bs-callout-info",
            HTML("<h4><i class='fas fa-search'></i> Hint</h4>"),
            HTML(
                "<p>Remember that each graph's line should try to fit the
                    <em>majority</em> of points as well as possible.  <code>mleMNL</code>
                    adds rug plots to the top and bottom of the graphs when \\(n \\geq
                    125\\) to help highlight how many points fall at various
                    \\(x\\) values.</p>")
        )
    )
)
