jqui_draggable(
    
    bsModal("modal_expl", "What's going on here?", "explainButton", size = "large",
            
            
        h4("The Situation"),
        p( "You have a dataset whose points are displayed in blue. Your
        dependent variable's values are integers that are ordinal in nature,
        ranging from 1 to 3. You have one independent variable (horizontal
        axis), \\( x \\), which (arbitrarily) is distributed normally with a
        mean of 0 and standard deviation of 2."),
    
        
        h4("Your Oveararching Goal"),
        p( "You'd like to estimate a regression model using these data.  That
        means you'd like to use values of \\(x \\) to predict values of \\( y \\).
        Because your dependent variable is ordinal, we've chosen to estimate an
        ordered logit regression."),
    
        
        h4("What needs to happen to reach my overarching goal?"),
        
        HTML("<p>R uses maximum likelihood estimation (MLE) to find the
        best-fitting line for ordered logit models.  Ordered regression models
        involve slope coefficients for any covariates, but also involve
        multiple \"cutpoints\" that act like intercepts. Look at how many
        values \\(y\\) takes, subtract 1, and that's how many cutpoints your
        regression will have (for our example here, \\(\\tau_1,
        \\tau_2\\)).<sup><a id='fn1_ret'></a><a href=\'#fn1\'>[1]</a>,<a
        id='fn2_ret'></a><a href=\'#fn2\'>[2]</a></sup></p>

        <p>Informally, when R uses MLE, it goes through all the possible
        \\(\\tau_1\\) and \\(\\tau_2\\) values, forms combinations with all
        the possible slope values for \\( x \\), and sees which of the
        combinations is \"most likely\" responsible for generating the
        patterns you see in your dataset. (Note: R is not *actually* going
        through every slope-cutpoint combination.  It uses various
        optimization rules to hone in on the likely candidates
        quickly.)</p>"),
        
        p("R isn't directing the search for the best slope and cutpoint values, 
        though.  ", strong("YOU"), "are."),
       
        HTML("<p>You have (an artificially limited range of) slope and
        cutpoint combinations to check manually, using the sliders in the left
        sidebar.  One way we can conceive of ordered models is as a series of
        cumulative distribution functions (CDF)&mdash;the probability of
        \\(y\\) being larger than 1, then \\(y\\) being  larger than 2 (and so
        on, if \\(y\\) had more three categories).
            <ul> 
                <li>Left graph: Focuses on \\(y>1\\), amounting to the
                probability that \\(y\\) is equal to 2 or 3.  The red line
                corresponds to the logit CDF corresponding to your current slope
                and first cutpoint value.</li>
                <li>Right graph: Focuses on \\(y>2\\), amounting to the
                probability that \\(y\\) is equal to 3.  The red line
                corresponds to the logit CDF corresponding to your current slope
                and second cutpoint value.</li>
            </ul>
            The \"likeliness\" of seeing your data, given these slope and cutpoint
            guesses, is displayed under \"Current Point Total\".
            If you have the prose slider set to \"Formal\", it'll instead read
            \"Current log-likelihood value\". In \"Formal\" prose mode, you'll
            also see a small eyeball button where you can view the actual formula
            for the calculation producing that value, if you'd like.</p>"),
        
       
        h4("Your Immediate Task"),
        HTML("<p>Because you'd like to find the \"most likely\" slope, 
        \\(\\tau_1\\), and \\(\\tau_2\\) values that produced your data, 
        your task is to <strong>find the combination that gives you the LARGEST
        \"point\"/likelihood value possible</strong>.  Be mindful of the
        likelihood's sign as you hunt: likelihood values are almost always
        negative (we log them to protect against really, really big numbers
        that'll eat up all the computer's memory and cause the program to
        crash).</p>"),
          
        p( "The largest likelihood value you've found so far is recorded under
        \"Current all-time best guess\"/\"Maximum log-likelihood value (so far)\".
        The orange \"Restore\" button will set the sliders to the slope and 
        cutpoint values associated with your best guess."),
    
        p( "After you hunt for a bit, clicking \"Show Answer\" will display the
        best slope-\\(\\tau_1\\)-\\(\\tau_2\\) combination that R found."),
    
        div(class="bs-callout bs-callout-info",
            HTML("<h4><i class='fas fa-search'></i> Hint</h4>"),
            HTML(
                "<p>Remember that each graph's line should try to fit the
                    <em>majority</em> of points as well as possible.  <code>mleOlogit</code>
                    adds rug plots to the top and bottom of the graphs when \\(n \\geq
                    125\\) to help highlight how many points fall at various
                    \\(x\\) values.</p>")
        ),

       
        hr(),
        HTML("<p style=font-size:80%;><a id=\'fn1\'></a>1: If we think of our
                ordered DV as deriving from an underlying continuous variable
                that we can't observe directly \\((y^*)\\), the cutpoints are
                the estimated \\(y^*\\) values where a subject's observed
                \\(y\\) value switches from 1 to 2 \\((\\tau_1) \\) and from 2 to
                3 \\((\\tau_2) \\). <a href=\'#fn1_ret\'>&larrhk;</a></p>"),
        HTML("<p style=font-size:80%;><a id=\'fn2\'></a>2: Both <code>polr</code> 
                in R and Stata use this strategy.  There are other equivalent
                parameterizations for ordered logit/probit that estimate
                an explicit intercept, but in return, hold one of the cutpoint
                values at 0. <a href=\'#fn2_ret\'>&larrhk;</a></p>")

    )
)
