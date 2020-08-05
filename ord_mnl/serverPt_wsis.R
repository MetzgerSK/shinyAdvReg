# WSIS conditionals ----
output$wsis <- renderUI({
	# Model we selected
	ifelse(input$model=="ord_model", {selMod <- "Ordinal logit";     aAn <-"an "},
									 {selMod <- "Multinomial logit"; aAn <-"a "})
	
	# Other model
	ifelse(input$model!="ord_model", {othMod <- "Ordinal logit"},
									 {othMod <- "Multinomial logit"})
	
	# Get appropriate header
	fluidRow(style="margin-left:0px !important; margin-right:0px !important",
		h4(
		  paste0("What happens when I estimate ", aAn, tolower(selMod),
			" when the data are truly ",
			ifelse(simDGP()=="ord_dgp", "ordinal ",
										"nominal "
			),
			"in nature?"
		  )
		),
		
		# If everything matches
		if(substr(simDGP(), 1, 3)==substr(input$model, 1, 3)){
			HTML(paste0("<p>The DGP and model match.  ", selMod, "'s estimates will be
				 <em>unbiased</em> and <em>more efficient</em>
				 (relative to ", tolower(othMod),").</p>"))
		
		# Mismatch    
		} else {
			# The wasteful mismatch
			if(simDGP()=="ord_dgp"){
				HTML("<p>An ordinal DGP produces an observed DV whose
				values allow us to order our observations from those possessing most 
				to least of the latent concept the DV measures.  However, a multinomial logit
				model assumes the observed DV conveys no information about ordering.</p>
				
				<p>As a result, while the MNL estimates will be <em> unbiased</em>,
				they will be <em>less efficient</em> than the ordered
				logit estimates because MNL does not use all the information
				conveyed to us by the DV's values.</p>")
				
			# The harmful mismatch
			} else {
				HTML("<p>An nominal DGP produces an observed DV whose
				values only allow us to place our observations into categories.
				These categories convey no information about whether an observation
				has more or less of the DV's latent concept than other observations.
				However, an ordinal logit model presumes information regarding such
				an ordering is present in the DV's values.</p>  
				<p>As a result, the ordered logit estimates will be <em> biased</em>.
				(With the bias, ordered logit's <em>efficiency</em> (relative to MNL)
				isn't particularly sensical to think about.)</p>")
			}
		},
		
		h4(style="margin-top: 30px !important;", "Where should I look to see that?"),
		h5("On the 'Main' tab..."),
		# UNBIASED
		if(substr(simDGP(), 1, 3)==substr(input$model, 1, 3) | 
		   simDGP()=="ord_dgp"){
			
			p(strong("Unbiased:"), 
					"Top two rows should match/be close for all columns; 
					top row should fall in between values in 3rd and 4th rows for all columns.")
		# BIASED
		} else {
			HTML("<p><strong>Biased:</strong> 
				Top two rows won't match for <em>x</em>'s coefficient; top row won't 
				fall in between values in 3rd and 4th rows for this column.</p>")
		},
		
		# MORE EFFICIENT
		if(substr(simDGP(), 1, 3)==substr(input$model, 1, 3)){
			p(strong("More Efficient:"), paste0("StDev of estimate will be smaller for ",
				tolower(selMod), " compared to StDev of estimate from ", tolower(othMod), "."))                                  
		
		# LESS EFFICIENT
		} else if(simDGP()=="ord_dgp") {
			p(strong("Less Efficient:"), paste0("StDev of estimate will be larger for ",
				tolower(selMod), " compared to StDev of estimate from ", tolower(othMod), "."))  
		}
	)
})