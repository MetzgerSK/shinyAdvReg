# shinyAdvReg
<em>For an identical version of this readme that has the repo's contents hidden: [https://MetzgerSK.github.io/shinyAdvReg](https://MetzgerSK.github.io/shinyAdvReg)</em>

[This](https://github.com/MetzgerSK/shinyAdvReg) repository houses various Shiny apps I've written for regression models typically taught in a MLE/GLM course.  Apps will get added as I finish writing and cleaning up the code, so check back from time to time.

By being in a GitHub repo, the apps can be run directly via [Binder](https://mybinder.org) using the links below.  Other than clicking the link and waiting <~60 seconds, there's nothing additional you need to install or do.

:arrow_right: **TL;DR: Click <span style="vertical-align:middle;">![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)</span> to launch an app.**

## The Apps

### Binary DV
- `mleLogit`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/mleLogit/)</span><br>
> See <a href="https://github.com/MetzgerSK/shinyElement#ch-2">`shinyElements/mleLogit`</a>

- `lp_hetOV`<span style="font-size:0.75em;">:pencil2:</span>&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyAdvReg/major?urlpath=shiny/lp_hetOV/)</span> <span style="font-size:0.75em;">:hourglass_flowing_sand:</span><br>
Examines the impact of (a) omitting a relevant covariate and/or (b) heteroskedastic errors on the estimates from binary models.  Features basic logit/probit models, as well as heteroskedastic logit/probit models.
	
### Ordinal DV
- `mleOlogit`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyAdvReg/major?urlpath=shiny/mleOlogit/)</span><br>
Focuses on the intuition behind maximum likelihood using an ordered logit model.  Students manually find the maximum of the model's likelihood function for a toy set of data.  `mleOlogit` displays two graphs with the proposed best-fit line and fake data, and this line updates as students select different slope + cutpoint values.
- `ord_mnl` <span style="font-size:0.75em;">:blue_book::pencil2:</span>&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyAdvReg/major?urlpath=shiny/ord_mnl/)</span> <span style="font-size:0.75em;">:hourglass_flowing_sand:</span><br>
Examines the ramifications of estimating an ordered logit model using truly nominal data, and a multinomial logit model using truly ordinal data.

### Nominal DV
- `ord_mnl`
> See [above](https://github.com/MetzgerSK/shinyAdvReg#ordinal-dv).

### Count DV
- `countOverZI` <span style="font-size:0.75em;">:blue_book::pencil2:</span>&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyAdvReg/major?urlpath=shiny/countOverZI/)</span> <span style="font-size:0.75em;">:hourglass_flowing_sand:</span><br>
Examines effect of overdispersion and/or a zero-inflated component on count model estimates.  Features Poisson, negative binomial, zero-inflated Poisson, and zero-inflated negative binomial models.

### Duration DV
- `whySurv` &nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/whySurv/)</span> <span style="font-size:0.75em;">:hourglass_flowing_sand:</span><br>
> See <a href="https://github.com/MetzgerSK/shinyElement#ch-2">`shinyElements/whySurv`</a>
	
## Notes
- The apps may take a minute or two to launch.  Hang tight.  
	- The Binder page should load in a new window/tab once you click the link.  If it doesn't, click the link again or manually copy/paste the URL into a new window/tab.
	- On the Binder page, as long as `Found built image, launching...` appears under the "Build Logs" header (click 'show' link at header's right to display the log), things are working fine.  
	- <span style="font-size:0.75em;">:hourglass_flowing_sand:</span> = more complex apps. They'll take longer than the others to launch.
	- If there's no text in the log after 15 seconds or so, refresh the page.  Rinse and repeat until the app loads.
	- If the app doesn't load within a minute or two after `server running at https://...` displays (it should auto-redirect), either type that URL manually into your browser or hit refresh. Rinse and repeat until the app loads.
- Every click launches a separate Binder instance, even for the same app.  This means multiple people can use the same app at once without any of the bottlenecking issues endemic to Shiny that stem from R being single-threaded.  
- <a name="fn1_ret"></a>To launch a different app, either come back to the GitHub repo and click the app's Binder link *or* hit :back: twice.<sup><a href="#fn1">1</a></sup> 

## Technical Notes
- All the apps' Docker images have been prebuilt.  
- Binder suggests each Binder'd GitHub repo can support ~100 simultaneous users.  My own rough tests suggest at least 50, under certain circumstances.
- With how Binder's memory allocation works, the more any of the apps get used, the faster any of them will load.
- If you fork any of the apps and make new commits in your forked repo, you'll have to go to the Binder [homepage](https://mybinder.org/) to generate a new link before you can run it on Binder.  When you first access the generated link, Binder will build the Docker image.  That usually takes anywhere from 20&ndash;40 minutes for the apps in their current state, depending.
- Once a Binder is running, you can edit and run the code for *any* of the repo's apps in a RStudio session by changing the end of the URL from <code>shiny/<em>appName</em>?token=...</code> to <code>rstudio/?token=...</code>.  (Others won't be able to see your edits.) <br/> > <ins>Note</ins>: Any changes you make to the code **won't be stored**.  Save local copies of anything you need before closing the window/tab.

## Other Apps
There are additional Shiny apps for linear regression models [here](https://github.com/MetzgerSK/shinyElement).

## Hat Tips
- :blue_book: = original simulation code from [Carsey](https://politicalscience.unc.edu/staff/https-sites-google-com-view-tom-carsey-home/) and [Harden](https://jharden.nd.edu/)'s [*Monte Carlo Simulation and Resampling Methods for Social Science*](https://us.sagepub.com/en-us/nam/monte-carlo-simulation-and-resampling-methods-for-social-science/book241131)  
- :pencil2: = conversion from C&H's code to Shiny app by [Janet Lawler](https://politics.virginia.edu/janet-lawler/), Metzger's RA.  Final streamlining, spiffing, adding of pedagogical details by Metzger.

## License
[MIT](https://choosealicense.com/licenses/mit/)
<br/>
<br/>

----
<a name="fn1">1</a>: You can also technically load another app from the GitHub repo by modifying your current Binder session.  The end of the current app's URL will be <code>shiny/<em>appName</em>?token=...</code>.  Replace <code><em>appName</em></code> in the URL with the other app's name and hit <kbd>Enter</kbd> to load. <span style="font-size:0.75em"><a href="#fn1_ret">↩</a></span>