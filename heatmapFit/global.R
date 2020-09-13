library(shiny)
library(shinyjs)
library(magrittr)
library(haven)
library(shinyWidgets)
library(shinycssloaders)

# Referenced w/::s
## shinythemes

#******************************************************************
# Doing some gymnastics for heatmapFit::heatmap.fit to make it app-friendly.
#**********************
## Print progress bar as message, not cat
source("globalPt_msg_progress_bar.R", local=TRUE)

## Modified heatmap.fit function from heatmapFit 
### (swaps cat() for message(), also returns funct's entire environment)
source("globalPt_heatmap.fitMod.R", local=TRUE)

# Heatmap plot function
hmfPlot <- function(x, ...) {
    # Load the environment into this one. (So very hacky, but.)
    objs <- ls(envir=x)
    for(i in 1:length(objs)){
        assign(paste0(objs[i]),
               get(objs[i], envir=x))
    }

    # Start plot (from calc.boot==TRUE chunk of heatmap.fit())
    def.par <- par(no.readonly = TRUE)
    o <- order(pred)
    nf <- layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(0.75,
        0.25), heights = 1)
    par(oma = c(0, 0, 3, 0))
 
    y.offset <- 0.1 * (max(c(pred, y.obs)) - min(c(pred,
        y.obs)))

    plot(y.obs[o] ~ pred[o], type = "n", ylim = c(min(c(pred,
        y.obs)) - y.offset, max(c(pred, y.obs))), ylab = "Smoothed Empirical Pr(y=1)",
        xlab = "Model Prediction, Pr(y=1)", main = "Heat Map Plot")
    
    
    f <- cbind(yo, pr)
    if (color == T) {
        for (i in 1:length(pr)) {
            segments(f[i, 2] - (tick/2), f[i, 1], f[i, 2] +
                (tick/2), f[i, 1], col = rgb(red = 2 * 255 *
                (0.5 - y.obs.prob2[i]), green = 0, blue = 2 *
                255 * (y.obs.prob2[i]), maxColorValue = 255),
                lwd = 5)
        }
    }
    else {
        for (i in 1:length(pr)) {
            segments(f[i, 2] - (tick/2), f[i, 1], f[i, 2] +
                (tick/2), f[i, 1], col = gray((1/0.6) * (y.obs.prob2[i])),
                lwd = 5)
        }
    }
    abline(0, 1, lty = 2)
    if (legend == T) {
        legend("topleft", lty = c(1, 2), lwd = c(5, 1),
            legend = c("heat map line", "perfect fit"))
    }
    par(new = T)
    if (comp.switch == 1) {
        h <- hist(pred.old, breaks = 50, plot = F)
        h$density <- h$density * (0.1/max(h$density))
        plot(h, freq = F, ylim = c(0.04, 1), xlim = c(min(pred),
            max(pred)), axes = F, ylab = "", xlab = "",
            main = "", col = "black")
    }
    else {
        h <- hist(pred, breaks = 50, plot = F)
        h$density <- h$density * (0.1/max(h$density))
        plot(h, freq = F, ylim = c(0.04, 1), xlim = c(min(pred),
            max(pred)), axes = F, ylab = "", xlab = "",
            main = "", col = "black")
    }
    par(mar = c(3, 0, 3, 0))
    clr <- seq(0.001, 0.499, by = 0.001)
    x.clr <- rep(5, length(clr))
    CLR <- cbind(clr, x.clr)
    if (color == T) {
        plot(CLR[, 1] ~ CLR[, 2], bty = "n", pch = 15, xaxt = "n",
            yaxt = "n", xlab = " ", ylab = " ", main = "p-Value\nLegend",
            xlim = c(4.9, 5.1), col = rgb(red = 2 * 255 *
                (0.5 - CLR[, 1]), green = 0, blue = 2 * 255 *
                (CLR[, 1]), maxColorValue = 255))
    }
    else {
        plot(CLR[, 1] ~ CLR[, 2], bty = "n", pch = 15, xaxt = "n",
            yaxt = "n", xlab = " ", ylab = " ", main = "p-Value\nLegend",
            xlim = c(4.9, 5.1), col = gray((1/0.6) * (CLR[,
                1])))
    }
    axis(2, at = c(seq(0, 0.5, by = 0.1)), labels = c(0.01,
        seq(0.1, 0.5, by = 0.1)), line = -2, las = 2)
    mtext("Predicted Probability Deviation \n Model Predictions vs. Empirical Frequency",
        outer = T, line = 0, adj = 0.5)
    par(def.par)
}
