# Could trim a bit of this, since the final plot's not generated here, plus the 
# bootstraps are hardcoded, but just faster to keep the whole thing and tweak 
# what needs tweaking to get everything to work.


# ORIGINAL SOURCE: heatmapFit::heatmap.fit()

heatmap.fitMod <- function (y, pred, calc.boot = TRUE, reps = 1000, span.l = "aicc", 
    color = FALSE, compress.obs = TRUE, init.grid = 2000, ret.obs = FALSE, 
    legend = TRUE) 
{
    temp.dat <- na.omit(data.frame(y = y, pred = pred))
    YY <- temp.dat$y
    pred <- temp.dat$pred
    n <- length(YY)
    if (min(YY == 1 | YY == 0) == 0) {
        stop("non-binary dependent variable detected; ensure that y is in {0, 1}", 
            "\n \n")
    }
    if (max(pred > 1 | pred < 0) == 1) {
        stop("improper values of pred detected; predicted probabilities must be on the interval [0,1]", 
            "\n \n")
    }
    if (compress.obs == T & n > 10000) {
        comp.switch <- 1
    }
    else {
        comp.switch <- 0
    }
    if (compress.obs == F & n > 10000) {
        message("Note: program is running w/o compression on a very large data set", 
            "\n")
        message("This procedure will be time-consuming...", "\n")
    }
    if (comp.switch == 1) {
        message("Collapsing large data set into bins based on predicted Pr(y)...", 
            "\n")
        YY.old <- YY
        pred.old <- pred
        n.old <- n
        list.compress <- heatmap.compress(YY, pred, init.grid)
        YY <- list.compress$y.out
        pred <- signif(list.compress$pred.out, 10)
        lo.weight <- list.compress$weight.out
        n <- length(YY)
        message("Data collapsed into ", length(unique(pred)), " weighted bins.", 
            "\n", sep = "")
    }
    else {
        lo.weight <- rep(1, n)
    }
    if (n >= 1000) {
        trace.hat.arg <- "approximate"
    }
    else {
        trace.hat.arg <- "exact"
    }
    if (span.l == "aicc" | span.l == "gcv") {
        loess.aic <- function(x) {
            if (!(inherits(x, "loess"))) 
                stop("Error: argument must be a loess object")
            span <- x$pars$span
            n <- x$n
            traceL <- x$trace.hat
            sigma2 <- (x$s)^2
            delta1 <- x$one.delta
            delta2 <- x$two.delta
            enp <- x$enp
            aicc <- log(sigma2) + 1 + 2 * (2 * (traceL + 1))/(n - 
                traceL - 2)
            aicc1 <- n * log(sigma2) + n * ((delta1/delta2) * 
                (n + enp)/(delta1^2/delta2) - 2)
            gcv <- n * sigma2/(n - traceL)^2
            result <- list(span = span, aicc = aicc, aicc1 = aicc1, 
                gcv = gcv)
            return(result)
        }
        smooth.err <- function(span.arg) {
            ok <- T
            plot.model <- withCallingHandlers(tryCatch(loess(YY ~ 
                pred, degree = 1, weights = lo.weight, span = span.arg, 
                control = loess.control(trace.hat = trace.hat.arg))), 
                warning = function(w) {
                    ok <<- F
                    invokeRestart("muffleWarning")
                })
            if (ok == T) {
                return(eval(parse(text = paste("loess.aic(plot.model)$", 
                    span.l, sep = ""))))
            }
            if (ok == F) {
                return(2e+10)
            }
        }
        message("Calculating optimal loess bandwith...") 
        span.l.name <- span.l
        span.l <- optimize(f = smooth.err, interval = c(0.01, 
            0.99))$minimum
        message(span.l.name, " Chosen Span = ", round(span.l, 7), "\n") 
    }
    ok <- T
    plot.model <- withCallingHandlers(tryCatch(loess(YY ~ pred, 
        degree = 1, weights = lo.weight, span = span.l, control = loess.control(trace.hat = trace.hat.arg))), 
        warning = function(w) {
            ok <<- F
            invokeRestart("muffleWarning")
        })
    if (ok == F) {
        message("Defaulting to span = 0.75", "\n") #, "\n")
        span.l <- 0.75
        plot.model <- loess(YY ~ pred, degree = 1, weights = lo.weight, 
            span = span.l, control = loess.control(trace.hat = trace.hat.arg))
    }
    y.obs <- predict(plot.model, newdata = pred)
    for (j in 1:length(y.obs)) {
        y.obs[j] <- max(min(y.obs[j], 1), 0)
    }
    tick <- (max(pred) - min(pred))/500
    pr <- seq(from = min(pred), to = max(pred), by = tick)
    yo <- predict(plot.model, newdata = pr)
    for (j in 1:length(yo)) {
        yo[j] <- max(min(yo[j], 1), 0)
    }
    if (calc.boot == TRUE) {
        message(c("Generating Bootstrap Predictions...", "\n"))
        btstrp <- function() {
            y.obs.boot.count <- matrix(data = 0, nrow = 1, ncol = length(pred))
            y.obs.bs.count <- matrix(data = 0, nrow = 1, ncol = length(pr))
            y.obs.boot.count.2 <- matrix(data = 0, nrow = 1, 
                ncol = length(pred))
            y.obs.bs.count.2 <- matrix(data = 0, nrow = 1, ncol = length(pr))
           # pb <- txtProgressBar(min = 0, max = reps, style = 3)
            pb <- msg_progress_bar(iter=reps)
            
            for (i in 1:reps) {
                #setTxtProgressBar(pb, i)
                pb$increment()
                if (comp.switch == 1) {
                    boot.y.1 <- rbinom(n = length(list.compress$n.out), 
                        size = list.compress$n.out, prob = list.compress$pred.total.out)
                    boot.y.0 <- list.compress$n.out - boot.y.1
                    boot.y <- c(rep(0, length(boot.y.0)), rep(1, 
                        length(boot.y.1)))
                    boot.pred <- signif(rep(list.compress$pred.total.out, 
                        2), 10)
                    boot.weight <- ((boot.y.1 + boot.y.0)/sum(boot.y.1 + 
                        boot.y.0)) * c((boot.y.0/(boot.y.1 + boot.y.0)), 
                        (boot.y.1/(boot.y.1 + boot.y.0)))
                }
                else {
                    boot.pred <- pred
                    boot.y <- ifelse(runif(n, min = 0, max = 1) < 
                        boot.pred, 1, 0)
                    boot.weight <- rep(1, length(boot.y))
                }
                plot.model3 <- withCallingHandlers(tryCatch(loess(boot.y ~ 
                    boot.pred, degree = 1, weights = boot.weight, 
                    span = span.l, control = loess.control(trace.hat = trace.hat.arg))), 
                    warning = function(w) {
                        invokeRestart("muffleWarning")
                    })
                y.obs.boot <- predict(plot.model3, newdata = boot.pred)
                y.obs.boot.two <- predict(plot.model3, newdata = pr)
                if (comp.switch == 1) {
                    y.obs.boot <- y.obs.boot[list.compress$retained.obs]
                }
                for (j in 1:length(y.obs.boot)) {
                    y.obs.boot[j] <- max(min(y.obs.boot[j], 1), 
                        0)
                }
                for (j in 1:length(y.obs.boot.two)) {
                    y.obs.boot.two[j] <- max(min(y.obs.boot.two[j], 
                        1), 0)
                }
                y.obs.boot.count <- y.obs.boot.count + as.numeric(y.obs <= 
                    y.obs.boot)
                y.obs.bs.count <- y.obs.bs.count + as.numeric(yo <= 
                    y.obs.boot.two)
                y.obs.boot.count.2 <- y.obs.boot.count.2 + as.numeric(y.obs < 
                    y.obs.boot)
                y.obs.bs.count.2 <- y.obs.bs.count.2 + as.numeric(yo < 
                    y.obs.boot.two)
            }
            y.obs.boot.count <- (y.obs.boot.count + y.obs.boot.count.2)/2
            y.obs.bs.count <- (y.obs.bs.count + y.obs.bs.count.2)/2
            return(list(y.obs.boot.count, y.obs.bs.count))
            close(pb)
        }
        return.list <- btstrp()
        y.obs.boot.count <- return.list[[1]]
        y.obs.bs.count <- return.list[[2]]
        y.obs.prob.t <- y.obs.boot.count/reps
        y.obs.prob2.t <- y.obs.bs.count/reps
        y.obs.prob <- pmin(y.obs.prob.t, 1 - y.obs.prob.t)
        y.obs.prob2 <- pmin(y.obs.prob2.t, 1 - y.obs.prob2.t)

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

        out1 <- y.obs.prob
        heatmapstat <- sum(as.numeric(out1 <= 0.1) * lo.weight)/sum(lo.weight)
        message("\n") 
        message("*******************************************")
        message(heatmapstat * 100, "% of Observations have one-tailed p-value <= 0.1")
        message("Expected Maximum = 20%") 
        message("*******************************************")  

    }
    else {
        par(oma = c(1, 0, 3, 0))
        y.offset <- 0.1 * (max(c(pred, y.obs)) - min(c(pred, 
            y.obs)))
        plot(yo ~ pr, type = "l", lwd = 5, col = "darkgray", 
            ylim = c(min(c(pred, y.obs)) - y.offset, max(c(pred, 
                y.obs))), ylab = "Smoothed Empirical Pr(y=1)", 
            xlab = "Model Prediction, Pr(y=1)", main = "Heat Map Plot")
        abline(0, 1, lty = 2)
        if (legend == T) {
            legend("topleft", lty = c(1, 2), lwd = c(5, 1), 
                col = c("darkgray", "black"), legend = c("heat map line", 
                    "perfect fit"))
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
        mtext("Predicted Probability Deviation \n Model Predictions vs. Empirical Frequency", 
            outer = T, line = 0, adj = 0.5)
        mtext("Note: bootstrap-based p-values not calculated for this plot.", 
            outer = T, side = 1, line = 0, adj = 0.5, cex = 0.8)
    }
    
    return(hmf.env=environment())
}
