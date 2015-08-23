lm_mtcars_1 <- function(df, field) {
        suppressMessages(library(plyr))
        ret <- function(nm) {
                if (nm == "df")
                        df1
                else if (nm == "field")
                        field1
                else if (nm == "allfits")
                        allfits
                else if (nm == "fitlist")
                        fitList
                else if (nm == "formulalist")
                        formulaList
        }
        df1 = df
        field1 = field
        fields <- names(df)
        ofields <- fields[fields != field1 & fields != "mpg"]
        fnpx <- function(main, others) {
                ret = list()
                i = 1
                ret[i] = paste(main, paste(others, collapse = " + "), sep = " + ")
                i = i + 1
                for (j in 1:length(others)) {
                        oth = others
                        oth[j] = paste(main, oth[j], sep = " * ")
                        ret[i] = paste(oth, collapse = " + ")
                        i = i + 1
                }
                if (length(others) > 1) {
                        ret[i] = paste(paste(main, paste(others, collapse = " + "), sep = " * ("), ")")
                }
                paste(c("mpg"), ret, sep = " ~ ")
        }
        fitList <- list()
        formulaList <- character()
        cnt <- 1
        for (i in 1:2) {
                combs <- combn(ofields, i)
                for (j in 1:(length(combs)/2)) {
                        vars <- combs[,j]
                        for (lmf in fnpx(field1, vars)) {
                                fit <- lm(as.formula(lmf), data = df)
                                fitList[[cnt]] <- fit
                                formulaList <- c(formulaList, lmf)
                                cnt <- cnt + 1
                        }
                }
        }
        fitList2 <- lapply(fitList, function(x) {
                s = summary(x)
                list(sigma = s$sigma, adjrsq = s$adj.r.squared, maxp = max(s$coef[,4]))
        })
        d1 <- ldply(fitList2, data.frame)
        d2 <- data.frame(call = formulaList)
        allfits <- cbind(d2, d1)
        ret
}

multisimulation <- function(topcnt) {
        models <- lm_mtcars_1(df = mtcars1, field = "am")
        allfits <- models("allfits")
        topfits <- allfits %>%
                filter(maxp < 0.1) %>%
                mutate(adjrsq = round(adjrsq,2), sigma = round(sigma,2)) %>%
                arrange(desc(adjrsq), maxp, sigma)
        topfits <- topfits[1:topcnt,]
        kable(topfits)
}

mpgbyam_boxplot <- function() {
        library(ggplot2, quietly = TRUE)
        g <- ggplot(mtcars, aes(x = factor(am), y = mpg))
        g <- g + geom_boxplot()
        g <- g + xlab("Transmission (1=Manual, 0=Automatic)") + ylab("MPG")
        g <- g + ggtitle("MPG by Transmission Type")
        print(g)
        # quantile(mtcars$mpg[mtcars$am == 0])
        # quantile(mtcars$mpg[mtcars$am == 1])
}

mpg_by_transmission_plot <- function() {
        plot(mtcars$am, mtcars$mpg, cex=1, pch=21, bg="lightblue", xlab="Transmission (1=Manual, 0=Automatic)", ylab="MPG")
        title("MPG by Transmission Type")
        abline(h=min(mtcars[mtcars$am == 1,]$mpg), col="red", lwd=2)
        abline(h=max(mtcars[mtcars$am == 0,]$mpg+0.2), col="blue", lwd=2)
        abline(s_fit_am$coef[1,1], s_fit_am$coef[2,1])
        abline(h=s_fit_am$coef[1,1], col="green", lwd=2)
        abline(h=s_fit_am$coef[1,1] + s_fit_am$coef[2,1], col="green", lwd=2)
}

co.pal <- c("lightblue", "blue", "yellowgreen", "seagreen", "plum1", "red")

mpg_by_am_wt_interaction <- function() {
        par(mfrow=c(2,1))
        plot(x = mtcars$wt, y = mtcars$mpg, type = 'n', xlab = "Weight (lb/100)", ylab = "MPG")
        title("Regression mpg ~ am * wt")
        points(x = mtcars$wt[mtcars$am==0], y = mtcars$mpg[mtcars$am==0], cex = 2, pch = 21, bg = co.pal[1], col = "black")
        points(x = mtcars$wt[mtcars$am==1], y = mtcars$mpg[mtcars$am==1], cex = 2, pch = 21, bg = co.pal[2], col = "black")
        s01c <- s01$coeff[,1]
        abline(s01c[1], s01c[3], lty=1, lwd=2, col=co.pal[1])
        abline(s01c[1] + s01c[2], s01c[3] + s01c[4], lty=1, lwd=2, col=co.pal[2])
        s1c <- s1$coeff[,1]
        abline(s1c[1], s1c[2], lty=1, lwd=2, col="black")
        legends <- c("Automatic", "Manual", "Overall")
        legendcolors <- c(co.pal[1:2], "black")
        legend(x = 'topright', legend = legends, lty=c(1,1), lwd = 2, col = legendcolors)
        plot(x = mtcars$mpg, y = resid(f01), type = 'n', xlab = "MPG", ylab = "Residuals")
        title("Residuals mpg ~ am * wt")
        points(x = mtcars$mpg[mtcars$am==0], y = resid(f01)[mtcars$am==0], cex = 1, pch = 21, bg = co.pal[1], col = "black")
        points(x = mtcars$mpg[mtcars$am==1], y = resid(f01)[mtcars$am==1], cex = 1, pch = 21, bg = co.pal[2], col = "black")
        legends2 <- c("Automatic", "Manual")
        legendcolors2 <- c(co.pal[1:2])
        legend(x = 'topleft', legend = legends2, lty=c(1,1), lwd = 2, col = legendcolors2)
        # points(x = mtcars$mpg, y = resid(lm(mpg ~ am, data=mtcars1)), cex = 1, pch = 21, bg = "black", col = "black")
}

finalmodel <- function() {
        fitx <- lm(mpg ~ am * wt + vs, data = mtcars1); sx <- summary(fitx)
        print(sx)
        cx <- sx$coef[,1]
        am0 <- mtcars$am == 0; am1 <- mtcars$am == 1
        vs0 <- mtcars$vs == 0; vs1 <- mtcars$vs == 1
        usecex = 1
        par(mfrow=c(1,2))
        plot(x = mtcars$wt, y = mtcars$mpg, type = 'n', xlab = "Weight (lb/1000)", ylab = "MPG")
        title("Automatic Transmission")
        points(x = mtcars$wt[am0 & vs0], y = mtcars$mpg[am0 & vs0], cex=usecex, pch=21, bg=co.pal[1])
        abline(cx[1], cx[3], lwd=2, lty=1, col=co.pal[1])
        points(x = mtcars$wt[am0 & vs1], y = mtcars$mpg[am0 & vs1], cex=usecex, pch=21, bg=co.pal[2])
        abline(cx[1]+cx[4], cx[3], lwd=2, lty=1, col=co.pal[2])
        legend(x = 'topright', legend = c("V", "S"), lty=c(1,1), lwd = 2, col = co.pal[1:2])
        plot(x = mtcars$wt, y = mtcars$mpg, type = 'n', xlab = "Weight (lb/1000)", ylab = "MPG")
        title("Manual Transmission")
        points(x = mtcars$wt[am1 & vs0], y = mtcars$mpg[am1 & vs0], cex=usecex, pch=21, bg=co.pal[1])
        abline(cx[1] + cx[2], cx[3] + cx[5], lwd=2, lty=1, col=co.pal[1])
        points(x = mtcars$wt[am1 & vs1], y = mtcars$mpg[am1 & vs1], cex=usecex, pch=21, bg=co.pal[2])
        abline(cx[1] + cx[2] + cx[4], cx[3] + cx[5], lwd=2, lty=1, col=co.pal[2])
        legend(x = 'topright', legend = c("V", "S"), lty=c(1,1), lwd = 2, col = co.pal[1:2])
}

finalresidualplot <- function() {
        fitx <- lm(mpg ~ am * wt + vs, data = mtcars1); sx <- summary(fitx)
        par(mfrow=c(1,1))
        plot(x = mtcars$mpg, y = resid(fitx), type = 'n', xlab = "MPG", ylab = "Residuals")
        title("Residuals mpg ~ am * wt + vs")
        points(x = mtcars$mpg[mtcars$am==0], y = resid(fitx)[mtcars$am==0], cex = 1, pch = 21, bg = co.pal[1], col = "black")
        points(x = mtcars$mpg[mtcars$am==1], y = resid(fitx)[mtcars$am==1], cex = 1, pch = 21, bg = co.pal[2], col = "black")
        legends2 <- c("Automatic", "Manual")
        legendcolors2 <- c(co.pal[1:2])
        legend(x = 'topleft', legend = legends2, lty=c(1,1), lwd = 2, col = legendcolors2)
}
