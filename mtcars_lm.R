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
