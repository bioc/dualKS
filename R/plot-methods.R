setMethod("plot", c("DKSPredicted", "missing"), 
    function(x, y, ...) {
        prediction <- x
        arg <- as.list(substitute(list(...)))
        if(length(arg) > 1) {
            actual <- eval(parse(text=arg[2]))
        } else {
            actual <- NULL
        }
        n <- length(levels(prediction@predictedClass)) + 1
        nc <- round(sqrt(n))
        if (nc^2 < n) {
            nr <- nc + 1
        }
        else {
            nr <- nc
        }
        par(xpd = TRUE, mar = c(3, 6, 2.5, 1), mfcol = c(nr, nc))
        scores <- prediction@scoreMatrix
        predicted <- prediction@predictedClass
        cols <- rainbow(length(levels(predicted)))
        for (i in 1:ncol(scores)) {
            ix <- sort(as.numeric(scores[, i]), index.return = TRUE, 
                decreasing = TRUE)$ix
            plot(scores[ix, 1], type = "l", col = cols[1], ylim = range(scores), 
                ylab = NA, xlab = NA, axes = FALSE)
            par(new = TRUE, xpd=FALSE)
            for (ii in seq(0, range(scores)[2], 1000)) {
                lines(c(0, nrow(scores)), c(ii, ii), lty = 1, lwd = 0.5, 
                    col = "#EEEEEE")
                par(new = TRUE)
            }
            plot(scores[ix, 1], type = "l", col = cols[1], ylim = range(scores), 
                ylab = NA, xlab = NA, axes = FALSE)
            par(new = TRUE, xpd=TRUE)
            for (ii in 2:ncol(scores)) {
                plot(scores[ix, ii], type = "l", col = cols[ii], 
                    ylim = range(scores), ylab = NA, xlab = NA, axes = FALSE)
                par(new = TRUE)
            }
            plot(scores[ix, i], type = "l", col = cols[i], ylim = range(scores), 
                lwd = 3, ylab = NA, xlab = NA, axes = FALSE)
            lx <- par("usr")[1] - 0.1 * (par("usr")[2] - par("usr")[1])
            ty1 <- par("usr")[4] + 0.17 * (par("usr")[4] - par("usr")[3])
            ty2 <- par("usr")[4] + 0.07 * (par("usr")[4] - par("usr")[3])
            py <- par("usr")[3] - 0.06 * (par("usr")[4] - par("usr")[3])
            ay <- par("usr")[3] - 0.12 * (par("usr")[4] - par("usr")[3])
            text(1, ty1, adj = 0, paste("samples ordered by:"), cex = 0.7, 
                font = 1)
            text(1, ty2, adj = 0, paste(colnames(scores)[i], "signature"), 
                cex = 1.3, col = cols[i], font = 2)
            par(new = FALSE, xpd = TRUE)
            if (!is.null(actual)) {
                text(1:nrow(scores), ay, "|", col = cols[match(actual[ix], 
                    colnames(scores))], cex = 1, font = 2)
                text(0, ay, "ACTUAL:", cex = 0.7, font = 2, adj = 1)
            }
            text(1:nrow(scores), py, "|", col = cols[match(predicted[ix], 
                colnames(scores))], cex = 1, font = 2)
            text(0, py, "PREDICTED:", cex = 0.7, font = 2, adj = 1)
            axis(2, cex.axis = 0.7)
            par(mgp = c(2, 1, 0))
            title(ylab = "KS SCORE", font.lab = 2, cex.lab = 0.8)
        }
        plot(1, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, 
            xlab = NA, ylab = NA)
        legend("topleft", col = cols, legend = colnames(scores), 
            lwd = 3, box.lty = 0, title = "SIGNATURE KEY:")
    })
    
