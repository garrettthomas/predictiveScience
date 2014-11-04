ebola <- read.csv("~/Dropbox/LEPR01/ebola/data/west.africa.best.csv", stringsAsFactors = FALSE)
ebola[, 1] <- as.Date(ebola[, 1], "%m/%d/%y")

# ebola$Cases_Liberia[3] <- NA
names <- c("Guinea", "Liberia", "Sierra Leone")
png("~/Desktop/plos_legend_figure.png", width = 800)
par(mfcol = c(2, 3))
par(mar = c(5.1, 5.1, 3.1, 5.1))
m <- matrix(c(1, 3, 5, 2, 4, 6, 7, 7, 7), nrow = 3, ncol = 3, byrow = TRUE)

layout(mat = m, heights = c(0.4, 0.4, 0.2))
# txt_file <- '~/Desktop/plos.txt' sink(txt_file) sink(NULL)
for (j in 3:5) {
    # jpeg(paste('~/Desktop/garrett',j,'.jpeg',sep=''), height = 600, width = 1200, quality = 100) par('mfcol' = c(1,2)) par('mar' = c(5.1,5.1,3.1,5.1))
    doubling_times <- data.frame(date = character(0), doubling_time = integer(0))
    for (i in (nrow(ebola) - 1):1) {
        if (!is.na(ebola[i, j])) {
            index <- which(!is.na(ebola[, j]) & ebola[i, 2] - ebola[, 2] > 0)
            if (length(index) != 0) {
                index <- min(index)
                d_time <- (ebola[i, 2] - ebola[index, 2]) * log(2)/log(ebola[i, j]/ebola[index, j])
                if (d_time > 0 & !is.na(d_time)) 
                  doubling_times <- rbind(doubling_times, data.frame(date = ebola[i, 1], doubling_time = d_time, stringsAsFactors = FALSE))
            }
        }
    }
    d_times <- data.frame(date = character(0), doubling_time = integer(0))
    for (i in (nrow(ebola) - 1):1) {
        index <- max(which(!is.na(ebola[, j])))
        if (!is.na(ebola[i, j])) {
            d_time <- (ebola[i, 2] - ebola[index, 2]) * log(2)/log(ebola[i, j]/ebola[index, j])
            if (d_time > 0 & !is.na(d_time) & is.finite(d_time)) 
                d_times <- rbind(d_times, data.frame(date = ebola[i, 1], doubling_time = d_time, stringsAsFactors = FALSE))
        }
    }
    
    plot(x = ebola[which(is.element(ebola[, 1], doubling_times$date)), 1], y = ebola[which(is.element(ebola[, 1], doubling_times$date)), j], type = "l", ylab = "Cumulative Total", 
        xlab = "Date", main = names[j - 2])
    conf_int = quantile(doubling_times[which(is.finite(doubling_times$doubling_time)), 2], probs = c(0.05, 0.95))
    
    lowerq = quantile(doubling_times[which(is.finite(doubling_times$doubling_time)), 2])[2]
    upperq = quantile(doubling_times[which(is.finite(doubling_times$doubling_time)), 2])[4]
    iqr = upperq - lowerq
    
    sd <- sd(doubling_times[which(is.finite(doubling_times$doubling_time) & doubling_times$doubling_time > 0), 2])
    mean <- mean(doubling_times[which(is.finite(doubling_times$doubling_time) & doubling_times$doubling_time > 0), 2])
    
    
    col <- rep("black", nrow(doubling_times))
    col[which(!is.finite(doubling_times$doubling_time))] <- "red"
    doubling_times$doubling_time[which(!is.finite(doubling_times$doubling_time))] <- -1
    col[which((doubling_times[, 2]) > mean + sd)] <- "blue"
    # col[which((doubling_times[, 2]) > upperq + 1.5*iqr)] <- 'blue'
    
    
    plot(doubling_times[, 1], doubling_times[, 2], col = col, pch = 20, ylab = "Doubling Time (days)", xlab = "Date", main = names[j - 2])
    lm <- lm(doubling_time ~ as.numeric(date), doubling_times[which(col == "black"), ])
    # lm <- lm(doubling_time ~ as.numeric(date), doubling_times[which(col != 'red'), ])
    
    conf_int <- quantile(doubling_times$doubling_time[which(col == "black")], c(0.05, 0.95))
    print(names[j - 2])
    cat("Confidence Interval\n")
    print(conf_int)
    cat("Mean Doubling Time (excluding outliers)\n")
    cat(mean(doubling_times$doubling_time[which(col == "black")]))
    cat("\nMedian (excluding outliers)\n")
    cat(median(doubling_times$doubling_time[which(col == "black")]))
    cat("\n\n")
    
    
    abline(lm)
    
    
    # print(lm$coefficients)
    par(new = TRUE)
    # plot(d_times[,1],d_times[,2], ylim=c(0,max(d_times[,2])),'pch' = 20, xlim=c(min(doubling_times[,1]),max(doubling_times[,1])),'ann' = FALSE,
    # col=rainbow(1,start=.5,alpha=.3),axes = FALSE) points(d_times[,1],d_times[,2], 'pch' = 20, col=rainbow(1,start=.5,alpha=.3))#,axes = FALSE)
    black <- which(col != "red")
    running <- data.frame(date = doubling_times[black[1], 1], doubling_time = doubling_times[black[1], 2])
    for (q in 2:(length(black) - 1)) {
        running <- rbind(running, data.frame(date = doubling_times[black[q], 1], doubling_time = mean(doubling_times[black[q - 1:q + 1], 2])))
    }
    lines(running[, 1], running[, 2], col = rainbow(1, start = 0.5, alpha = 0.5), lwd = 3)
    
    # axis(4) mtext(expression('Doubling Time (days) Δt = '*'t'[0]*'to t'[i]), side = 4, line = 3)
    black <- expression("Doubling Time")
    blue <- expression("Doubling Time Outliers")
    red <- expression("No New Cases")
    light_blue <- expression("Running Mean")
    # a <- predict(lm,date=as.numeric(doubling_times$date[which(col == 'black'),]), interval = 'confidence') lines(doubling_times[which(col == 'black'),1],a[,2])
    # lines(doubling_times[which(col == 'black'),1],a[,3])
    
    # sink(txt_file,append = TRUE) print(names[j-2],font=2) print('Mean Doubling Time From Previous') print(mean(doubling_times$doubling_time)) print('Median Doubling
    # Time From Previous') print(median(doubling_times$doubling_time)) print('Mean Doubling Time From Previous Excluding Outliers')
    # print(mean(doubling_times$doubling_time[which(col=='black')])) print('Median Doubling Time From Previous Excluding Outliers')
    # print(median(doubling_times$doubling_time[which(col=='black')])) print('Mean Doubling Time From First') print(mean(d_times$doubling_time)) print('Median
    # Doubling Time From First') print(median(d_times$doubling_time)) cat('\n') sink(NULL)
    
}
par(pty = "m")
par(mar = c(5.1, 4.1, 0, 2.1))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("top", c(black, blue, red, light_blue), col = c("black", "blue", "red", rainbow(1, start = 0.5, alpha = 0.5)), text.width = c(0.082, 0.1, 0.07, 0.08), pch = c(20, 
    20, 20, NA), lty = c(NA, NA, NA, 1), lwd = 3, horiz = TRUE, bty = "n", title = "Legend", x.intersp = c(0.2, 0.2, 0.2, 0.5))

dev.off() 
