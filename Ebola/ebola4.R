ebola <- read.csv("~/Dropbox/LEPR01/ebola/data/west.africa.best.csv", stringsAsFactors = FALSE)
ebola[, 1] <- as.Date(ebola[, 1], "%m/%d/%y")

names <- c("Guinea", "Liberia", "Sierra Leone")
png("~/Desktop/plos_running_mean_figure.png", width = 800)
# par('mfcol' = c(1,4))

par(mar = c(5.1, 4.5, 2.1, 1.1))
# txt_file <- '~/Desktop/plos.txt' sink(txt_file) sink(NULL)
m <- matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, ncol = 3, byrow = TRUE)

layout(mat = m, heights = c(0.4, 0.1))



for (j in 3:5) {
    # jpeg(paste('~/Desktop/garrett',j,'.jpeg',sep=''), height = 600, width = 1200, quality = 100) par('mfcol' = c(1,2)) par('mar' = c(5.1,5.1,3.1,5.1))
    doubling_times <- data.frame(date = character(0), doubling_time = integer(0))
    average_doubling_times <- data.frame(date = character(0), doubling_time = integer(0))
    for (i in (nrow(ebola) - 1):1) {
        if (!is.na(ebola[i, j])) {
            index <- which(!is.na(ebola[, j]) & ebola[i, 2] - ebola[, 2] > 0)
            if (length(index) != 0) {
                index <- min(index)
                d_time <- (ebola[i, 2] - ebola[index, 2]) * log(2)/log(ebola[i, j]/ebola[index, j])
                if (d_time > 0 & !is.na(d_time)) 
                  doubling_times <- rbind(doubling_times, data.frame(date = ebola[i, 1], doubling_time = d_time, stringsAsFactors = FALSE))
            }
            if (is.finite(d_time) & d_time > 0 & !is.na(d_time)) {
                if (nrow(average_doubling_times) == 0) {
                  average_doubling_times <- rbind(average_doubling_times, data.frame(date = ebola[i, 1], doubling_time = d_time, stringsAsFactors = FALSE))
                } else {
                  average <- (average_doubling_times[nrow(average_doubling_times), 2] * as.numeric(average_doubling_times[nrow(average_doubling_times), 1] - ebola[max(which(!is.na(ebola[, 
                    j]))), 1]) + d_time * as.numeric(ebola[i, 1] - average_doubling_times[nrow(average_doubling_times), 1]))/as.numeric(ebola[i, 1] - ebola[max(which(!is.na(ebola[, 
                    j]))), 1])
                  average_doubling_times <- rbind(average_doubling_times, data.frame(date = ebola[i, 1], doubling_time = average, stringsAsFactors = FALSE))
                }
                
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
    # plot(x=ebola[which(is.element(ebola[,1],doubling_times$date)),1], y=ebola[which(is.element(ebola[,1],doubling_times$date)),j],type='l', ylab = 'Cumulative
    # Total', xlab = 'Date', main=names[j-2])
    lowerq = quantile(doubling_times[which(is.finite(doubling_times$doubling_time)), 2])[2]
    upperq = quantile(doubling_times[which(is.finite(doubling_times$doubling_time)), 2])[3]
    iqr = upperq - lowerq
    col <- rep("black", nrow(doubling_times))
    col[which(!is.finite(doubling_times$doubling_time))] <- "red"
    doubling_times$doubling_time[which(!is.finite(doubling_times$doubling_time))] <- -1
    col[which(doubling_times[, 2] > (iqr * 3) + upperq)] <- "blue"
    
    # plot(doubling_times[,1],doubling_times[,2], col = col, 'pch' = 20, ylab = 'Doubling Time (days)', xlab = 'Date', main=names[j-2]) #mtext(expression('Doubling
    # Time (days) Δt = '*'t'[i-1]*'- t'[i]),side=2,line=3) lm <- lm(doubling_time ~ as.numeric(date), doubling_times[which(col == 'black'),]) abline(lm) par('new' =
    # TRUE) # plot(d_times[,1],d_times[,2], ylim=c(0,max(d_times[,2])),'pch' = 20, # xlim=c(min(doubling_times[,1]),max(doubling_times[,1])),'ann' = FALSE, #
    # col=rainbow(1,start=.5,alpha=.3),axes = FALSE) qq = 1 for ( q in which(col=='black') ) {
    # points(doubling_times[q,1],mean(doubling_times[which(col=='black')[1:qq],2]), 'pch' = 20, col=rainbow(1,start=.5,alpha=.3))#,axes = FALSE) qq <- qq + 1 } qq = 1
    # for ( q in which(col !='red') ) { points(doubling_times[q,1],mean(doubling_times[which(col !='red')[1:qq],2]), 'pch' = 20,
    # col=rainbow(1,start=.7,alpha=.3))#,axes = FALSE) qq <- qq + 1 } #axis(4) #mtext(expression('Doubling Time (days) Δt = '*'t'[0]*'to t'[i]), side = 4, line = 3)
    # black <- expression('Δt = '*'t'[i-1]*'- t'[i]) blue <- expression('Δt = '*'t'[i-1]*'- t'[i]*' Outliers') red <- expression('Δt = '*'t'[i-1]*'- t'[i]*' INF')
    # light_blue <- expression('Δt = '*'t'[0]*'to t'[i]) legend('topleft', c(black, blue, red, light_blue), col =
    # c('black','blue','red',rainbow(1,start=.5,alpha=.3)), pch=20)
    
    
    # a <- predict(lm,date=as.numeric(doubling_times$date[which(col == 'black'),]), interval = 'confidence') lines(doubling_times[which(col == 'black'),1],a[,2])
    # lines(doubling_times[which(col == 'black'),1],a[,3])
    
    # sink(txt_file,append = TRUE) print(names[j-2],font=2) print('Mean Doubling Time From Previous') print(mean(doubling_times$doubling_time)) print('Median Doubling
    # Time From Previous') print(median(doubling_times$doubling_time)) print('Mean Doubling Time From Previous Excluding Outliers')
    # print(mean(doubling_times$doubling_time[which(col=='black')])) print('Median Doubling Time From Previous Excluding Outliers')
    # print(median(doubling_times$doubling_time[which(col=='black')])) print('Mean Doubling Time From First') print(mean(d_times$doubling_time)) print('Median
    # Doubling Time From First') print(median(d_times$doubling_time)) cat('\n') sink(NULL)
    par(pty = "s")
    plot(doubling_times[which(col != "blue"), 1], doubling_times[which(col != "blue"), 2], col = col[which(col != "blue")], pch = 20, ylab = "Doubling Time (days)", 
        xlab = "Date", main = names[j - 2])
    black <- which(col == "black")
    running <- data.frame(date = doubling_times[black[1], 1], doubling_time = doubling_times[black[1], 2])
    for (q in 2:(length(black) - 1)) {
        running <- rbind(running, data.frame(date = doubling_times[black[q], 1], doubling_time = mean(doubling_times[black[q - 1:q + 1], 2])))
    }
    lines(running[, 1], running[, 2], col = rainbow(1, start = 0.7, alpha = 0.3), lwd = 3)
}
par(pty = "m")
par(mar = c(5.1, 4.1, 0, 2.1))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
text <- c("Doubling Time", expression("No New Cases From " * "t"[i - 1] * "to t"[i]), "Running Mean")
legend(x = "top", inset = 0, legend = text, col = c("black", "red", rainbow(1, start = 0.7, alpha = 0.3)), lwd = 3, cex = 1, horiz = TRUE, pch = c(20, 20, NA), lty = c(NA, 
    NA, 1), x.intersp = c(0.2, 0.2, 0.5), text.width = c(0.11, 0.18, 0.1), bty = "n", title = "Legend")
dev.off() 
