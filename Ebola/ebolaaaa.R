ebola <- read.csv("~/Dropbox/LEPR01/ebola/data/west.africa.best.csv")
ebola[, 1] <- as.Date(ebola[, 1], "%m/%d/%y")

for (country in 3:5) {
    plot(ebola[, 2], log(ebola[, country]))
    model <- lm(log(ebola[, country]) ~ ebola[, 2])
    abline(model)
    plot(model$residuals)
    for (count in 1:(length(model$fitted.values) - 1)) {
        double <- (ebola[as.integer(names(model$fitted.values[count])), 2] - ebola[as.integer(names(model$fitted.values[count + 1])), 2]) * log(2)/(model$fitted.values[[count]] - 
            model$fitted.values[[count + 1]])
        print(double)
        
    }
    
}
# (207-205)*log(2) / (8.886973 - 8.810857) (81-79)*log(2) / (1.3515 - 1.23735) 
