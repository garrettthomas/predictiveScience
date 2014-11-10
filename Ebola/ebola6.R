ebola <- read.csv("~/Dropbox/LEPR01/ebola/data/west.africa.best.csv", stringsAsFactors = FALSE)
ebola[, 1] <- as.Date(ebola[, 1], "%m/%d/%y")

ebola[7,3] <- NA
ebola[c(5,7),5] <- NA

guinea <- lm(log(ebola$Cases_Guinea[93:i]) ~ ebola$Day[93:i])

con_guinea <- confint(guinea, level = 0.9)

liberia <- lm(log(ebola$Cases_Liberia[93:i]) ~ ebola$Day[93:i])

con_liberia <- confint(liberia, level = 0.9)

sierra_leone <- lm(log(ebola$Cases_SierraLeone[93:i]) ~ ebola$Day[93:i])

con_sierra_leone <- confint(sierra_leone, level = 0.9)

# plot(ebola$Day[93:24], log(ebola$Cases_Guinea)[93:24]) abline(guinea)
cat("Guinea\n")
cat(log(2)/log(exp(guinea$coefficients[[2]])))
cat("\n Confidence Interval \n   5%     95% \n")
cat(log(2)/log(exp(con_guinea[c(4,2)])))


cat("\n\nLiberia\n")
cat(log(2)/log(exp(liberia$coefficients[[2]])))
cat("\n Confidence Interval \n   5%     95% \n")
cat(log(2)/log(exp(con_liberia[c(4,2)])))


cat("\n\nSierraLeone\n")
cat(log(2)/log(exp(sierra_leone$coefficients[[2]])))
cat("\nConfidence Interval \n   5%     95% \n")
cat(log(2)/log(exp(con_sierra_leone[c(4,2)])))
cat("\n\n\n")
 
