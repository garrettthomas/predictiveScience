ebola <- read.csv("~/Dropbox/LEPR01/ebola/data/west.africa.best.csv", stringsAsFactors = FALSE)
ebola[, 1] <- as.Date(ebola[, 1], "%m/%d/%y")

ebola$Cases_Liberia[3] <- NA

guinea <- lm(log(ebola$Cases_Guinea[93:i]) ~ ebola$Day[93:i])
liberia <- lm(log(ebola$Cases_Liberia[93:i]) ~ ebola$Day[93:i])
sierra_leone <- lm(log(ebola$Cases_SierraLeone[93:i]) ~ ebola$Day[93:i])

# plot(ebola$Day[93:24], log(ebola$Cases_Guinea)[93:24]) abline(guinea)
print("Guinea")
print(log(2)/log(exp(guinea$coefficients[[2]])))
print("Liberia")
print(log(2)/log(exp(liberia$coefficients[[2]])))
print("SierraLeone")
print(log(2)/log(exp(sierra_leone$coefficients[[2]])))
cat("\n\n\n")
 
