
require(car)
source("~/predictiveScience/R_Functions/google_flu_trends/google_peak_distance.R")
source("~/predictiveScience/R_Functions/google_flu_trends/google_peak_sh.R")
source("~/predictiveScience/R_Functions/google_flu_trends/google_peak_school.R")

red = FALSE

distance <- google_peak_distance(5)
sh <- google_peak_sh(5)
school <- google_peak_school(5)

mult_regression <- data.frame(name = distance$name, peak_date = distance$peak_date, distance_black = distance$black_distance, distance_red = distance$red_distance, 
    sh = NA, school = as.Date(NA), color = distance$color)

for (i in 1:nrow(mult_regression)) {
    
    sh_index <- which(sh$name == mult_regression$name[i])
    mult_regression$sh[i] <- sh$sh[sh_index]
    
    school_index <- which(school$name == mult_regression$name[i])
    mult_regression$school[i] <- school$school_start[school_index]
}


model_Bdistance <- lm(as.numeric(peak_date) ~ distance_black, data = mult_regression)
model_sh <- lm(as.numeric(peak_date) ~ sh, data = mult_regression)
model_school <- lm(as.numeric(peak_date) ~ school, data = mult_regression)

model_Bdistance_sh <- lm(as.numeric(peak_date) ~ distance_black + sh, data = mult_regression)
model_sh_school <- lm(as.numeric(peak_date) ~ sh + school, data = mult_regression)
model_school_Bdistance <- lm(as.numeric(peak_date) ~ school + distance_black, data = mult_regression)


model <- lm(as.numeric(peak_date) ~ distance_black + as.numeric(school) + sh, data = mult_regression)
Rmodel <- lm(as.numeric(peak_date) ~ distance_red + as.numeric(school) + sh, data = mult_regression[which(mult_regression$color == "red"), ])


cat("\n########################################################################### \n\n")
cat("GFT DATA: HIGHLIGHTS\n\n")
cat("########################################################################### \n\n\n\n")

summary <- summary(model)
anova <- anova(model)
cat("ALL CITIES \n\n")
cat("Signifince Black Distance = ")
cat(anova[[5]][1])
cat("\n\n")
cat("Signifince School Start Date = ")
cat(anova[[5]][2])
cat("\n\n")
cat("Signifince Specific Humidity = ")
cat(anova[[5]][3])
cat("\n\n")
cat("R Squared = ")
cat(summary$r.squared)
cat("\n\n")
cat("Adjusted R Squared = ")
cat(summary$adj.r.squared)
cat("\n\n")

summary_R <- summary(Rmodel)
anova_R <- anova(Rmodel)

cat("\nEAST CITIES \n\n")
cat("Signifince Red Distance = ")
cat(anova_R[[5]][1])
cat("\n\n")
cat("Signifince School Start Date = ")
cat(anova_R[[5]][2])
cat("\n\n")
cat("Signifince Specific Humidity = ")
cat(anova_R[[5]][3])
cat("\n\n")
cat("R Squared = ")
cat(summary_R$r.squared)
cat("\n\n")
cat("Adjusted R Squared = ")
cat(summary_R$adj.r.squared)
cat("\n\n\n")






cat("\n########################################################################### \n\n")
cat("GFT DATA: MULTIPLE REGRESSION ANALYSIS ALL CITIES USING BLACK DISTANCE\n\n")
cat("########################################################################### \n\n\n\n")










cat("Linear Model Summary Peak ~ Black_Distance\n")
print(summary(model_Bdistance))
cat("\n")
cat("Linear Model Summary Peak ~ SH\n")
print(summary(model_sh))
cat("\n")
cat("Linear Model Summary Peak ~ School Start Date\n")
print(summary(model_school))

cat("\n")
cat("Linear Model Summary Peak ~ Black_Distance + SH\n")
print(summary(model_Bdistance_sh))
cat("\n")
cat("Linear Model Summary Peak ~ SH + School Start Date\n")
print(summary(model_sh_school))
cat("\n")
cat("Linear Model Summary Peak ~ School Start Date + Black_Distance\n")
print(summary(model_school_Bdistance))



cat("\n")
cat("Linear Model Summary Peak ~ Black_Distance, SH + School Start Date\n")
print(summary(model))

cat("\n")
cat("Variable Inflation Factors of Peak ~ Black_Distance + SH Linear Model\n")
cat("\n")
print(vif(model_Bdistance_sh))
cat("\n")
cat("Variable Inflation Factors of Peak ~ SH + School Start Date Linear Model\n")
cat("\n")
print(vif(model_sh_school))
cat("\n")
cat("Variable Inflation Factors of Peak ~ School Start Date + Black_Distance Linear Model\n")
cat("\n")
print(vif(model_school_Bdistance))
cat("\n")
cat("Variable Inflation Factors of Peak ~ School Start Date + Black_Distance + SH Linear Model\n")
cat("\n")
print(vif(model))

cat("\nCorrelation Matrix\n")
print(cor(data.frame(peak_date = as.numeric(mult_regression$peak_date), distance_black = mult_regression$distance_black, sh = mult_regression$sh, school = as.numeric(mult_regression$school))))


################################################################################# Analysis of all cities using the red distance


if (red) {
    model_Rdistance <- lm(as.numeric(peak_date) ~ distance_red, data = mult_regression)
    model_sh <- lm(as.numeric(peak_date) ~ sh, data = mult_regression)
    model_school <- lm(as.numeric(peak_date) ~ school, data = mult_regression)
    
    model_Rdistance_sh <- lm(as.numeric(peak_date) ~ distance_red + sh, data = mult_regression)
    model_sh_school <- lm(as.numeric(peak_date) ~ sh + school, data = mult_regression)
    model_school_Rdistance <- lm(as.numeric(peak_date) ~ school + distance_red, data = mult_regression)
    
    
    Rmodel <- lm(as.numeric(peak_date) ~ distance_red + as.numeric(school) + sh, data = mult_regression)
    
    
    print(summary(model_Rdistance))
    print(summary(model_sh))
    print(summary(model_school))
    
    print(summary(model_Rdistance_sh))
    print(summary(model_sh_school))
    print(summary(model_school_Rdistance))
    
    print(summary(Rmodel))
    cat("vif(model_Rdistance_sh)")
    print(vif(model_Rdistance_sh))
    cat("vif(model_sh_school)")
    print(vif(model_sh_school))
    cat("vif(model_school_Rdistance)")
    print(vif(model_school_Rdistance))
    cat("vif(Rmodel)")
    
    print(vif(Rmodel))
    
    cat("\nCorrelation Matrix\n")
    print(cor(data.frame(as.numeric(mult_regression$peak_date), mult_regression$distance_red, mult_regression$sh, as.numeric(mult_regression$school))))
}

################################################################################# Analysis of red cities using the red distance

cat("\n\n\n\n")
cat("########################################################################### \n\n")
cat("GFT DATA: MULTIPLE REGRESSION ANALYSIS ONLY EAST CITIES USING RED DISTANCE \n\n")
cat("########################################################################### \n\n\n\n")
model_Rdistance <- lm(as.numeric(peak_date) ~ distance_red, data = mult_regression[which(mult_regression$color == "red"), ])
model_sh <- lm(as.numeric(peak_date) ~ sh, data = mult_regression[which(mult_regression$color == "red"), ])
model_school <- lm(as.numeric(peak_date) ~ school, data = mult_regression[which(mult_regression$color == "red"), ])

model_Rdistance_sh <- lm(as.numeric(peak_date) ~ distance_red + sh, data = mult_regression[which(mult_regression$color == "red"), ])
model_sh_school <- lm(as.numeric(peak_date) ~ sh + school, data = mult_regression[which(mult_regression$color == "red"), ])
model_school_Rdistance <- lm(as.numeric(peak_date) ~ school + distance_red, data = mult_regression[which(mult_regression$color == "red"), ])




cat("\n")
cat("Linear Model Summary (using only data from red cities) Peak ~ Red_Distance\n")
print(summary(model_Rdistance))
cat("\n")
cat("Linear Model Summary (using only data from red cities) Peak ~ SH\n")
print(summary(model_sh))
cat("\n")
cat("Linear Model Summary (using only data from red cities) Peak ~ School Start Date\n")
print(summary(model_school))

cat("\n")
cat("Linear Model Summary (using only data from red cities) Peak ~ Red_Distance + SH\n")
print(summary(model_Rdistance_sh))
cat("\n")
cat("Linear Model Summary (using only data from red cities) Peak ~ SH + School Start Date\n")
print(summary(model_sh_school))
cat("\n")
cat("Linear Model Summary (using only data from red cities) Peak ~ School Start Date + Red_Distance\n")
print(summary(model_school_Rdistance))
cat("\n")
cat("Linear Model Summary (using only data from red cities) Peak ~ Black_Distance + SH + School Start Date\n")
print(summary(Rmodel))


cat("\n")
cat("(Red only) Variable Inflation Factor Peak ~ Red_Distance + SH")
cat("\n\n")
print(vif(model_Rdistance_sh))
cat("\n\n")
cat("(Red only) Variable Inflation Factor Peak ~ School Start Date + SH")
cat("\n\n")
print(vif(model_sh_school))
cat("\n\n")
cat("(Red only) Variable Inflation Factor Peak ~ Red_Distance + School Start Date")
cat("\n\n")
print(vif(model_school_Rdistance))
cat("\n")
cat("(Red only) Variable Inflation Factor Peak ~ Red_Distance + School Start Date + SH\n")
cat("\n")
print(vif(Rmodel))
cat("\n")
cat("(Red Only) Correlation Matrix\n")
cat("\n")
print(cor(data.frame(peak_date = as.numeric(mult_regression[which(mult_regression$color == "red"), ]$peak_date), distance_red = mult_regression[which(mult_regression$color == 
    "red"), ]$distance_red, sh = mult_regression[which(mult_regression$color == "red"), ]$sh, school = as.numeric(mult_regression[which(mult_regression$color == "red"), 
    ]$school)))) 
