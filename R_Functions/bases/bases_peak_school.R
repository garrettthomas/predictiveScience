bases_peak_school <- function(year = 10, plot = FALSE) {
    require(mapdata)
    require(ggmap)
    require(Imap)
    require(Hmisc)
    source("~/predictiveScience/R_Functions/bases/find_peak_week_bases.R")
    source("~/predictiveScience/R_Functions/find_hhs_region.R")
    source("~/predictiveScience/R_Functions/get_median_school_start_dates.R")
    data <- get_school_start()
    zip <- read.csv("~/Downloads/free-zipcode-database-Primary.csv", stringsAsFactors = FALSE)[, c(1, 4)]
    data(us.cities)
    par(mfrow = c(1, 1))
    a <- find_peak_week_bases()
    for ( zz in year) {
    b <- data.frame(zip = as.integer(names(a)), peak = as.Date(unlist(unname(a[zz,]))), 
                    state = as.character(NA), school_start = as.Date(NA), 
                    color = as.character(NA), stringsAsFactors = FALSE)
    if ( length(which(is.na(b$peak))) != 0 ) {
      b <- b[-1*which(is.na(b$peak)), ]
    }
    #a[, c(3:5)] <- NA
    #names(a)[c(3:5)] <- c("long", "lat", "state")
    #location_data_frame <- read.csv("~/Dropbox/LEPR/data/AFHSC-top300-zip5.csv")[, c(4, 8, 9)]
    remove_rows <- integer(0)
    for (i in 1:nrow(b)) {
      index <- which(zip$Zipcode == as.integer(b[i, 1]))
      if (zip$State[index] == "HI" | zip$State[index] == "AK" |
            zip$State[index] == "AE" | zip$State[index] == "AP") {
        remove_rows <- append(remove_rows, i)
      } 
      else {
        state_index <- which(data$state_abb == zip$State[index])
        b$school_start[i] <- data$start_date[state_index]
        b$state[i] <- zip$State[index]
        }
            }
    if ( length(remove_rows) != 0 ) {
    b <- b[-1 * remove_rows, ]
    }
    
    
    
    b$color <- find_hhs_region(b$state)
    
    if ( plot ) {
    par(pty = "s")
    plot(x = as.Date(b$peak), y = as.Date(b$school_start), pch = 20, col = b$color, ylab = "School Open Date", xlab = "Peak Date")
    
    all_lm <- lm(formula = as.numeric(school_start) ~ as.numeric(peak), data = b)
    east_lm <- lm(formula = as.numeric(school_start) ~ as.numeric(peak), data = b[which(b$color == "red"), ])
    
    corr_b <- rcorr(as.numeric(b$school_start), as.numeric(b$peak), type = "pearson")
    corr_r <- rcorr(as.numeric(b[which(b$color == "red"), 2]), as.numeric(b[which(b$color == "red"), 4]), type = "pearson")
    
    par(pty = "s")
    abline(all_lm)
    abline(east_lm, col = "red")
    
    title <- paste("Peak Date of 2009 - 2010 Influenza Wave Against School Start Date\nBlack:", round(corr_b$r[2], 2), ", p ~", signif(corr_b$P[2], 
        1), "   Red:", round(corr_r$r[2], 2), ", p ~", signif(corr_r$P[2], 1))
    title(main = title)
    legend("topleft", c("All US", "East US"), lwd = c(2, 2), col = c("black", "red"))
}
}
return(b)
}
