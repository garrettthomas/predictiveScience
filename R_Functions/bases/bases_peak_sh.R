bases_peak_sh <- function( year = 10, plot = FALSE ) {
  require(Hmisc)
source("~/predictiveScience/R_Functions/bases/find_peak_week_bases.R")
source("~/predictiveScience/R_Functions/find_hhs_region.R")
peak_week_bases <- find_peak_week_bases()
dmss_years <- c("2000-2001", "2001-2002", "2002-2003", "2003-2004", "2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009", "2009-2010", 
    "2010-2011")
sh_data <- read.csv("~/Dropbox/LEPR/humidity_data/ILI-small-specific-humidity-by-zip5-2000-01-03-2011-12-26.csv", stringsAsFactors = FALSE)
sh_data$date <- as.Date(sh_data$date, "%m/%d/%y")
colnames(sh_data)[4:103] <- substr(colnames(sh_data)[4:103], start = 2, stop = 10)
zip <- read.csv("~/Downloads/free-zipcode-database-Primary.csv", stringsAsFactors = FALSE)[, c(1, 4)]
 for ( zz in year ) {

a <- data.frame(zip = as.integer(colnames(peak_week_bases)), peak = as.Date(unname(unlist(peak_week_bases[zz, ]))), sh = NA, state = NA, color = NA, 
    stringsAsFactors = FALSE)
if (length(which(is.na(a$peak))) != 0) {
    a <- a[-1 * which(is.na(a$peak)), ]
}

remove_rows <- integer(0)
for (i in 1:nrow(a)) {
    humidity <- sh_data[which(sh_data$date == a$peak[i]) - 1, as.character(a[i, 1])]
    if (is.na(humidity)) {
        remove_rows <- append(remove_rows, i)
    } else {
        a[i, 3] <- humidity
        index <- which(zip$Zipcode == as.integer(a[i, 1]))
        if (zip$State[index] == "HI" | zip$State[index] == "AK" |
              zip$State[index] == "AE" | zip$State[index] == "AP") {
            remove_rows <- append(remove_rows, i)
        } else a$state[i] <- zip$State[index]
    }
}
if (length(remove_rows) != 0) {
    a <- a[-1 * remove_rows, ]
}

a$color <- find_hhs_region(a$state)
if (plot) {
  par(pty = "s")
    plot(x = a$peak, y = a$sh, xlab = "Date of Peak Week", ylab = "Average Specific Humidity Week Prior to Peak", 
         col = a$color, pch = 20, ylim = c(0,0.02))
    black_corr <- rcorr(a$peak, a$sh, type = "pearson")
    red_corr <- rcorr(a$peak[which(a$color == "red")], a$sh[which(a$color == "red")], type = "pearson")
  black_model <- lm(sh ~ as.numeric(peak), a)
  abline(black_model)
  red_model <- lm(sh ~ as.numeric(peak), a[which(a$color == "red"),])
  abline(red_model, col = "red")
  title <- paste("MPZ-DATA Peak", dmss_years[zz], "Against Specific Humidity
Black:", round(black_corr$r[2], 2), ", p ~", signif(black_corr$P[2], 1), 
      "Red:", round(red_corr$r[2], 2), ", p ~", signif(red_corr$P[2],1)) 
  title(main=title)
}
return(a)
}
 } 
