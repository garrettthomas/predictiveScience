find_peak_week_bases <- function(which_years = c(0:10), min = 11) {
    bases <- read.csv("~/Dropbox/LEPR/data/ILI-small-all-datescurves-by-zip5-v2.0.csv", check.names = FALSE)
    data <- data.frame(matrix(nrow = length(which_years), ncol = 100))
    
    colnames(data) <- colnames(bases[, 4:103])
    
    for (j in which_years) {
        a = 37 + 52 * j
        b = 80 + 52 * j
        
        # Get highest range of that year of all regions
        
        for (k in 4:103) {
            if (length(which(bases[a:b, k] > min)) != 0) {
                index <- which.max(bases[a:b, k])
                if (length(index) == 0) 
                  data[which(which_years == j), k - 3] <- NA else data[which(which_years == j), k - 3] <- as.character(bases$date[a - 1 + index])
            }
        }
    }
    
    
    return(data)
} 
