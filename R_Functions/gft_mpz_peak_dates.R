require(mapdata)
require(ggmap)
require(Imap)
source("~/predictiveScience/R_Functions/google_flu_trends/google_peak_distance.R")
source("~/predictiveScience/R_Functions/find_hhs_region.R")
source("~/predictiveScience/R_Functions/bases/bases_peak_distance.R")
require("Hmisc")
data(us.cities)
par(mfrow = c(1, 1))

a <- bases_peak_distance()
b <- google_peak_distance()

# a <- a[ ,c(2,3,4,6)] b <- b[ , c(2,8,9)]
for (min in 12:40) {
    a_not <- find_peak_week_bases(which_years = 9, min = min)
    a <- a[which(is.element(a$zip, names(a_not)[which(!is.na(a_not[1, ]))])), ]
    print(which(is.element(a$zip, names(a_not)[which(!is.na(a_not[1, ]))])))
    print(min)
    distance_peaks <- data.frame(mpz_peak = as.Date(a$peak), gft_peak = as.Date(NA), distance = NA, color = "blue", base = a$zip, city = as.character(NA), stringsAsFactors = FALSE)
    
    for (i in 1:nrow(a)) {
        distance <- gdist(lon.1 = as.numeric(a[i, 4]), lat.1 = as.numeric(a[i, 3]), lon.2 = as.numeric(b[1, 9]), lat.2 = as.numeric(b[1, 8]), units = "km")
        distance_peaks[i, 2] <- b[1, 2]
        distance_peaks[i, 3] <- distance
        distance_peaks[i, 6] <- b[1, 1]
        
        for (j in 2:nrow(b)) {
            dist <- gdist(lon.1 = as.numeric(a[i, 4]), lat.1 = as.numeric(a[i, 3]), lon.2 = as.numeric(b[j, 9]), lat.2 = as.numeric(b[j, 8]), units = "km")
            if (dist < distance) {
                distance_peaks[i, 2] <- b[j, 2]
                distance_peaks[i, 3] <- dist
                distance_peaks[i, 6] <- b[j, 1]
                distance <- dist
            }
        }
    }
    distance_peaks$color <- "blue"
    distance_peaks$color[which(distance_peaks$distance > 30)] <- "green"
    # distance_peaks$gft_peak <- distance_peaks$gft_peak - 6
    plot(y = distance_peaks$mpz_peak, x = distance_peaks$gft_peak, pch = 20, col = distance_peaks$color, xlab = "Time of Citizen Peak", ylab = "Time of MPZ Peak")  #, xlim = c(min(distance_peaks$mpz_peak),max(distance_peaks$mpz_peak)),
    # ylim = c(min(distance_peaks$mpz_peak),max(distance_peaks$mpz_peak)))
    distance_peaks
    text(x = distance_peaks$gft_peak, y = distance_peaks$mpz_peak, distance_peaks$city, cex = 0.6, pos = 4, col = "blue")
    abline(a = 0, b = 1)
    abline(lm(distance_peaks$mpz_peak ~ distance_peaks$gft_peak), col = "purple")
} 
