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

a <- a[ ,c(2,3,4,6)]
b <- b[ , c(2,8,9)]

distance_peaks <- data.frame(mpz_peak = as.Date(a$peak), gft_peak = as.Date(NA), distance = NA, color = a$color)

for ( i in 1:nrow(a) ) {
  distance <- gdist(lon.1 = as.numeric(a[i, 3]), lat.1 = as.numeric(a[i, 2]), 
                            lon.2 = as.numeric(b[1, 3]), lat.2 = as.numeric(b[1, 2]), units = "km")
  for ( j in 2:nrow(b) ) {
    dist <- gdist(lon.1 = as.numeric(a[i, 3]), lat.1 = as.numeric(b[i, 2]), 
            lon.2 = as.numeric(b[j,3]), lat.2 = as.numeric(b[j,2]), units = "km")
    if ( dist < distance ) {
      distance_peaks[i,2] <- z[j,7]
      distance_peaks[i,3] <- dist
      distance <- dist
    }
  }
}
#distance_peaks$gft_peak <- distance_peaks$gft_peak - 6
plot(y=distance_peaks$mpz_peak, x = distance_peaks$gft_peak, pch=20, col = a$color,
     xlab = "Time of Citizen Peak", ylab = "Time of MPZ Peak", xlim = c(min(distance_peaks$mpz_peak),max(distance_peaks$mpz_peak)),
     ylim = c(min(distance_peaks$mpz_peak),max(distance_peaks$mpz_peak)))
abline(a=0, b=1)
#abline(lm((distance_peaks$mpz_peak - distance_peaks$gft_peak) ~ distance_peaks$distance))
