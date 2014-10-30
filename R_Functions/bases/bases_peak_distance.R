bases_peak_distance <- function(year = 10, plot = FALSE) {
    require(mapdata)
    require(ggmap)
    require(Imap)
    require(Hmisc)
    source("~/predictiveScience/R_Functions/find_hhs_region.R")
    source("~/predictiveScience/R_Functions/bases/find_peak_week_bases.R")
    par(mfrow = c(1, 1))
    peak_week_bases <- find_peak_week_bases()
    dmss_years <- c("2000-2001", "2001-2002", "2002-2003", "2003-2004", "2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009", "2009-2010", 
        "2010-2011")
    location_data_frame <- read.csv("~/Dropbox/LEPR/data/AFHSC-top300-zip5.csv")[, c(4, 8, 9)]
    zip <- read.csv("~/Downloads/free-zipcode-database-Primary.csv", stringsAsFactors = FALSE)[, c(1, 4, 6, 7)]
    for (zz in year) {
        # 1:11) {
        a <- data.frame(zip = as.integer(colnames(peak_week_bases)), peak = as.Date(unname(unlist(peak_week_bases[zz, ]))), stringsAsFactors = FALSE)
        if (length(which(is.na(a[, 2]))) != 0) {
            a <- a[-1 * which(is.na(a[, 2])), ]
        }
        a <- cbind(a, data.frame(lat = NA, long = NA, state = as.character(NA), 
                                 color = as.character(NA),stringsAsFactors = FALSE))
        
        remove_rows <- integer(0)
        for (i in 1:nrow(a)) {
            index <- which(location_data_frame$zip5 == a[i, 1])
            index_zip <- which(zip$Zipcode == a[i, 1])
            second_try <- FALSE
            if (length(index) != 0) { 
              lat_lon <- location_data_frame[index, c(3, 2)] 
              state <- map.where('state', y = lat_lon[1], x = lat_lon[2]) 
              
              if ( is.na(state) && length(index_zip) == 0) { 
                remove_rows <- append(remove_rows, i) 
                } 
              
              else if ( is.na(state) && length(index_zip) != 0) { 
                second_try <- TRUE
              }
              
              else { 
                if ( grepl(":", state) ) {
                  state <- substring(state, 1, gregexpr(":", state)[[1]][1] - 1)
                }
                
                state_name <- state.abb[grep(state, state.name, ignore.case = TRUE)] 
                
                if ( state == "district of columbia") {
                  state_name <- "DC"
                }
                length(state_name) <- 1 
                a[i, c(3, 4)] <- lat_lon 
                a[i, 5] <-state_name 
              } 
            }
            
            else if ((length(index) == 0 && length(index_zip) != 0) | second_try ) {
              
                if (is.element(zip[index_zip, 2], c("HI", "AK", "AE", "AP"))) {
                    remove_rows <- append(remove_rows, i)
                } 
            
                else {
                    a[i, c(3, 4, 5)] <- c(zip[index_zip, c(3, 4, 2)])
                }
            }
            
            else if ( length(index_zip) == 0 && length(index) == 0) { 
              remove_rows <- append(remove_rows, i) 
            }
        }
        
        if (length(remove_rows) != 0) {
            a <- a[-1 * remove_rows, ]
        }
        if (length(which(is.na(a[, 3]))) != 0) {
            b <- a[-1 * which(is.na(a[, 3])), ]
        } else b <- a
        c <- as.Date(unname(unlist(b[, 2])), "%Y-%m-%d")
        b$color <- find_hhs_region(b[, 5])
        cor_black <- 0
        cor_red <- 0
        for (i in 1:nrow(b)) {
            q <- b[i, c(3, 4)]
            temp_data_frame <- data.frame(distance = NA, peak_date = as.Date(c))
            for (j in 1:length(c)) {
                temp_data_frame[j, 1] <- gdist(lat.1 = as.numeric(b[j, 3]), lon.1 = as.numeric(b[j, 4]), lat.2 = as.numeric(q[1]), lon.2 = as.numeric(q[2]), 
                  units = "km")
            }
            cor1 <- cor(as.numeric(temp_data_frame[, 2]), temp_data_frame[, 1], method = "pearson")
            cor2 <- cor(as.numeric(temp_data_frame[which(b$color == "red"), 2]), temp_data_frame[which(b$color == "red"), 1], method = "pearson")
            if (cor1 > cor_black) {
                cor_black <- cor1
                distance_data_frame <- temp_data_frame
                zip <- b$zip[i]
                state <- b$state[i]
            }
            if (cor2 > cor_red) {
                cor_red <- cor2
                red_data_frame <- temp_data_frame
                zip_red <- b$zip[i]
                state_red <- b$state[i]
            }
        }
        
        ########### plot which maximizes black correlation
        ans <- cbind(b, black_dist = distance_data_frame[, 1], red_dist = red_data_frame[, 1])
        
        if (plot) {
            all_lm <- lm(formula = distance ~ peak_date, data = distance_data_frame)
            east_lm <- lm(formula = distance ~ peak_date, data = distance_data_frame[which(b$color == "red"), ])
            
            par(pty = "s")
            speed_black <- round(all_lm$coefficients[[2]]/24, 2)
            speed_red <- round(east_lm$coefficients[[2]]/24, 2)
            
            y_label <- paste("Distance from", zip, state, "(km)")
            corr_bb <- rcorr(as.numeric(distance_data_frame[, 2]), distance_data_frame[, 1], type = "pearson")
            corr_br <- rcorr(as.numeric(distance_data_frame[which(b$color == "red"), 2]), distance_data_frame[which(b$color == "red"), 1], type = "pearson")
            title <- paste("MPZ-DATA Peak", dmss_years[zz], "Against Distance from", zip, state, "\nBlack:", round(corr_bb$r[2], 
                2), ", p ~", signif(corr_bb$P[2], 1), ", speed ~", speed_black, "km/hr  Red:", round(corr_br$r[2], 2), ", p ~", signif(corr_br$P[2], 
                1), ", speed ~", speed_red, "km/hr")
            plot(x = as.Date(distance_data_frame[, 2]), y = distance_data_frame[, 1], xlab = "Date of Peak Week", ylab = y_label, col = b$color, 
                pch = 20)
            title(main = title, cex = 0.7)
            abline(all_lm)
            abline(east_lm, col = "red")
            
            legend("topleft", c("All US", "East US"), lwd = c(2, 2), col = c("black", "red"))
            
            
            ########### plot which maximizes red correlation
            
            
            all_lm_red <- lm(formula = distance ~ peak_date, data = red_data_frame)
            east_lm_red <- lm(formula = distance ~ peak_date, data = red_data_frame[which(b$color == "red"), ])
            
            speed_rb <- round(all_lm_red$coefficients[[2]]/24, 2)
            speed_rr <- round(east_lm_red$coefficients[[2]]/24, 2)
            
            par(pty = "s")
            y_label <- paste("Distance from", zip_red, state_red, "(km)")
            corr_rb <- rcorr(as.numeric(red_data_frame[, 2]), red_data_frame[, 1], type = "pearson")
            corr_rr <- rcorr(as.numeric(red_data_frame[which(b$color == "red"), 2]), red_data_frame[which(b$color == "red"), 1], type = "pearson")
            title <- paste("MPZ-DATA Peak", dmss_years[zz], "Against Distance from", zip_red, state_red, "\nBlack:", round(corr_rb$r[2], 
                2), ", p ~", signif(corr_rb$P[2], 1), ", speed ~", speed_rb, "km/hr  Red:", round(corr_rr$r[2], 2), ", p ~", signif(corr_rr$P[2], 
                1), ", speed ~", speed_rr, "km/hr")
            plot(x = as.Date(red_data_frame[, 2]), y = red_data_frame[, 1], xlab = "Date of Peak Week", ylab = y_label, col = b$color, pch = 20)
            
            title(main = title, cex = 0.7)
            abline(all_lm_red)
            abline(east_lm_red, col = "red")
            legend("topleft", c("All US", "East US"), lwd = c(2, 2), col = c("black", "red"))
        }
    }
    return(ans)
} 
