require(sqldf)

GFT_City_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_city_data.csv", header = TRUE, 
                          check.names = FALSE, stringsAsFactors = FALSE)
GFT_City_Data$Date <- as.Date(GFT_City_Data$Date, "%m/%d/%y")

GFT_State_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_national_data.csv", header = TRUE, 
                           check.names = FALSE, stringsAsFactors = FALSE)
GFT_City_Data$Date <- as.Date(GFT_City_Data$Date, "%m/%d/%y")

GFT_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_regional_data.csv", header = TRUE, 
                              check.names = FALSE, stringsAsFactors = FALSE)
GFT_HHS_Regional_Data$Date <- as.Date(GFT_HHS_Regional_Data$Date, "%m/%d/%y")

CDC_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/ilinet_regional_data.csv", header = TRUE,
                                  check.names = FALSE, stringsAsFactors = FALSE)

CDC_National_Data <- CDC_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/ILINet_national.csv", 
                                                        header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
WHO_National_Data <- CDC_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/WHO_NREVSS_national.csv", 
                                                        header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
CDC_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data CITY/ILINetCITY.csv", header = TRUE,
                                   check.names = FALSE, stringsAsFactors = FALSE)
WHO_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data CITY/WHO_NREVSSCITY.csv", header = TRUE,
                                   check.names = FALSE, stringsAsFactors = FALSE)


date_week_num <- data.frame(Date = as.Date("1999-10-09"), Week = 40, Year = 1999)
for ( i in 2:900 ) {
  date1 <- date_week_num[ i-1, 1]
  date2 <- date1 + 7
  
  if ( format(date2, "%m") == "01" && is.element( format(date2, "%d"), c("04", "05", "06", "07", "08", "09", "10") )) {
    week <- 1
  }
  else week <- date_week_num[ i-1, 2] + 1
  
  date_week_num <- rbind(date_week_num, data.frame(date = date2, week = week, year = format(date1, "%Y")))
}
