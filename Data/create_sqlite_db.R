require(sqldf)

BSVE_dbConn <- dbConnect(SQLite(), dbname = "BSVE.sqlite")

# date_week_num <- data.frame(week_start_date = as.Date('1989-12-31'), week_end_date = as.Date('1990-01-06'), Week = 1, Year = 1990) for ( i in 2:1400 ) {
# date1_end <- date_week_num[ i-1, 2] date2_end <- date1_end + 7 date1_start <- date_week_num[ i-1, 1] date2_start <- date1_start + 7 if ( format(date2_end, '%m')
# == '01' && is.element( format(date2_end, '%d'), c('04', '05', '06', '07', '08', '09', '10') )) { week <- 1 date_week_num <- rbind(date_week_num,
# data.frame(week_start_date = date2_start, week_end_date = date2_end, Week = week, Year = format(date2_end, '%Y'))) } else { week <- date_week_num[ i-1, 3] + 1
# date_week_num <- rbind(date_week_num, data.frame(week_start_date = date2_start, week_end_date = date2_end, Week = week, Year = format(date1_end, '%Y'))) } }
# date_week_num$week_start_date <- as.character(date_week_num$week_start_date) date_week_num$week_end_date <- as.character(date_week_num$week_end_date)
# sql_statement <- 'create table DATE_WEEK(week_start_date text unique, week_end_date text unique, Week integer, Year integer, unique(Week, Year) )'
# dbGetQuery(db, sql_statement) dbWriteTable(conn = db, name = 'DATE_WEEK', value = date_week_num, append = TRUE, row.names = FALSE)



###### LOAD GOOGLE FLU TRENDS CITY DATA, STANDARDIZE DATE FORMAT, CONVERT VALUES FROM CASES PER 100,000 PHYSICIAN VISITS TO ILI PERCENT (DIVIDE BY 100,000), SET
###### CONTRAINTS ON FIELDS (UNIQUE, PRIMARY KEY, ETC.)


GFT_City_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_city_data.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

names(GFT_City_Data) <- gsub("\\.", "_", names(GFT_City_Data))

GFT_City_Data$Date <- as.character(as.Date(GFT_City_Data$Date, "%m/%d/%y"))

for (i in 2:ncol(GFT_City_Data)) {
    GFT_City_Data[, i] <- GFT_City_Data[, i]/1e+05
    
    
}

sql_gft_city_statement <- paste("create table GFT_CITY(Date text unique,", paste(names(GFT_City_Data[, c(2:ncol(GFT_City_Data))]), collapse = " integer, "), "integer, primary key (Date))")

dbGetQuery(BSVE_dbConn, sql_gft_city_statement)

dbWriteTable(conn = BSVE_dbConn, name = "GFT_CITY", value = GFT_City_Data, row.names = FALSE, append = TRUE)



###### LOAD GOOGLE FLU TRENDS STATE DATA, STANDARDIZE DATE FORMAT, CONVERT VALUES FROM CASES PER 100,000 PHYSICIAN VISITS TO ILI PERCENT (DIVIDE BY 100,000), SET
###### CONTRAINTS ON FIELDS (UNIQUE, PRIMARY KEY, ETC.)



GFT_State_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_national_data.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

GFT_State_Data$Date <- as.character(as.Date(GFT_State_Data$Date, "%m/%d/%y"))

names(GFT_State_Data) <- gsub("\\.", "_", names(GFT_State_Data))

for (i in 2:ncol(GFT_State_Data)) {
    GFT_State_Data[, i] <- GFT_State_Data[, i]/1e+05
}

sql_gft_state_statement <- paste("create table GFT_STATE(Date text unique,", paste(names(GFT_State_Data[, c(2:ncol(GFT_State_Data))]), collapse = " integer, "), "integer, primary key (Date))")

dbGetQuery(BSVE_dbConn, sql_gft_state_statement)

dbWriteTable(conn = BSVE_dbConn, name = "GFT_STATE", value = GFT_State_Data, row.names = FALSE, append = TRUE)



###### LOAD GOOGLE FLU TRENDS HHS REGION DATA, STANDARDIZE DATE FORMAT, CONVERT VALUES FROM CASES PER 100,000 PHYSICIAN VISITS TO ILI PERCENT (DIVIDE BY 100,000), SET
###### CONTRAINTS ON FIELDS (UNIQUE, PRIMARY KEY, ETC.)



GFT_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_regional_data.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)


GFT_HHS_Regional_Data$Date <- as.character(as.Date(GFT_HHS_Regional_Data$Date, "%m/%d/%y"))

for (i in 2:ncol(GFT_HHS_Regional_Data)) {
    GFT_HHS_Regional_Data[, i] <- GFT_HHS_Regional_Data[, i]/1e+05
}

sql_gft_hhs_statement <- paste("create table GFT_HHS(Date text unique,", paste(names(GFT_HHS_Regional_Data[, c(2:ncol(GFT_HHS_Regional_Data))]), collapse = " integer, "), 
    "integer, primary key (Date))")

dbGetQuery(BSVE_dbConn, sql_gft_hhs_statement)

dbWriteTable(conn = BSVE_dbConn, name = "GFT_HHS", value = GFT_HHS_Regional_Data, row.names = FALSE, append = TRUE)



####### LOAD CDC (ILINET) CENSUS REGION DATA, STANDARDIZE DATE FORMAT, DATA IS INHARIENTLY THREE DIMENSIONAL (DATE by REGION by VARIABLE. VARIABLES = ILITOTAL,TOTAL
####### PATIENTS,NUM. OF PROVIDERS,% WEIGHTED ILI,%UNWEIGHTED ILI, AGE 0-4,AGE 5-24,AGE 25-64,AGE 25-49,AGE 50-64,AGE 65 AND REGIONS ARE CENSUS REGIONS = NEW ENGLAND,
####### MID-ATLANTIC, EAST NORTH CENTRAL, WEST NORTH CENTRAL, SOUTH ATLANTIC, EAST SOUTH CENTRAL, WEST SOUTH CENTRAL, MOUTAIN, PACIFIC), CONVERT SINGLE DATA FRAME TO
####### ELEVEN DATA FRAMES TO SIMULATE THREE DIMENSIONAL DATA (I'M NOT 100 PERCENT SURE THIS IS THE BEST WAY TO GO ABOUT THIS)



CDC_CENSUS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data CITY/ILINetCITY.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

names(CDC_CENSUS_Regional_Data) <- gsub("\\.", "_", names(CDC_CENSUS_Regional_Data))

CDC_CENSUS_LIST <- list()

for (i in 5:ncol(CDC_CENSUS_Regional_Data)) {
    CDC_CENSUS_LIST[[i - 4]] <- data.frame(Year = CDC_CENSUS_Regional_Data$YEAR[seq(1, (nrow(CDC_CENSUS_Regional_Data) - 8), by = 9)], WEEK = CDC_CENSUS_Regional_Data$WEEK[seq(1, 
        (nrow(CDC_CENSUS_Regional_Data) - 8), by = 9)], NEW_ENGLAND = CDC_CENSUS_Regional_Data[seq(1, (nrow(CDC_CENSUS_Regional_Data) - 8), by = 9), i], MID_ATLANTIC = CDC_CENSUS_Regional_Data[seq(2, 
        (nrow(CDC_CENSUS_Regional_Data) - 7), by = 9), i], EAST_NORTH_CENTRAL = CDC_CENSUS_Regional_Data[seq(3, (nrow(CDC_CENSUS_Regional_Data) - 6), by = 9), i], 
        WEST_NORTH_CENTRAL = CDC_CENSUS_Regional_Data[seq(4, (nrow(CDC_CENSUS_Regional_Data) - 5), by = 9), i], SOUTH_ATLANTIC = CDC_CENSUS_Regional_Data[seq(5, (nrow(CDC_CENSUS_Regional_Data) - 
            4), by = 9), i], EAST_SOUTH_CENTRAL = CDC_CENSUS_Regional_Data[seq(6, (nrow(CDC_CENSUS_Regional_Data) - 3), by = 9), i], WEST_SOUTH_CENTRAL = CDC_CENSUS_Regional_Data[seq(7, 
            (nrow(CDC_CENSUS_Regional_Data) - 2), by = 9), i], MOUTAIN = CDC_CENSUS_Regional_Data[seq(8, (nrow(CDC_CENSUS_Regional_Data) - 1), by = 9), i], PACIFIC = CDC_CENSUS_Regional_Data[seq(9, 
            nrow(CDC_CENSUS_Regional_Data), by = 9), i])
}

for (i in 1:length(CDC_CENSUS_LIST)) {
    
    dbWriteTable(conn = BSVE_dbConn, name = paste("CDC_CENSUS_", names(CDC_CENSUS_Regional_Data)[i + 4], sep = ""), value = CDC_CENSUS_LIST[[i]], row.names = FALSE)
    
}




####### LOAD CDC (ILINET) HHS REGION DATA, STANDARDIZE DATE FORMAT, DATA IS INHARIENTLY THREE DIMENSIONAL (DATE by REGION by VARIABLE. VARIABLES = ILITOTAL,TOTAL
####### PATIENTS,NUM. OF PROVIDERS,% WEIGHTED ILI,%UNWEIGHTED ILI, AGE 0-4,AGE 5-24,AGE 25-64,AGE 25-49,AGE 50-64,AGE 65), CONVERT SINGLE DATA FRAME TO ELEVEN DATA
####### FRAMES TO SIMULATE THREE DIMENSIONAL DATA (I'M NOT 100 PERCENT SURE THIS IS THE BEST WAY TO GO ABOUT THIS)



CDC_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data Regional/ILINetRegional.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

names(CDC_HHS_Regional_Data) <- gsub("\\.", "_", names(CDC_HHS_Regional_Data))

CDC_HHS_LIST <- list()

for (i in 5:ncol(CDC_HHS_Regional_Data)) {
    CDC_HHS_LIST[[i - 4]] <- data.frame(Year = CDC_HHS_Regional_Data$YEAR[seq(1, (nrow(CDC_HHS_Regional_Data) - 9), by = 10)], WEEK = CDC_HHS_Regional_Data$WEEK[seq(1, 
        (nrow(CDC_HHS_Regional_Data) - 9), by = 10)], HHS_REGION_1 = CDC_HHS_Regional_Data[seq(1, (nrow(CDC_HHS_Regional_Data) - 9), by = 10), i], HHS_REGION_2 = CDC_HHS_Regional_Data[seq(2, 
        (nrow(CDC_HHS_Regional_Data) - 8), by = 10), i], HHS_REGION_3 = CDC_HHS_Regional_Data[seq(3, (nrow(CDC_HHS_Regional_Data) - 7), by = 10), i], HHS_REGION_4 = CDC_HHS_Regional_Data[seq(4, 
        (nrow(CDC_HHS_Regional_Data) - 6), by = 10), i], HHS_REGION_5 = CDC_HHS_Regional_Data[seq(5, (nrow(CDC_HHS_Regional_Data) - 5), by = 10), i], HHS_REGION_6 = CDC_HHS_Regional_Data[seq(6, 
        (nrow(CDC_HHS_Regional_Data) - 4), by = 10), i], HHS_REGION_7 = CDC_HHS_Regional_Data[seq(7, (nrow(CDC_HHS_Regional_Data) - 3), by = 10), i], HHS_REGION_8 = CDC_HHS_Regional_Data[seq(8, 
        (nrow(CDC_HHS_Regional_Data) - 2), by = 10), i], HHS_REGION_9 = CDC_HHS_Regional_Data[seq(9, (nrow(CDC_HHS_Regional_Data) - 1), by = 10), i], HHS_REGION_10 = CDC_HHS_Regional_Data[seq(10, 
        (nrow(CDC_HHS_Regional_Data)), by = 10), i])
}

for (i in 1:length(CDC_HHS_LIST)) {
    
    dbWriteTable(conn = BSVE_dbConn, name = paste("CDC_HHS_", names(CDC_HHS_Regional_Data)[i + 4], sep = ""), value = CDC_HHS_LIST[[i]], row.names = FALSE)
    
}




####### LOAD CDC (ILINET) NATIONAL DATA, STANDARDIZE DATE FORMAT, DATA IS TWO DIMENSIONAL BECAUSE THERE IS ONLY ONE REGION (NATIONAL)


CDC_National_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/ILINet_national.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

CDC_National_Data <- CDC_National_Data[, c(1, 3:ncol(CDC_National_Data))]

names(CDC_National_Data) <- gsub("\\.", "_", names(CDC_National_Data))

dbWriteTable(conn = BSVE_dbConn, name = "CDC_NATIONAL", value = CDC_National_Data, row.names = FALSE)




####### LOAD WHO (NREVSS) CENSUS REGION DATA, STANDARDIZE DATE FORMAT, DATA IS INHARIENTLY THREE DIMENSIONAL (DATE by REGION by VARIABLE. VARIABLES = TOTAL
####### SPECIMENS,PERCENT POSITIVE,A (H1),A (Unable to Subtype), A (H3),2009 H1N1,A (Subtyping not Performed),B,H3N2v AND REGIONS ARE CENSUS REGIONS = NEW ENGLAND,
####### MID-ATLANTIC, EAST NORTH CENTRAL, WEST NORTH CENTRAL, SOUTH ATLANTIC, EAST SOUTH CENTRAL, WEST SOUTH CENTRAL, MOUTAIN, PACIFIC), CONVERT SINGLE DATA FRAME TO
####### NINE DATA FRAMES TO SIMULATE THREE DIMENSIONAL DATA (I'M NOT 100 PERCENT SURE THIS IS THE BEST WAY TO GO ABOUT THIS)



WHO_CENSUS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data CITY/WHO_NREVSSCITY.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

names(WHO_CENSUS_Regional_Data) <- gsub("\\.", "_", names(WHO_CENSUS_Regional_Data))

WHO_CENSUS_LIST <- list()

for (i in 5:ncol(WHO_CENSUS_Regional_Data)) {
    WHO_CENSUS_LIST[[i - 4]] <- data.frame(Year = WHO_CENSUS_Regional_Data$YEAR[seq(1, (nrow(WHO_CENSUS_Regional_Data) - 8), by = 9)], WEEK = WHO_CENSUS_Regional_Data$WEEK[seq(1, 
        (nrow(WHO_CENSUS_Regional_Data) - 8), by = 9)], NEW_ENGLAND = WHO_CENSUS_Regional_Data[seq(1, (nrow(WHO_CENSUS_Regional_Data) - 8), by = 9), i], MID_ATLANTIC = WHO_CENSUS_Regional_Data[seq(2, 
        (nrow(WHO_CENSUS_Regional_Data) - 7), by = 9), i], EAST_NORTH_CENTRAL = WHO_CENSUS_Regional_Data[seq(3, (nrow(WHO_CENSUS_Regional_Data) - 6), by = 9), i], 
        WEST_NORTH_CENTRAL = WHO_CENSUS_Regional_Data[seq(4, (nrow(WHO_CENSUS_Regional_Data) - 5), by = 9), i], SOUTH_ATLANTIC = WHO_CENSUS_Regional_Data[seq(5, (nrow(WHO_CENSUS_Regional_Data) - 
            4), by = 9), i], EAST_SOUTH_CENTRAL = WHO_CENSUS_Regional_Data[seq(6, (nrow(WHO_CENSUS_Regional_Data) - 3), by = 9), i], WEST_SOUTH_CENTRAL = WHO_CENSUS_Regional_Data[seq(7, 
            (nrow(WHO_CENSUS_Regional_Data) - 2), by = 9), i], MOUTAIN = WHO_CENSUS_Regional_Data[seq(8, (nrow(WHO_CENSUS_Regional_Data) - 1), by = 9), i], PACIFIC = WHO_CENSUS_Regional_Data[seq(9, 
            nrow(WHO_CENSUS_Regional_Data), by = 9), i])
}

for (i in 1:length(WHO_CENSUS_LIST)) {
    
    dbWriteTable(conn = BSVE_dbConn, name = paste("WHO_CENSUS_", names(WHO_CENSUS_Regional_Data)[i + 4], sep = ""), value = WHO_CENSUS_LIST[[i]], row.names = FALSE)
    
}




####### LOAD WHO (NREVSS) HHS REGION DATA, STANDARDIZE DATE FORMAT, DATA IS INHARIENTLY THREE DIMENSIONAL (DATE by REGION by VARIABLE. VARIABLES = TOTAL
####### SPECIMENS,PERCENT POSITIVE,A (H1),A (Unable to Subtype), A (H3),2009 H1N1,A (Subtyping not Performed),B,H3N2v), CONVERT SINGLE DATA FRAME TO NINE DATA FRAMES TO
####### SIMULATE THREE DIMENSIONAL DATA (I'M NOT 100 PERCENT SURE THIS IS THE BEST WAY TO GO ABOUT THIS)



WHO_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data Regional/WHO_NREVSS_REGIONAL.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

names(WHO_HHS_Regional_Data) <- gsub("\\.", "_", names(WHO_HHS_Regional_Data))
WHO_HHS_LIST <- list()

for (i in 5:ncol(WHO_HHS_Regional_Data)) {
    WHO_HHS_LIST[[i - 4]] <- data.frame(Year = WHO_HHS_Regional_Data$YEAR[seq(1, (nrow(WHO_HHS_Regional_Data) - 9), by = 10)], WEEK = WHO_HHS_Regional_Data$WEEK[seq(1, 
        (nrow(WHO_HHS_Regional_Data) - 9), by = 10)], HHS_REGION_1 = WHO_HHS_Regional_Data[seq(1, (nrow(WHO_HHS_Regional_Data) - 9), by = 10), i], HHS_REGION_2 = WHO_HHS_Regional_Data[seq(2, 
        (nrow(WHO_HHS_Regional_Data) - 8), by = 10), i], HHS_REGION_3 = WHO_HHS_Regional_Data[seq(3, (nrow(WHO_HHS_Regional_Data) - 7), by = 10), i], HHS_REGION_4 = WHO_HHS_Regional_Data[seq(4, 
        (nrow(WHO_HHS_Regional_Data) - 6), by = 10), i], HHS_REGION_5 = WHO_HHS_Regional_Data[seq(5, (nrow(WHO_HHS_Regional_Data) - 5), by = 10), i], HHS_REGION_6 = WHO_HHS_Regional_Data[seq(6, 
        (nrow(WHO_HHS_Regional_Data) - 4), by = 10), i], HHS_REGION_7 = WHO_HHS_Regional_Data[seq(7, (nrow(WHO_HHS_Regional_Data) - 3), by = 10), i], HHS_REGION_8 = WHO_HHS_Regional_Data[seq(8, 
        (nrow(WHO_HHS_Regional_Data) - 2), by = 10), i], HHS_REGION_9 = WHO_HHS_Regional_Data[seq(9, (nrow(WHO_HHS_Regional_Data) - 1), by = 10), i], HHS_REGION_10 = WHO_HHS_Regional_Data[seq(10, 
        (nrow(WHO_HHS_Regional_Data)), by = 10), i])
}

for (i in 1:length(WHO_HHS_LIST)) {
    
    dbWriteTable(conn = BSVE_dbConn, name = paste("WHO_HHS_", names(WHO_HHS_Regional_Data)[i + 4], sep = ""), value = WHO_HHS_LIST[[i]], row.names = FALSE)
    
}



####### LOAD WHO (NREVSS) NATIONAL DATA, STANDARDIZE DATE FORMAT, DATA IS TWO DIMENSIONAL BECAUSE THERE IS ONLY ONE REGION (NATIONAL)


WHO_National_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/ILINet_national.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

WHO_National_Data <- WHO_National_Data[, c(1, 3:ncol(WHO_National_Data))]

names(WHO_National_Data) <- gsub("\\.", "_", names(WHO_National_Data))

dbWriteTable(conn = BSVE_dbConn, name = "WHO_NATIONAL", value = WHO_National_Data, row.names = FALSE)


dbDisconnect(BSVE_dbConn) 
