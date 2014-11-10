require(sqldf)

bsve_db <- dbConnect(SQLite(), dbname = "~/predictiveScience/BSVE.sqlite")

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

sql_gft_city_statement <- paste("create table ili_gft_city_percent(Date text unique,", paste(names(GFT_City_Data[, c(2:ncol(GFT_City_Data))]), collapse = " integer, "), "integer, primary key (Date))")

dbGetQuery(bsve_db, sql_gft_city_statement)

dbWriteTable(conn = bsve_db, name = "ili_gft_city_percent", value = GFT_City_Data, row.names = FALSE, append = TRUE)



###### LOAD GOOGLE FLU TRENDS STATE DATA, STANDARDIZE DATE FORMAT, CONVERT VALUES FROM CASES PER 100,000 PHYSICIAN VISITS TO ILI PERCENT (DIVIDE BY 100,000), SET
###### CONTRAINTS ON FIELDS (UNIQUE, PRIMARY KEY, ETC.)



GFT_State_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_national_data.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

GFT_State_Data$Date <- as.character(as.Date(GFT_State_Data$Date, "%m/%d/%y"))

names(GFT_State_Data) <- gsub("\\.", "_", names(GFT_State_Data))

for (i in 2:ncol(GFT_State_Data)) {
    GFT_State_Data[, i] <- GFT_State_Data[, i]/1e+05
}

sql_gft_state_statement <- paste("create table ili_gft_state_percent(Date text unique,", paste(names(GFT_State_Data[, c(2:ncol(GFT_State_Data))]), collapse = " integer, "), "integer, primary key (Date))")

dbGetQuery(bsve_db, sql_gft_state_statement)

dbWriteTable(conn = bsve_db, name = "ili_gft_state_percent", value = GFT_State_Data, row.names = FALSE, append = TRUE)



###### LOAD GOOGLE FLU TRENDS HHS REGION DATA, STANDARDIZE DATE FORMAT, CONVERT VALUES FROM CASES PER 100,000 PHYSICIAN VISITS TO ILI PERCENT (DIVIDE BY 100,000), SET
###### CONTRAINTS ON FIELDS (UNIQUE, PRIMARY KEY, ETC.)



GFT_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_regional_data.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)


GFT_HHS_Regional_Data$Date <- as.character(as.Date(GFT_HHS_Regional_Data$Date, "%m/%d/%y"))

for (i in 2:ncol(GFT_HHS_Regional_Data)) {
    GFT_HHS_Regional_Data[, i] <- GFT_HHS_Regional_Data[, i]/1e+05
}

sql_gft_hhs_statement <- paste("create table ili_gft_hhs_percent(Date text unique,", paste(names(GFT_HHS_Regional_Data[, c(2:ncol(GFT_HHS_Regional_Data))]), collapse = " integer, "), 
    "integer, primary key (Date))")

dbGetQuery(bsve_db, sql_gft_hhs_statement)

dbWriteTable(conn = bsve_db, name = "ili_gft_hhs_percent", value = GFT_HHS_Regional_Data, row.names = FALSE, append = TRUE)



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
    
    dbWriteTable(conn = bsve_db, name = paste("ili_cdc_census_", names(CDC_CENSUS_Regional_Data)[i + 4], sep = ""), value = CDC_CENSUS_LIST[[i]], row.names = FALSE)
    
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
    
    dbWriteTable(conn = bsve_db, name = paste("ili_cdc_hhs_", names(CDC_HHS_Regional_Data)[i + 4], sep = ""), value = CDC_HHS_LIST[[i]], row.names = FALSE)
    
}




####### LOAD CDC (ILINET) NATIONAL DATA, STANDARDIZE DATE FORMAT, DATA IS TWO DIMENSIONAL BECAUSE THERE IS ONLY ONE REGION (NATIONAL)


CDC_National_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/ILINet_national.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

names(CDC_National_Data) <- gsub("\\.", "_", names(CDC_National_Data))

CDC_National_LIST <- list()

for ( i in 4:ncol(CDC_National_Data) ) {
  
  CDC_National_LIST[[i - 3]] <- data.frame(year = CDC_National_Data$YEAR, week = CDC_National_Data$WEEK, value = CDC_National_Data[,i] )
  names(CDC_National_LIST[[i - 3]])[3] <- names(CDC_National_Data)[i + 4]
}

for (i in 1:length(CDC_National_LIST)) {
  
  dbWriteTable(conn = bsve_db, name = paste("ili_cdc_national_", names(CDC_National_Data)[i + 4], sep = ""), value = CDC_National_LIST[[i]], row.names = FALSE)
  
}





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
    
    dbWriteTable(conn = bsve_db, name = paste("ili_who_census_", names(WHO_CENSUS_Regional_Data)[i + 4], sep = ""), value = WHO_CENSUS_LIST[[i]], row.names = FALSE)
    
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
    
    dbWriteTable(conn = bsve_db, name = paste("ili_who_hhs_", names(WHO_HHS_Regional_Data)[i + 4], sep = ""), value = WHO_HHS_LIST[[i]], row.names = FALSE)
    
}



####### LOAD WHO (NREVSS) NATIONAL DATA, STANDARDIZE DATE FORMAT, DATA IS TWO DIMENSIONAL BECAUSE THERE IS ONLY ONE REGION (NATIONAL)


WHO_National_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/ILINet_national.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

names(WHO_National_Data) <- gsub("\\.", "_", names(WHO_National_Data))

WHO_National_LIST <- list()

for ( i in 4:ncol(WHO_National_Data) ) {
  
  WHO_National_LIST[[i - 3]] <- data.frame(year = WHO_National_Data$YEAR, week = WHO_National_Data$WEEK, value = WHO_National_Data[,i] )
  names(WHO_National_LIST[[i - 3]])[3] <- names(WHO_National_Data)[i + 4]
}

for (i in 1:length(WHO_National_LIST)) {
  
  dbWriteTable(conn = bsve_db, name = paste("ili_who_national_", names(WHO_National_Data)[i + 4], sep = ""), value = WHO_National_LIST[[i]], row.names = FALSE)
  
}

ili_mpz <- read.table("~/tests/p-medds/pmedds.core/data/ILI.small.pandemic.TOP100.curves.by.zip5.20000101.20111231.txt", stringsAsFactors = FALSE)

row.names(ili_mpz) <- NULL

ili_mpz$date <- as.Date(ili_mpz$date, "%m/%d/%y")

sql_mpz_statement <- paste("create table ili_mpz_zip_cases(week integer, date text unique,", paste(names(ili_mpz[, 3:ncol(ili_mpz)]), collapse = " integer, "), "integer, primary key (date))")

dbGetQuery(bsve_db, sql_mpz_statement)

dbWriteTable(conn = bsve_db, name = "ili_mpz_zip_cases", value = ili_mpz, append = TRUE)




pH1N1_mpz_age1 <- read.table("~/tests/p-medds/pmedds.core/data/ILI.small.pandemic.TOP100.curves.by.zip5.20090315.20100715.AGE1.txt", stringsAsFactors = FALSE)

names(pH1N1_mpz_age1)[1] <- "week"

row.names(pH1N1_mpz_age1) <- NULL

sql_mpz_age1_statement <- paste("create table pH1N1_mpz_zip_age1(week integer, date text unique,", 
                           paste(names(pH1N1_mpz_age1[, 3:ncol(pH1N1_mpz_age1)]), collapse = " integer, "), "integer, primary key (date))")

dbGetQuery(bsve_db, sql_mpz_age1_statement)

dbWriteTable(conn = bsve_db, name = "pH1N1_mpz_zip_age1", value = pH1N1_mpz_age1, append = TRUE)





pH1N1_mpz_age2 <- read.table("~/tests/p-medds/pmedds.core/data/ILI.small.pandemic.TOP100.curves.by.zip5.20090315.20100715.AGE2.txt", stringsAsFactors = FALSE)

sql_mpz_age2_statement <- paste("create table pH1N1_mpz_zip_age2(week integer, date text unique,", 
                                paste(names(pH1N1_mpz_age2[, 3:ncol(pH1N1_mpz_age2)]), collapse = " integer, "), "integer, primary key (date))")

names(pH1N1_mpz_age2)[1] <- "week"

row.names(pH1N1_mpz_age2) <- NULL

dbGetQuery(bsve_db, sql_mpz_age2_statement)

dbWriteTable(conn = bsve_db, name = "pH1N1_mpz_zip_age2", value = pH1N1_mpz_age2, append = TRUE)




pH1N1_mpz <- read.table("~/tests/p-medds/pmedds.core/data/ILI.small.pandemic.curves.by.zip5.20090320.20100628.txt", stringsAsFactors = FALSE)

row.names(pH1N1_mpz) <- NULL

pH1N1_mpz$date <- as.Date(pH1N1_mpz$date, "%m/%d/%y")

sql_mpz_h1n1_statement <- paste("create table pH1N1_mpz_zip_cases(week integer, date text unique,", 
                                paste(names(pH1N1_mpz[, 3:ncol(pH1N1_mpz)]), collapse = " integer, "), "integer, primary key (date))")

dbGetQuery(bsve_db, sql_mpz_h1n1_statement)

dbWriteTable(conn = bsve_db, name = "pH1N1_mpz_zip_cases", value = pH1N1_mpz, append = TRUE)






sh_ili <- read.table("~/tests/p-medds/pmedds.core/data/ILI.small.specific.humidity.by.zip5.20000103.20111226.txt", stringsAsFactors = FALSE)

sh_ili <- sh_ili[,2:ncol(sh_ili)]

row.names(sh_ili) <- NULL

sh_ili$date <- as.Date(sh_ili$date, "%m/%d/%y")

sql_sh_ili_statement <- paste("create table ili_mpz_zip_sh(week integer, date text unique,", 
                                paste(names(sh_ili[, 3:ncol(sh_ili)]), collapse = " real, "), "real, primary key (date))")

dbGetQuery(bsve_db, sql_sh_ili_statement)

dbWriteTable(bsve_db, "ili_mpz_zip_sh", sh_ili, append = TRUE)



sh_pH1N1 <- read.table("~/tests/p-medds/pmedds.core/data/ILI.small.specific.humidity.by.zip5.20090320.20100628.txt", stringsAsFactors = FALSE)

sh_pH1N1 <- sh_ili[,2:ncol(sh_pH1N1)]

row.names(sh_pH1N1) <- NULL

sh_pH1N1$date <- as.Date(sh_pH1N1$date, "%m/%d/%y")

sql_sh_pH1N1_statement <- paste("create table pH1N1_mpz_zip_sh(week integer, date text unique,", 
                              paste(names(sh_pH1N1[, 3:ncol(sh_pH1N1)]), collapse = " real, "), "real, primary key (date))")

dbGetQuery(bsve_db, sql_sh_pH1N1_statement)

dbWriteTable(bsve_db, "pH1N1_mpz_zip_sh", sh_pH1N1, append = TRUE)




sars_canada <- read.table("~/tests/p-medds/pmedds.core/data/sarscanada2003.txt", header = TRUE)

sql_sars_canada_statement <- "create table sars_canada(day integer unique, cases integer, primary key (day))"

dbGetQuery(bsve_db, sql_sars_canada_statement)

dbWriteTable(bsve_db, "sars_canada", sars_canada, append = TRUE)




sars_china <- read.table("~/tests/p-medds/pmedds.core/data/sarschina2003.txt", header = TRUE)

sql_sars_china_statement <- "create table sars_china(day integer unique, cases integer, primary key (day))"

dbGetQuery(bsve_db, sql_sars_china_statement)

dbWriteTable(bsve_db, "sars_china", sars_china, append = TRUE)





sars_hongkong <- read.table("~/tests/p-medds/pmedds.core/data/sarshongkong2003.txt", header = TRUE)

sql_sars_hongkong_statement <- "create table sars_hongkong(day integer unique, cases integer, primary key (day))"

dbGetQuery(bsve_db, sql_sars_hongkong_statement)

dbWriteTable(bsve_db, "sars_hongkong", sars_hongkong, append = TRUE)




sars_singapore <- read.table("~/tests/p-medds/pmedds.core/data/sarssingapore2003.txt", header = TRUE)

sql_sars_singapore_statement <- "create table sars_singapore(day integer unique, cases integer, primary key (day))"

dbGetQuery(bsve_db, sql_sars_singapore_statement)

dbWriteTable(bsve_db, "sars_singapore", sars_singapore, append = TRUE)




sars_vietnam <- read.table("~/tests/p-medds/pmedds.core/data/sarsvietnam2003.txt", header = TRUE)

sql_sars_vietnam_statement <- "create table sars_vietnam(day integer unique, cases integer, primary key (day))"

dbGetQuery(bsve_db, sql_sars_vietnam_statement)

dbWriteTable(bsve_db, "sars_vietnam", sars_vietnam, append = TRUE)



pop_zip <- read.table("~/tests/p-medds/pmedds.core/data/population.txt", header = TRUE)

dbWriteTable(bsve_db, "pop_zip", pop_zip)




## census_data
load("~/tests/p-medds/pmedds.core/data/region_state_census.RData")
pop_national <- cbind(data.frame(country = "United States"), census_data[1,])
row.names(pop_national) <- NULL
dbWriteTable(bsve_db, "pop_national", pop_national)

pop_state <- cbind(data.frame(state = row.names(census_data)[6:57]), census_data[6:57,])
row.names(pop_state) <- NULL
dbWriteTable(bsve_db, "pop_state", pop_state)

pop_hhs <- cbind(data.frame(state = row.names(census_data)[58:67]), census_data[58:67,])
row.names(pop_hhs) <- NULL
dbWriteTable(bsve_db, "pop_hhs", pop_hhs)

pop_region <- cbind(data.frame(state = row.names(census_data)[2:6]), census_data[2:6,])
row.names(pop_region) <- NULL
dbWriteTable(bsve_db, "pop_region", pop_region)

school_zip <- read.table("~/tests/p-medds/pmedds.core/data/ILI.small.school.closure.by.zip5.20000103.20131230.txt", header = TRUE)
school_zip$date <- as.Date(school_zip$date, "%m/%d/%y")
dbWriteTable(bsve_db, "school_zip", school_zip)



mers <- read.csv("~/Downloads/cases-v1.csv", stringsAsFactors = FALSE)

row.names(mers) <- NULL

mers$date <- as.Date(mers$date, "%m/%d/%y")

sql_mers <- "create table mers(date text unique, week integer, cases integer, death integer, primary key(date))"

dbGetQuery(bsve_db, sql_mers)

dbWriteTable(bsve_db, "mers", mers, append = TRUE)





#region_census <- read.csv("~/tests/p-medds/pmedds.core/data/region_state_census.csv", header = TRUE)



