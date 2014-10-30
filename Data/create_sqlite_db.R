require(sqldf)

db <- dbConnect(SQLite(), dbname="NEWTest.sqlite")

date_week_num <- data.frame(week_start_date = as.Date("1989-12-31"), week_end_date = as.Date("1990-01-06"), 
                            Week = 1, Year = 1990)
for ( i in 2:1400 ) {
  date1_end <- date_week_num[ i-1, 2]
  date2_end <- date1_end + 7
  date1_start <- date_week_num[ i-1, 1]
  date2_start <- date1_start + 7
  
  if ( format(date2_end, "%m") == "01" && is.element( format(date2_end, "%d"), c("04", "05", "06", "07", "08", "09", "10") )) {
    week <- 1
    date_week_num <- rbind(date_week_num, data.frame(week_start_date = date2_start,
                                                     week_end_date = date2_end, Week = week, Year = format(date2_end, "%Y")))
  }
  else {
    week <- date_week_num[ i-1, 3] + 1
    
    date_week_num <- rbind(date_week_num, data.frame(week_start_date = date2_start,
                                                     week_end_date = date2_end, Week = week, Year = format(date1_end, "%Y")))
  }
}

sql_statement <- "create table DATE_WEEK(week_start_date integer unique, week_end_date integer unique, Week integer, Year integer, unique(Week, Year) )"

dbGetQuery(db, sql_statement)
dbWriteTable(conn = db, name = "DATE_WEEK", value = date_week_num, append = TRUE, row.names = FALSE)

GFT_City_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_city_data.csv", header = TRUE, 
                          check.names = TRUE, stringsAsFactors = FALSE)

names(GFT_City_Data) <- gsub("\\.", "_", names(GFT_City_Data))

GFT_City_Data$Date <- as.Date(GFT_City_Data$Date, "%m/%d/%y")


#s <- sprintf("create table %s(%s, primary key(%s))", "DF",
#             paste(names(DF), collapse = ", "),
#             names(DF)[1])


sql_statement <- paste("create table GFT_CITY(", paste(names(GFT_City_Data), collapse = " integer, "), 
"integer, primary key (Date))" )

dbGetQuery(db, sql_statement)

dbWriteTable(conn = db, name = "GFT_CITY", value = GFT_City_Data, row.names = FALSE, append=TRUE)


GFT_State_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_national_data.csv", header = TRUE, 
                           check.names = TRUE, stringsAsFactors = FALSE)

GFT_State_Data$Date <- as.Date(GFT_State_Data$Date, "%m/%d/%y")

dbWriteTable(conn = db, name = "GFT_STATE", value = GFT_State_Data, row.names = FALSE)


GFT_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/google_regional_data.csv", header = TRUE, 
                              check.names = TRUE, stringsAsFactors = FALSE)
GFT_HHS_Regional_Data$Date <- as.Date(GFT_HHS_Regional_Data$Date, "%m/%d/%y")

dbWriteTable(conn = db, name = "GFT_HHS", value = GFT_HHS_Regional_Data, row.names = FALSE)

CDC_HHS_Regional_Data <- read.csv(file = "~/GFTvsILINET/DATA/ilinet_regional_data.csv", header = TRUE,
                                  check.names = TRUE, stringsAsFactors = FALSE)

dbWriteTable(conn = db, name = "CDC_HHS", value = CDC_HHS_Regional_Data, row.names = FALSE)

CDC_National_Data <- CDC_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/ILINet_national.csv", 
                                                        header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

dbWriteTable(conn = db, name = "CDC_NATIONAL", value = CDC_National_Data, row.names = FALSE)

WHO_National_Data <- CDC_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2DataNational/WHO_NREVSS_national.csv", 
                                                        header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)

dbWriteTable(conn = db, name = "WHO_NATIONAL", value = WHO_National_Data, row.names = FALSE)

CDC_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data CITY/ILINetCITY.csv", header = TRUE,
                                   check.names = TRUE, stringsAsFactors = FALSE)

dbWriteTable(conn = db, name = "CDC_CENCUS", value = CDC_Cencus_Region_Data, row.names = FALSE)

WHO_Cencus_Region_Data <- read.csv(file = "~/GFTvsILINET/DATA/FluViewPhase2Data CITY/WHO_NREVSSCITY.csv", header = TRUE,
                                   check.names = TRUE, stringsAsFactors = FALSE)


dbWriteTable(conn = db, name = "WHO_CENCUS", value = WHO_Cencus_Region_Data, row.names = FALSE)



dbGetQuery(db, "SELECT DATE_WEEK.Week, DATE_WEEK.Year, GFT_STATE.Alabama FROM DATE_WEEK INNER JOIN GFT_STATE
           ON GFT_STATE.Date=DATE_WEEK.week_start_date")


#dbDisconnect(db)
