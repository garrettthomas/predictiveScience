get_school_start <- function() {
    setwd("~/Dropbox/LEPR/garrett/school_start_data/")
    files <- list.files(pattern = ".csv")
    data <- data.frame(matrix(nrow = 0, ncol = 3))
    for (file in files) {
        start_dates <- read.csv(file)
        start_dates$start_date <- as.Date(start_dates$start_date, "%m/%d/%y")
        median_start_date <- median(start_dates$start_date, na.rm = TRUE)
        location <- gregexpr(pattern = "_", file)
        state <- substring(file, 1, location[[1]][1] - 1)
        if (is.element(state, c("new", "west", "north", "rhode", "south", "district"))) {
            state <- substring(file, 1, location[[1]][2] - 1)
        }
        if (grepl("_", state)) {
            state <- gsub("_", " ", state)
        }
        index <- grep(state, state.name, ignore.case = TRUE)
        if (length(index) > 1) {
            first_length <- nchar(state.name[index[1]])
            second_length <- nchar(state.name[index[2]])
            if (first_length > second_length) 
                index <- index[2] else index <- index[1]
        }
        state_abb <- state.abb[index]
        if (length(state_abb) == 0 && state == "district columbia") {
            state_abb <- "DC"
        }
        data <- rbind(data, data.frame(state = state, start_date = as.Date(median_start_date), state_abb = state_abb))
    }
    
    
    
    return(data)
} 
