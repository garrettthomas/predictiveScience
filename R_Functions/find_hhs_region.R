find_hhs_region <- function(states) {
    east <- c("CT", "ME", "MA", "NH", "RI", "VT", "NY", "NJ", "PR", "VI", "DE", "DC", "MD", "PA", "VA", "WV", "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", "IL", 
        "IN", "MI", "MN", "OH", "WI")
    # west <- c('AZ', 'CA', 'NV', 'ID', 'OR', 'WA')
    color <- rep("black", length(states))
    for (i in 1:length(states)) {
        if (is.element(states[i], east)) 
            color[i] <- "red"
        
    }
    return(color)
} 
