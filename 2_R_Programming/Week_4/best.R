library(plyr)
library(dplyr)

get_name_for_outcome <- function(outcome)
{
    # get column
    if (outcome=="heart attack")
    {
        name <- "Heart.Attack"
    }
    else if (outcome=="heart failure")
    {
        name <- "Heart.Failure"
    }
    else if (outcome=="pneumonia")
    {
        name <- "Pneumonia"
    }    
    else
    {
        stop("invalid outcome")
    }
    name <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", name, sep = "")
    
    return(name)
}

get_data_for_outcome <- function(name, state)
{
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    if(state!="all")
    {
        ## verify state
        states <- which(outcomes$State == state)
        if (length(states) == 0) 
        {
            stop("invalid state")
        }
        
        # Remove other states and sort alphabetically
        outcomes<-filter(outcomes, outcomes$State==state)
    }
    
    ## Check that state and outcome are valid
    outcomes <- outcomes[complete.cases(outcomes[, name]), ]
    outcomes[,name] <- as.numeric(as.character(outcomes[,name]))    
    
    return(outcomes)
}

best <- function(state, outcome) {
    name <- get_name_for_outcome(outcome)
    outcomes <- get_data_for_outcome(name, state)

    minimum <- min(outcomes[ ,name])
    
    outcomes <- outcomes[outcomes[, name] == minimum,]
    outcomes <- outcomes[order(outcomes$Hospital.Name),]

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    return(as.character(outcomes$Hospital.Name[1]))
}

# print(best("TX", "heart attack"))
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# print(best("TX", "heart failure"))
# [1] "FORT DUNCAN MEDICAL CENTER"
# print(best("MD", "heart attack"))
# "JOHNS HOPKINS HOSPITAL, THE"
# print(best("MD", "pneumonia"))
# "GREATER BALTIMORE MEDICAL CENTER"
# best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    name <- get_name_for_outcome(outcome)
    outcomes <- get_data_for_outcome(name, state)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if(num=="best")
    {
        index <- 1
    }
    else if(num=="worst")
    {
        index <- nrow(outcomes)
    }
    else if(num<1 || num>nrow(outcomes))
    {
        return(NA)
    }
    else
    {
        index <- num
    }
    
    ordered <- outcomes[order(outcomes[name], outcomes$Hospital.Name), ]
               
    as.character(ordered$Hospital.Name[index])
}

# print(rankhospital("TX", "heart failure", 4))
# [1] "DETAR HOSPITAL NAVARRO"
# print(rankhospital("MD", "heart attack", "worst"))
# [1] "HARFORD MEMORIAL HOSPITAL"
# print(rankhospital("MN", "heart attack", 5000))
# [1] NA

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    name <- get_name_for_outcome(outcome)
    outcomes <- get_data_for_outcome(name, "all")
    ## For each state, find the hospital of the given rank
    states <- outcomes$State
    states <- unique(states)
    result <- data.frame(hospital=character(), state=character(), stringsAsFactors=FALSE)
    for(s in states)
    {
        result[s, ] <- c(rankhospital(s, outcome, num), s)
    }
    result <- result[order(result$state),]
    return(result)
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}

#print(head(rankall("heart attack", 20), 10))
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# 4
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
#tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
#tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY