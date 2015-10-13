best <- function(state, outcome) {
    ## Read outcome data, note: this data currently sits in working directory.
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    valid_states<-unique(outcomedata$State)
    valid_outcomes<- c("heart attack", "heart failure", "pneumonia")
    
    if (!state %in% valid_states){
        stop("invalid state")
    }
    if (!outcome %in% valid_outcomes){
        stop("invalid outcome")
    }
    relevant_data<-subset(outcomedata,outcomedata[7]==state)
    ## Return hospital name in that state with lowest 30-day death
    ## rate, columns 11,17,and 23 correspond to heart attack, heart failure,
    ## and pneumonia respectively 
    if (outcome =="heart attack"){
        relevant_column<-relevant_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    }
    else{
        if(outcome == "heart failure"){
            relevant_column<-relevant_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        }
        else{
            relevant_column <-relevant_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        }
    }
    ## Convert relevant death rate column to numeric type and suppress Warnings to ignore NAs
    numeric_death_rate_column<-suppressWarnings(as.numeric(relevant_column))
    min_mortality_rate<-min(numeric_death_rate_column, na.rm=TRUE)
    ## In case of tie on lowest death rates, return first hospital in alphabetic order by name
    best_hospitals<-sort(subset(relevant_data$Hospital.Name,numeric_death_rate_column==min_mortality_rate))
    best_hospitals[1]
}