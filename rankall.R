rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    valid_outcomes<- c("heart attack", "heart failure", "pneumonia")
    
    if (!outcome %in% valid_outcomes){
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    states<-sort(unique(outcomedata$State))
    state_hospital<-character()
    for (s in states){
        ## restrict data to the input state and extract the relevant column from outcome
        relevant_data<-subset(outcomedata,outcomedata[7]==s)
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
        ## Rank Hospitals by death rate, with hospital name being used to break ties
        hospital_ranking<-order(numeric_death_rate_column,relevant_data$Hospital.Name)
        ## Return hospital name in that state with the given rank
        if (num == "best"){ 
            hospital<-relevant_data$Hospital.Name[hospital_ranking[1]] }
        else {
            n<-sum(!is.na(numeric_death_rate_column))
            if (num == "worst"){ 
                hospital<-relevant_data$Hospital.Name[hospital_ranking[n]] }
            else {
                if (num>n){
                    hospital<-"NA"}
                else {
                    hospital<-relevant_data$Hospital.Name[hospital_ranking[num]] }
            }
        }
        ## output the value of hospital
        state_hospital<-append(state_hospital,hospital)
    }

    ## Return a data frame with the hospital names and the (abbreviated) state name
    data.frame(hospital = state_hospital, state = states)
}

