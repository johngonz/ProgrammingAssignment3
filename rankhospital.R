rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
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
    ## restrict data to the input state and extract the relevant column from outcome
    relevant_data<-subset(outcomedata,outcomedata[7]==state)
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
    hospital
}