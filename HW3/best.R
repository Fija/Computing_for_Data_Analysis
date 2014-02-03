best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    file<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
    ##file<-read.csv("outcome-of-care-measures.csv")
    if(!(state %in% file[,7])) stop("invalid state")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    ##idx<-complete.cases(file[,c(11,17,23)])
    ##file<-file[idx,]
    if (outcome == "heart attack") {
        data<-file[file$State==state,]
        besthospital<-data[order(data[,11]),][1,2]
    }
    if (outcome == "heart failure") {
        data<-file[file$State==state,]
        besthospital<-data[order(data[,17]),][1,2]
    }
    if (outcome == "pneumonia") {
        data<-file[file$State==state,]
        besthospital<-data[order(data[,23]),][1,2]
    }
    as.vector(besthospital)

}
