
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    file<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
    if(!(state %in% file[,7])) stop("invalid state")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    idx<-complete.cases(file[,c(11,17,23)])
    file<-file[idx,]
    if (outcome == "heart attack") {
        data<-file[file$State==state,]
        hospitallist<-data[order(data[,11],data[,2]),]
    }
    if (outcome == "heart failure") {
        data<-file[file$State==state,]
        hospitallist<-data[order(data[,17],data[,2]),]
    }
    if (outcome == "pneumonia") {
        data<-file[file$State==state,]
        hospitallist<-data[order(data[,23],data[,2]),]
    }
    if(num=="best"){
        num<-1
    }
    if(num=="worst"){
        num<-nrow(hospitallist)
    }
    ##as.vector(hospitallist[num,2])
    cbind(as.vector(hospitallist[num,2]),state)
}
