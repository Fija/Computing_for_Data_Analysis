
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    file<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
    rankhospital <- function(state, outcome, num) {
        if(!(state %in% file[,7])) stop("invalid state")
        if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
        ##idx<-complete.cases(file[,c(11,17,23)])
        ##file<-file[idx,]
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
        cbind(as.vector(hospitallist[num,2]),state)
    }
    states<-as.character(unique(file[,7]))
    states<-states[order(states)]
    ##states<-c("AK","AL","AR","AZ")
    ans<-do.call(rbind.data.frame,lapply(states,rankhospital,outcome,num))
    rownames(ans)<-ans$state
    colnames(ans)[1]<-"hospital"
    ans
    ##lapply(states,rankhospital,outcome,num)
}
