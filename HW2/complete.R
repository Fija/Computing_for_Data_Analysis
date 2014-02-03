complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        getcomplete <- function(id) {
            id<-as.numeric(id)
            if(id>0 && id<10) {
                sid<-paste0("00",id)
            }
            if(id>=10 && id<100) {
                sid<-paste0("0",id)
            }
            if(id>=100) {
                sid<-id
            }

            desc<-paste0(directory,"/",sid,".csv")
            t<-complete.cases(read.csv(desc))
            t<-t[which(t==TRUE)]
            list(id=id,nobs=length(t))
        }
        
        data.frame(t(sapply(id,getcomplete)))

}
