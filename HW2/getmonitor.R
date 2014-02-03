getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
    id<-as.numeric(id)
    if(id>0 && id<10) {
        id<-paste0("00",id)
    }
    if(id>=10 && id<100) {
        id<-paste0("0",id)
    }

    desc<-paste0(directory,"/",id,".csv")
    a<-read.csv(desc)
    if(summarize == TRUE) {
        print(summary(a))
    }
    a
    
        
}
