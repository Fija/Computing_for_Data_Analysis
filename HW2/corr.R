corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        getcorr <- function(id) {
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
            file<-read.csv(desc)
            t<-complete.cases(file)
            t<-t[which(t==TRUE)]
            if(length(t)>threshold) {
                cor(file[,2:3],use="pairwise.complete.obs")[1,2]
            }else {
                -2
            }
        }
        
        ans<-data.frame(sapply(1:332,getcorr))
        ans[ans!=-2]
    
}
