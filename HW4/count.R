count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error 
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file 
  ## Extract causes of death 
  ## Return integer containing count of homicides for that cause
  if (is.null(cause) || !(cause %in% c('asphyxiation','blunt force','other',
                                       'shooting','stabbing','unknown'))){
    stop('No such cause!')
  }
  homicides<-readLines('homicides.txt')
  init<-substring(cause,1,1)
  remain<-substring(cause,2)
  length(grep(paste('<dd>[Cc]ause: [',toupper(init),init,']',remain,'</dd>',sep=''),
         homicides))
}