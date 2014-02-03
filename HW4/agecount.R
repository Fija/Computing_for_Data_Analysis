agecount <- function(age = NULL) { 
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is ## given
  ## Return integer containing count of homicides for that age
  if(is.null(age)) stop('Please input age.')
  homicides<-readLines('homicides.txt')
  age<-as.character(age)
  regex<-paste(age,' years old',sep='')
  length(grep(regex,homicides))
}
