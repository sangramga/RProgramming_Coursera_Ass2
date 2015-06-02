rankall <- function(outcome,num = "best")
{
  path <- "~/hospital/outcome-of-care-measures.csv" #file path
  
  #check validity of outcome arguement
  out <- c("heart attack","heart failure","pneumonia")
  if(outcome %in% out == FALSE) stop("invalid outcome")
  else x <- match(x = outcome,table = out)
  
  
  #check validity of state
  state.data <- read.csv(file = path)[,7]
  if(state %in% state.data == FALSE) stop("invalid state")
  
  
  
  df <- read.csv(file = path,colClasses = "character")
  
  allstates <- levels(as.factor(df[,7]))
  h.name <- character()
  for(i in 1:length(allstates))
  {#split according to state
    s <- split(df,df$State)
    df1 <- s[[allstates[i]]] #store the data of state variable in df1
    if(x == 1)
    {
      suppressWarnings(h.attack <- as.numeric(df1[,11])) 
      o <- order(h.attack,df1[,2],na.last = NA,decreasing = FALSE)
    }  
  
    if(x==2)
    {
      suppressWarnings(h.fail <- as.numeric(df1[,17]))
      o <- order(h.fail,df1[,2],na.last = NA,decreasing = FALSE)
    }
  
    if(x==3)
    {
      suppressWarnings(h.pneum <- as.numeric(df1[,23]))
      o <- order(h.pneum,df1[,2],na.last = NA,decreasing = FALSE)
    
    }
    #looking at 'num' variable
    if(num == "best") n <- 1
    else if(num == "worst") n<- length(o)
    else n<-num
  
    h.name <- c(h.name,df1[o[n],2])
  }
  
  output <- data.frame(hospital = h.name,state = allstates)
}