# Hospital ranking according to the state
rankhospital <- function(state, outcome, num = 'best')
{
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
  ## Check that state and outcome are valid
  if (!state %in% unique(data$State))
    stop("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("Invalid outcome")
  
  #Select the correct row no. for the relevant records
  if (outcome == "heart attack") outcome = 11
  if (outcome == "heart failure") outcome = 17
  if (outcome == "pneumonia") outcome = 23
  
  #Coerce the data into numeric
  data[,outcome] <- as.numeric(data[,outcome])
  
 #Filter by state
  statedata <- data[data$State == state & !is.na(data[, outcome]), ]
  
  #sort by outcome
  name <- statedata[order(statedata[, outcome], statedata$Hospital.Name), ]
  
  #Check num condition
  if (num == "best") num <- 1
  else if (num == "worst") num <- nrow(name)
  else if (num > nrow(name)) return(NA)
  
  name$Hospital.Name[num]
}