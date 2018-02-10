#Get hospital name statewise for specified rank
rankall <- function(outcome, num = "best"){
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("Invalid outcome")
  
  
  #Select the correct row no. for the relevant records
  if (outcome == "heart attack") outcome = 11
  if (outcome == "heart failure") outcome = 17
  if (outcome == "pneumonia") outcome = 23
  
  Output <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("hospital", "state"))
  
  #Coerce the data into numeric
  data[,outcome] <- as.numeric(data[,outcome])
  
  #Looping through all states  
  for (state in unique(data$State)) {
    statedata <- data[data$State == state & !is.na(data[, outcome]), ]
    
  #sort by outcome
  name <- statedata[order(statedata[, outcome], statedata$Hospital.Name), ]
  
  #Check num condition
  if (num == "best") {
    num <- 1
    Output <- rbind(Output, data.frame(hospital = name$Hospital.Name[num], state = state))
  }
  else if (num == "worst") {
    Output <- rbind(Output, data.frame(hospital = as.character(name$Hospital.Name[nrow(name)]), state = state))
  }
  else if (num > nrow(name)){
    Output <- rbind(Output, data.frame(hospital = NA, state = state))
  }
  else
    Output <- rbind(Output, data.frame(hospital = name$Hospital.Name[num], state = state))
  }
  Output[order(as.character(Output$state)),]
}
