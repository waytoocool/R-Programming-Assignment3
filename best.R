# To find the best hospital in a region
best <- function(state, outcome)
{
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% unique(data$State))
    stop("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("Invalid outcome")
  
  ### Return hospital name in that state with lowest 30-day death
  
  #Select the correct row no. for the relevant records
  if (outcome == "heart attack") outcome = 11
  if (outcome == "heart failure") outcome = 17
  if (outcome == "pneumonia") outcome = 23
  
  
  #Coerce the data into numeric
  data[,outcome] <- as.numeric(data[,outcome])
  
  #Select Hospital name according to state and outcome
  name <- data[data[,outcome] == min(data[data$State == state,outcome], na.rm = TRUE) & #calculate min. and include rows with min. values
                !is.na(data[,outcome]) &  # Remove all the NA from the output as rows with NA will also be included in previous condition 
                 data$State == state, ]$Hospital.Name #include rows with valid state
  sort(name)[1]
  
}