best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_state <- levels(factor(data[ , 7]))
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  
  if ((state %in% valid_state) == FALSE){stop(print("invalid state"))}
  else if ((outcome %in% valid_outcome) == FALSE){stop(print("invalid outcome"))}
  
  colnumber <- if (outcome == "heart attack"){11}
  else if (outcome == "heart failure"){17}
  else {23}
  
  selectedData <- subset(data, State == state)
  selectedCol <- suppressWarnings(as.numeric(selectedData[ , colnumber]))
  
  selectedData <- selectedData[!(is.na(selectedCol)), ]
  selectedCol <- as.numeric(selectedData[ , colnumber])
  
  row <- which(selectedCol == min(selectedCol))
  
  hospital <- selectedData[row, 2]
  
  sortedhospital <- sort(hospital)
  
  return(sortedhospital)
  
}