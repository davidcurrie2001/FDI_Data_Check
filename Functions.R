
YearsPresentInData <- function(myData){
  
  returnText <- ""
  
  
  if("YEAR" %in% names(myData)){
    
    returnText <- paste(NROW(unique(myData$YEAR)),"year(s) present in the data:")
    years <- paste(sort(unique(myData$YEAR)),collapse=",")
    returnText <- paste(returnText, years)
    
  } else {
    returnText <- paste("Column 'YEAR' not found in the data")
  }
  
  returnText
  
}

ValuesPresentInData <- function(myData,myColumn){
  
  returnText <- ""
  
  if(myColumn %in% names(myData)){
    
    myValues <- myData[[myColumn]]
    myValues <- sort(unique(myValues))
    
    returnText <- paste(NROW(myValues),"unique values present in the data:")
    values <- paste(myValues,collapse=",")
    returnText <- paste(returnText, values)
    
  } else {
    returnText <- paste("Column '",myColumn,"' not found in the data")
  }
  
  returnText
  
}



PlotTableC <-function(LogYScale = FALSE){
  
  #DataToPlot <- Table_C_NAO
  DataToPlot <- myTables[['Table_C_NAO']]
  
  # Get rid of NKs
  DataToPlot <- DataToPlot[DataToPlot$AGE!="NK" & DataToPlot$NO_AGE!="NK" ,]
  
  # Change Age to a number
  DataToPlot$AGE <- as.numeric(DataToPlot$AGE)
  
  # Multiply NO_AGE by 1000 to get the actual raised number
  DataToPlot$NO_AGE <- as.numeric(DataToPlot$NO_AGE) * 1000.0
  
  # Aggregate by Year, Species, and Age
  DataToPlot <- aggregate(DataToPlot[,c("AGE","NO_AGE")], by=list(DataToPlot$YEAR , DataToPlot$AGE, DataToPlot$SPECIES), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(DataToPlot)[names(DataToPlot)=="Group.1"] <- "Year"
  names(DataToPlot)[names(DataToPlot)=="Group.2"] <- "Age"
  names(DataToPlot)[names(DataToPlot)=="Group.3"] <- "Species"
  
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=Age, y=NO_AGE)) +
    geom_point() +
    labs(title="Table C Discards by age", x="Age", y="Raised Number at Age") 
  
  if (LogYScale == TRUE) {
    p<-p+scale_y_log10()
  }
  
  p<- p + facet_grid(vars(Species), vars(Year))
  
  print(p)
  
}


PlotTableD <-function(LogYScale = FALSE, SpeciesToPlot = NULL){

  #DataToPlot <- Table_D_NAO
  DataToPlot <- myTables[['Table_D_NAO']]
  
  # Filter the data if required
  if(is.null(SpeciesToPlot)){
    # Do nothing
  }
  else if (NROW(SpeciesToPlot)>1) {
    DataToPlot<- DataToPlot[DataToPlot$SPECIES %in% SpeciesToPlot,]
  } else {
    DataToPlot<- DataToPlot[DataToPlot$SPECIES==SpeciesToPlot,]
  }
  
  
  # Get rid of NKs
  DataToPlot <- DataToPlot[DataToPlot$LENGTH!="NK" & DataToPlot$NO_LENGTH!="NK" ,]
  
  # Change Age to a number
  DataToPlot$LENGTH <- as.numeric(DataToPlot$LENGTH)
  
  # Multiply NO_AGE by 1000 to get the actual raised number
  DataToPlot$NO_LENGTH <- as.numeric(DataToPlot$NO_LENGTH) * 1000.0
  
  # Aggregate by Year, Species, and Age
  DataToPlot <- aggregate(DataToPlot[,c("LENGTH","NO_LENGTH")], by=list(DataToPlot$YEAR , DataToPlot$LENGTH, DataToPlot$SPECIES), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(DataToPlot)[names(DataToPlot)=="Group.1"] <- "Year"
  names(DataToPlot)[names(DataToPlot)=="Group.2"] <- "Length"
  names(DataToPlot)[names(DataToPlot)=="Group.3"] <- "Species"
  
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=LENGTH, y=NO_LENGTH)) +
    geom_point() +
    labs(title="Table D Discards by length", x="Length", y="Raised Number at Length") 
  
  if (LogYScale == TRUE) {
    p<-p+scale_y_log10()
  }
  
  p<- p + facet_grid(vars(Species), vars(Year))
  
  print(p)

}

