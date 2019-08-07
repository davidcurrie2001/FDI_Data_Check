


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

# Function to produce some summary plots of Table A for sanity checking
PlotTableA<-function(){

  DataToCheck <- myTables[['Table_A']]
  
  # Remove the BSA values first so we're not double counting
  DataToCheck <- DataToCheck[DataToCheck$SUB_REGION!='BSA',]

  DataToCheck <- DataToCheck[,c("YEAR","SPECIES", "TOTWGHTLANDG","TOTVALLANDG","DISCARDS")]
  
  # Change NK in Discards to 0 so we can aggregate - (not really correct thing to do 
  # but otherwise we can't aggrgeate for species that have some Discards values and some NKs)
  DataToCheck[DataToCheck$DISCARDS=='NK',c("DISCARDS")] <- 0
  
  # Change columns to numeric
  DataToCheck$TOTWGHTLANDG <- as.numeric(DataToCheck$TOTWGHTLANDG)
  DataToCheck$TOTVALLANDG <- as.numeric(DataToCheck$TOTVALLANDG)
  DataToCheck$DISCARDS <- as.numeric(DataToCheck$DISCARDS)
  
  # Aggregate by Year and species
  AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(DataToCheck$YEAR , DataToCheck$SPECIES), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "SpeciesGroup"
  
  # Aggrgeate by species
  AggDataToCheckBySpecies <- aggregate(AggDataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(AggDataToCheck$SpeciesGroup), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(AggDataToCheckBySpecies)[names(AggDataToCheckBySpecies)=="Group.1"] <- "SpeciesGroup"
  
  # Calculate Top 10 species by weight
  AggDataToCheckBySpecies <- AggDataToCheckBySpecies[order(-AggDataToCheckBySpecies$TOTWGHTLANDG),]
  Top10ByWeight <- AggDataToCheckBySpecies[1:10,]
  
  # Calculate Top 10 species by value
  AggDataToCheckBySpecies <- AggDataToCheckBySpecies[order(-AggDataToCheckBySpecies$TOTVALLANDG),]
  Top10ByValue <- AggDataToCheckBySpecies[1:10,]
  
  # Calculate Top 10 species by discards
  AggDataToCheckBySpecies <- AggDataToCheckBySpecies[order(-AggDataToCheckBySpecies$DISCARDS),]
  Top10ByDiscards <- AggDataToCheckBySpecies[1:10,]
  
  # Plot the data
  DataToPlot <- AggDataToCheck[AggDataToCheck$SpeciesGroup %in% Top10ByWeight$SpeciesGroup ,]
  p <- ggplot(DataToPlot, aes(x=SpeciesGroup, y=TOTWGHTLANDG, shape = YearGroup)) +
    geom_point() +
    labs(title="Table A Top 10 Species by Landings", x="Species", y="Landings Live Weight (T)") 
  print(p)
  
  DataToPlot <- AggDataToCheck[AggDataToCheck$SpeciesGroup %in% Top10ByValue$SpeciesGroup ,]
  p <- ggplot(DataToPlot, aes(x=SpeciesGroup, y=TOTVALLANDG, shape = YearGroup)) +
    geom_point() +
    labs(title="Table A Top 10 Species by Value", x="Species", y="Landings Value (€)") 
  print(p)
  
  DataToPlot <- AggDataToCheck[AggDataToCheck$SpeciesGroup %in% Top10ByDiscards$SpeciesGroup ,]
  p <- ggplot(DataToPlot, aes(x=SpeciesGroup, y=DISCARDS, shape = YearGroup)) +
    geom_point() +
    labs(title="Table A Top 10 Species by Discards", x="Species", y="Discards (T)") 
  print(p)

}

# Function used to plot out numbers at age by species and year for Table C and E
PlotNoAtAge <-function(LogYScale = FALSE, SpeciesToPlot = NULL, Data, PlotTitle){
  
  #DataToPlot <- Table_C_NAO
  #DataToPlot <- myTables[['Table_C_NAO']]
  DataToPlot <- Data
  
  # Filter the data if required
  if(is.null(SpeciesToPlot)){
    # Do nothing
  }
  else if (NROW(SpeciesToPlot)>1) {
    DataToPlot<- DataToPlot[DataToPlot$SPECIES %in% SpeciesToPlot,]
  } else {
    DataToPlot<- DataToPlot[DataToPlot$SPECIES==SpeciesToPlot,]
  }
  
  # Check NKs
  NKData <- DataToPlot[DataToPlot$AGE=="NK" | DataToPlot$NO_AGE=="NK" ,]
  
  if (NROW(NKData) >0) {
    print("Checking how many rows per species have NK 'AGE' or 'NO_AGE' - these are excluded")
    AggNKData <- aggregate(NKData[,c("COUNTRY")], by=list(NKData$SPECIES), FUN=length)
    names(AggNKData)[names(AggNKData)=="Group.1"] <- "SpeciesGroup"
    names(AggNKData)[names(AggNKData)=="COUNTRY"] <- "Number_Of_NK_Rows"
    print(AggNKData[,])
    #kable(AggNKData,caption = "Rows per species with NK 'AGE' or 'NO_AGE' - these are excluded")
  
  }
  
  # Get rid of NKs
  DataToPlot <- DataToPlot[DataToPlot$AGE!="NK" & DataToPlot$NO_AGE!="NK" ,]
  
  
  
  # Change Age to a number
  DataToPlot$AGE <- as.numeric(DataToPlot$AGE)
  
  # Multiply NO_AGE by 1000 to get the actual raised number
  DataToPlot$NO_AGE <- as.numeric(DataToPlot$NO_AGE) * 1000.0
  
  # Aggregate by Year, Species, and Age
  DataToPlot <- aggregate(DataToPlot[,c("AGE","NO_AGE")], by=list(DataToPlot$YEAR , DataToPlot$AGE, DataToPlot$SPECIES), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(DataToPlot)[names(DataToPlot)=="Group.1"] <- "YearGroup"
  names(DataToPlot)[names(DataToPlot)=="Group.2"] <- "AgeGroup"
  names(DataToPlot)[names(DataToPlot)=="Group.3"] <- "SpeciesGroup"
  
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=AgeGroup, y=NO_AGE)) +
    geom_point() +
    labs(title=PlotTitle, x="Age", y="Raised Number at Age") 
  
  if (LogYScale == TRUE) {
    p<-p+scale_y_log10()
  }
  
  p<- p + facet_grid(vars(SpeciesGroup), vars(YearGroup))
  
  print(p)
  
}

# Function used to plot out numbers at length by species and year for Table D and F
PlotNoAtLength <- function(LogYScale = FALSE, SpeciesToPlot = NULL, Data, PlotTitle){
  
  DataToPlot <- Data
  
  # Filter the data if required
  if(is.null(SpeciesToPlot)){
    # Do nothing
  }
  else if (NROW(SpeciesToPlot)>1) {
    DataToPlot<- DataToPlot[DataToPlot$SPECIES %in% SpeciesToPlot,]
  } else {
    DataToPlot<- DataToPlot[DataToPlot$SPECIES==SpeciesToPlot,]
  }
  
  # Check NKs
  NKData <- DataToPlot[DataToPlot$LENGTH=="NK" | DataToPlot$NO_LENGTH=="NK" ,]
  
  if (NROW(NKData) >0) {
    print("Checking how many rows per species have NK 'LENGTH' or 'NO_LENGTH' - these are excluded")
    AggNKData <- aggregate(NKData[,c("COUNTRY")], by=list(NKData$SPECIES), FUN=length)
    names(AggNKData)[names(AggNKData)=="Group.1"] <- "SpeciesGroup"
    names(AggNKData)[names(AggNKData)=="COUNTRY"] <- "Number_Of_NK_Rows"
    print(AggNKData[,])
    #kable(AggNKData,caption = "Rows per species with NK 'AGE' or 'NO_AGE' - these are excluded")
    
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
  names(DataToPlot)[names(DataToPlot)=="Group.1"] <- "YearGroup"
  names(DataToPlot)[names(DataToPlot)=="Group.2"] <- "LengthGroup"
  names(DataToPlot)[names(DataToPlot)=="Group.3"] <- "SpeciesGroup"
  
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=LengthGroup, y=NO_LENGTH)) +
    geom_point() +
    labs(title=PlotTitle, x="Length", y="Raised Number at Length") 
  
  if (LogYScale == TRUE) {
    p<-p+scale_y_log10()
  }
  
  p<- p + facet_grid(vars(SpeciesGroup), vars(YearGroup))
  
  print(p)
  
}


PlotTableC <-function(LogYScale = FALSE, SpeciesToPlot = NULL){
  
  PlotNoAtAge(LogYScale = LogYScale,SpeciesToPlot = SpeciesToPlot, Data = myTables[['Table_C_NAO']], PlotTitle = "Table C Discards by age" )
}

PlotTableD <-function(LogYScale = FALSE, SpeciesToPlot = NULL){
  
  PlotNoAtLength(LogYScale = LogYScale,SpeciesToPlot = SpeciesToPlot, Data = myTables[['Table_D_NAO']], PlotTitle = "Table D Discards by length" )
}

PlotTableE <-function(LogYScale = FALSE, SpeciesToPlot = NULL){
  
  PlotNoAtAge(LogYScale = LogYScale,SpeciesToPlot = SpeciesToPlot, Data = myTables[['Table_E_NAO']], PlotTitle = "Table E Landings by age" )
}


PlotTableF <-function(LogYScale = FALSE, SpeciesToPlot = NULL){
  
  PlotNoAtLength(LogYScale = LogYScale,SpeciesToPlot = SpeciesToPlot, Data = myTables[['Table_F_NAO']], PlotTitle = "Table F Landings by length" )
}

