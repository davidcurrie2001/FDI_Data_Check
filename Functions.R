


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
PlotTableA<-function(DataToCheck = NULL){


  # IMPORTANT! Remove the BSA rows first so we're not double counting
  DataToCheck <- DataToCheck[DataToCheck$SUB_REGION!='BSA',]

  #DataToCheck <- DataToCheck[,c("YEAR","SPECIES", "TOTWGHTLANDG","TOTVALLANDG","DISCARDS")]
  
  # Change NK in Discards and TOTVALLANDG to 0 so we can aggregate - (not really correct thing to do 
  # but otherwise we can't aggrgeate for species that have some values and some NKs)
  DataToCheck[DataToCheck$DISCARDS=='NK',c("DISCARDS")] <- '0'
  DataToCheck[DataToCheck$TOTVALLANDG=='NK',c("TOTVALLANDG")] <- '0'
  
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
  Top20ByValue <- AggDataToCheckBySpecies[1:20,]
  
  # Calculate Top 10 species by discards
  AggDataToCheckBySpecies <- AggDataToCheckBySpecies[order(-AggDataToCheckBySpecies$DISCARDS),]
  Top10ByDiscards <- AggDataToCheckBySpecies[1:10,]
  
  # Plot the data
  DataToPlot <- AggDataToCheck[AggDataToCheck$SpeciesGroup %in% Top10ByWeight$SpeciesGroup ,]
  p <- ggplot(DataToPlot, aes(x=reorder(SpeciesGroup,-TOTWGHTLANDG) , y=TOTWGHTLANDG, shape = YearGroup)) +
    scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
    geom_point() +
    labs(title="Table A Top 10 Species by Landings", x="Species", y="Landings Live Weight (T)")
  print(p)
  
  DataToPlot <- AggDataToCheck[AggDataToCheck$SpeciesGroup %in% Top10ByValue$SpeciesGroup ,]
  p <- ggplot(DataToPlot, aes(x=reorder(SpeciesGroup,-TOTVALLANDG), y=TOTVALLANDG, shape = YearGroup)) +
    scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
    geom_point() +
    labs(title="Table A Top 10 Species by Value", x="Species", y="Landings Value (Euro)") 
  print(p)
  
  DataToPlot <- AggDataToCheck[AggDataToCheck$SpeciesGroup %in% Top10ByDiscards$SpeciesGroup ,]
  p <- ggplot(DataToPlot, aes(x=reorder(SpeciesGroup,-DISCARDS), y=DISCARDS, shape = YearGroup)) +
    scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
    geom_point() +
    labs(title="Table A Top 10 Species by Discards", x="Species", y="Discards (T)") 
  print(p)

  # Aggregate by Year and vessel size
  AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(DataToCheck$YEAR , DataToCheck$VESSEL_LENGTH), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "VesselLengthGroup"
  
  DataToPlot <- AggDataToCheck
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=VesselLengthGroup, y=TOTWGHTLANDG, shape = YearGroup)) +
    scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
    geom_point() +
    labs(title="Landings by Vessel Length", x="Vessel Length", y="Landings Live Weight (T)") 
  print(p)
  
  # Aggregate by Year and Fishing Tech
  AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(DataToCheck$YEAR , DataToCheck$FISHING_TECH), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "FishingTechGroup"
  
  DataToPlot <- AggDataToCheck
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=FishingTechGroup, y=TOTWGHTLANDG, shape = YearGroup)) +
    scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
    geom_point() +
    labs(title="Landings by fishing tech", x="Fishing Tech", y="Landings Live Weight (T)") 
  print(p)
  
  # Aggregate by Year and Gear
  AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(DataToCheck$YEAR , DataToCheck$GEAR_TYPE), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "GearGroup"
  
  DataToPlot <- AggDataToCheck
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=GearGroup, y=TOTWGHTLANDG, shape = YearGroup)) +
    scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
    geom_point() +
    labs(title="Landings by gear", x="Fishing Gear", y="Landings Live Weight (T)")  +
    theme(axis.text.x = element_text(angle = 90))
  print(p)
  
  # Aggregate by Year and Sub-region
  AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(DataToCheck$YEAR , DataToCheck$SUB_REGION), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "AreaGroup"
  
  DataToPlot <- AggDataToCheck
  
  # Plot the data
  p <- ggplot(DataToPlot, aes(x=AreaGroup, y=TOTWGHTLANDG, shape = YearGroup)) +
    scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
    geom_point() +
    labs(title="Landings by area", x="Area", y="Landings Live Weight (T)")  +
    theme(axis.text.x = element_text(angle = 90))
  print(p)


  
}

## Compare Table A, with Tables C, D, E, and F and see if there are any important gaps
CompareTableA<-function(DataToCheck = NULL){
  
  
  # IMPORTANT! Remove the BSA rows first so we're not double counting
  DataToCheck <- DataToCheck[DataToCheck$SUB_REGION!='BSA',]
  

  # Change NK in Discards to 0 so we can aggregate - (not really correct thing to do 
  # but otherwise we can't aggrgeate for species that have some Discards values and some NKs)
  DataToCheck[DataToCheck$DISCARDS=='NK',c("DISCARDS")] <- '0'
  
  # Change columns to numeric
  DataToCheck$TOTWGHTLANDG <- as.numeric(DataToCheck$TOTWGHTLANDG)
  DataToCheck$TOTVALLANDG <- as.numeric(DataToCheck$TOTVALLANDG)
  DataToCheck$DISCARDS <- as.numeric(DataToCheck$DISCARDS)
  
  # Calculate Top 20 species by value
  AggDataToCheckBySpecies <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(DataToCheck$SPECIES), FUN=sum)
  names(AggDataToCheckBySpecies)[names(AggDataToCheckBySpecies)=="Group.1"] <- "SpeciesGroup"
  AggDataToCheckBySpecies <- AggDataToCheckBySpecies[order(-AggDataToCheckBySpecies$TOTVALLANDG),]
  Top10ByValue <- AggDataToCheckBySpecies[1:10,]
  
  
  # Aggregate by Year, Domains, and species
  AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG","DISCARDS")], by=list(DataToCheck$YEAR, DataToCheck$DOMAIN_LANDINGS, DataToCheck$DOMAIN_DISCARDS, DataToCheck$SPECIES), FUN=sum)
  
  # Rename the columns produced by the aggregate function
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "DomainLandGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.3"] <- "DomainDiscardGroup"
  names(AggDataToCheck)[names(AggDataToCheck)=="Group.4"] <- "SpeciesGroup"
  
  names(AggDataToCheck)[names(AggDataToCheck)=="TOTWGHTLANDG"] <- "A_TOTWGHTLANDG"
  names(AggDataToCheck)[names(AggDataToCheck)=="TOTVALLANDG"] <- "A_TOTVALLANDG"
  names(AggDataToCheck)[names(AggDataToCheck)=="DISCARDS"] <- "A_DISCARDS"
  
  TabC<-unique(myTables[['Table_C_NAO']][,c("YEAR", "DOMAIN_DISCARDS", "TOTWGHTLANDG", "DISCARDS")])
  
  AC <- merge(x=AggDataToCheck, y = TabC, by.x=c("YearGroup","DomainDiscardGroup"), by.y=c("YEAR","DOMAIN_DISCARDS"),all=TRUE )
  
  names(AC)[names(AC)=="TOTWGHTLANDG"] <- "C_TOTWGHTLANDG"
  names(AC)[names(AC)=="DISCARDS"] <- "C_DISCARDS"
  
  TabD<-unique(myTables[['Table_D_NAO']][,c("YEAR", "DOMAIN_DISCARDS", "TOTWGHTLANDG", "DISCARDS")])
  
  ACD <- merge(x=AC, y = TabD, by.x=c("YearGroup","DomainDiscardGroup"), by.y=c("YEAR","DOMAIN_DISCARDS"),all=TRUE )
  
  names(ACD)[names(ACD)=="TOTWGHTLANDG"] <- "D_TOTWGHTLANDG"
  names(ACD)[names(ACD)=="DISCARDS"] <- "D_DISCARDS"
  
  TabE<-unique(myTables[['Table_E_NAO']][,c("YEAR", "DOMAIN_LANDINGS", "TOTWGHTLANDG")])
  
  ACDE <- merge(x=ACD, y = TabE, by.x=c("YearGroup","DomainLandGroup"), by.y=c("YEAR","DOMAIN_LANDINGS"),all=TRUE )
  
  names(ACDE)[names(ACDE)=="TOTWGHTLANDG"] <- "E_TOTWGHTLANDG"
  
  TabF<-unique(myTables[['Table_F_NAO']][,c("YEAR", "DOMAIN_LANDINGS", "TOTWGHTLANDG")])
  
  ACDEF <- merge(x=ACDE, y = TabF, by.x=c("YearGroup","DomainLandGroup"), by.y=c("YEAR","DOMAIN_LANDINGS"),all=TRUE )
  
  names(ACDEF)[names(ACDEF)=="TOTWGHTLANDG"] <- "F_TOTWGHTLANDG"
  
  #summaryACDEF <- aggregate(ACDEF[,], by=list(ACDEF$YearGroup, ACDEF$SpeciesGroup), FUN=length)
  
  summaryACDEF <- aggregate(ACDEF[,], by=list(ACDEF$YearGroup, ACDEF$SpeciesGroup), FUN=function(x) { sum(!is.na(x))})
  
  names(summaryACDEF)[names(summaryACDEF)=="Group.1"] <- "Year"
  names(summaryACDEF)[names(summaryACDEF)=="Group.2"] <- "Species"
  names(summaryACDEF)[names(summaryACDEF)=="DomainLandGroup"] <- "Landings_Domains"
  names(summaryACDEF)[names(summaryACDEF)=="DomainDiscardGroup"] <- "Discards_Domains"
  names(summaryACDEF)[names(summaryACDEF)=="C_DISCARDS"] <- "C_Domains"
  names(summaryACDEF)[names(summaryACDEF)=="D_DISCARDS"] <- "D_Domains"
  names(summaryACDEF)[names(summaryACDEF)=="E_TOTWGHTLANDG"] <- "E_Domains"
  names(summaryACDEF)[names(summaryACDEF)=="F_TOTWGHTLANDG"] <- "F_Domains"
  

  summaryFinal <- merge(x=summaryACDEF, y=Top10ByValue, by.x = "Species", by.y="SpeciesGroup")
  
  summaryFinal[order(summaryFinal$Year, -summaryFinal$TOTVALLANDG),c("Year", "Species", "Landings_Domains", "Discards_Domains", "C_Domains", "D_Domains", "E_Domains", "F_Domains")]
  
}

# Produce boxplot of ages
AgeBoxPlot <-function(Data, PlotTitle){
  
  DataToPlot <- Data
  
  # Get rid of NKs
  DataToPlot <- DataToPlot[DataToPlot$AGE!="NK",c("AGE")]
  DataToPlot$AGE <- as.numeric(DataToPlot$AGE)
  
  if (nrow(DataToPlot)>0){
    boxplot(DataToPlot$AGE, main=PlotTitle, ylab="Age")
  } else {
    print("No data to plot")
  }
  
}

# Produce boxplot of lengths
LengthBoxPlot <-function(Data, PlotTitle){
  
  DataToPlot <- Data
  
  # Get rid of NKs
  DataToPlot <- DataToPlot[DataToPlot$LENGTH!="NK",]
  DataToPlot$LENGTH <- as.numeric(DataToPlot$LENGTH)
  
  # Change mm to cm if required
  DataToPlot[DataToPlot$LENGTH_UNIT=="mm",c("LENGTH")]<-DataToPlot[DataToPlot$LENGTH_UNIT=="mm",c("LENGTH")]/10.0
  
  if (nrow(DataToPlot)>0){
    boxplot(DataToPlot$LENGTH, main=PlotTitle, ylab="Length (cm)")
  } else {
    print("No data to plot")
  }
  
}




# Function used to plot out numbers at age by species and year for Table C and E
PlotNoAtAge <-function(LogYScale = FALSE, SpeciesToPlot = NULL, Data, PlotTitle){
  
  if (LogYScale == TRUE) {
    PlotTitle <- paste(PlotTitle,"(Log scale Y axis)")
  }
  
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
  
  if (nrow(DataToPlot)>0){
  
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
    
  } else {
    print("No data to plot")
  }
  
}

# Function used to plot out numbers at length by species and year for Table D and F
PlotNoAtLength <- function(LogYScale = FALSE, SpeciesToPlot = NULL, Data, PlotTitle){
  
  if (LogYScale == TRUE) {
    PlotTitle <- paste(PlotTitle,"(Log scale Y axis)")
  }
  
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
  
  if (nrow(DataToPlot)>0){
  
    # Change Age to a number
    DataToPlot$LENGTH <- as.numeric(DataToPlot$LENGTH)
    
    # Change mm to cm if required
    DataToPlot[DataToPlot$LENGTH_UNIT=="mm",c("LENGTH")]<-DataToPlot[DataToPlot$LENGTH_UNIT=="mm",c("LENGTH")]/10.0
    
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
      labs(title=PlotTitle, x="Length (cm)", y="Raised Number at Length") 
    
    if (LogYScale == TRUE) {
      p<-p+scale_y_log10()
    }
    
    p<- p + facet_grid(vars(SpeciesGroup), vars(YearGroup))
    
    print(p)
  
  } else {
    print("No data to plot")
  }
  
}


# Summarise Effort data
PlotTableG <- function(DataToCheck = NULL){
  
  # IMPORTANT! Remove the BSA rows first so we're not double counting
  DataToCheck <- DataToCheck[DataToCheck$SUB_REGION!='BSA',]
  
  if (nrow(DataToCheck)>0) {
  
    # Change NK in to 0 so we can aggregate - (not really correct thing to do 
    # but otherwise we can't aggrgeate for species that have some Discards values and some NKs)
    DataToCheck[DataToCheck$TOTSEADAYS=='NK',c("TOTSEADAYS")] <- '0'
    DataToCheck[DataToCheck$TOTFISHDAYS=='NK',c("TOTFISHDAYS")] <- '0'
    DataToCheck[DataToCheck$TOTVES=='NK',c("TOTVES")] <- '0'
    
    # Change columns to numeric
    DataToCheck$TOTSEADAYS <- as.numeric(DataToCheck$TOTSEADAYS)
    DataToCheck$TOTFISHDAYS <- as.numeric(DataToCheck$TOTFISHDAYS)
    DataToCheck$TOTVES <- as.numeric(DataToCheck$TOTVES)
    
    # Aggregate by year and vessel length
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTSEADAYS","TOTFISHDAYS","TOTVES")], by=list(DataToCheck$YEAR , DataToCheck$VESSEL_LENGTH), FUN=sum)
  
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "VesselLengthGroup"
    
    DataToPlot <- AggDataToCheck
    
    p <- ggplot(DataToPlot, aes(x=VesselLengthGroup, y = value, color = Type, shape = YearGroup)) +
      scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
      geom_point(aes(y = TOTSEADAYS, col = "Sea Days")) + 
      geom_point(aes(y = TOTFISHDAYS, col = "Fishing Days")) +
      labs(title="Sea/Fishing days by vessel length", x="Vessel length", y="Days")
    print(p)
    
  
    # Aggregate by year and fishing tech
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTSEADAYS","TOTFISHDAYS","TOTVES")], by=list(DataToCheck$YEAR , DataToCheck$FISHING_TECH), FUN=sum)
    
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "FishingTechGroup"
    
    DataToPlot <- AggDataToCheck
    
    p <- ggplot(DataToPlot, aes(x=FishingTechGroup, y = value, color = Type, shape = YearGroup)) + 
      scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
      geom_point(aes(y = TOTSEADAYS, col = "Sea Days")) + 
      geom_point(aes(y = TOTFISHDAYS, col = "Fishing Days")) +
      labs(title="Sea/Fishing days by fishing tech", x="Fishing Tech", y="Days")
    
    print(p)
    
    # Aggregate by year and gear
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTSEADAYS","TOTFISHDAYS","TOTVES")], by=list(DataToCheck$YEAR , DataToCheck$GEAR_TYPE), FUN=sum)
    
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "GearGroup"
    
    DataToPlot <- AggDataToCheck
    
    p <- ggplot(DataToPlot, aes(x=GearGroup, y = value, color = Type, shape = YearGroup)) + 
      scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
      geom_point(aes(y = TOTSEADAYS, col = "Sea Days")) + 
      geom_point(aes(y = TOTFISHDAYS, col = "Fishing Days")) +
      labs(title="Sea/Fishing days by gear", x="Gear", y="Days") +
      theme(axis.text.x = element_text(angle = 90))
    
    print(p)
    
    # Aggregate by year and area
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTSEADAYS","TOTFISHDAYS","TOTVES")], by=list(DataToCheck$YEAR , DataToCheck$SUB_REGION), FUN=sum)
    
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "AreaGroup"
    
    DataToPlot <- AggDataToCheck
    
    p <- ggplot(DataToPlot, aes(x=AreaGroup, y = value, color = Type, shape = YearGroup)) + 
      scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
      geom_point(aes(y = TOTSEADAYS, col = "Sea Days")) + 
      geom_point(aes(y = TOTFISHDAYS, col = "Fishing Days")) +
      labs(title="Sea/Fishing days by area", x="Area", y="Days") +
      theme(axis.text.x = element_text(angle = 90))
    
    print(p)
    
  } else {
    print("No data to plot")
  }
  
}

# Produces maps of landings data
plotTableH <- function(DataToCheck = NULL){
  
  # IMPORTANT! Remove the BSA rows first so we're not double counting
  DataToCheck <- DataToCheck[DataToCheck$SUB_REGION!='BSA',]
  
  DataToCheck[DataToCheck$EEZ_INDICATOR == "EU","EEZ_INDICATOR"] <- "1) EU"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "UK","EEZ_INDICATOR"] <- "2) UK"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "NA","EEZ_INDICATOR"] <- "3) NA"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "COAST","EEZ_INDICATOR"] <- "4) COAST"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "RFMO","EEZ_INDICATOR"] <- "5) RFMO"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "NK","EEZ_INDICATOR"] <- "6) NK"
  
  if(nrow(DataToCheck)>0){
  
    # Change NK in TOTVALLANDG to 0 so we can aggregate - (not really correct thing to do 
    # but otherwise we can't aggrgeate for species that have some Discards values and some NKs)
    DataToCheck[DataToCheck$TOTVALLANDG=='NK',c("TOTVALLANDG")] <- '0'
    
    # Change columns to numeric
    DataToCheck$TOTWGHTLANDG <- as.numeric(DataToCheck$TOTWGHTLANDG)
    DataToCheck$TOTVALLANDG <- as.numeric(DataToCheck$TOTVALLANDG)
  
    # Aggregate by Year and lat/lon
    #AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG")], by=list(DataToCheck$YEAR , DataToCheck$RECTANGLE_LON, DataToCheck$RECTANGLE_LAT), FUN=sum)
    # Aggregate by Year and lat/lon and EEZ
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTWGHTLANDG","TOTVALLANDG")], by=list(DataToCheck$YEAR , DataToCheck$RECTANGLE_LON, DataToCheck$RECTANGLE_LAT,DataToCheck$EEZ_INDICATOR), FUN=sum)
    
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "Lon"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.3"] <- "Lat"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.4"] <- "EEZ"
    
    AggDataToCheck$Lon <- as.numeric(AggDataToCheck$Lon)
    AggDataToCheck$Lat <- as.numeric(AggDataToCheck$Lat)
    
    for (myYear in sort(unique(AggDataToCheck$YearGroup))){
      p<- getMap(Year=myYear, DataToPlot=AggDataToCheck, VariableToPlot = "TOTWGHTLANDG", ChartTitle = "Landings Live Weight (T)", VariableForColour="EEZ")
      print(p)
    }
    
  
  } else {
    print("No data to plot")
  }
      
}

# Utility function to create maps
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
getMap <- function(Year = NULL, DataToPlot = NULL, VariableToPlot = NULL, ChartTitle = NULL, VariableForColour = NULL){
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  minLon <- min(DataToPlot$Lon, na.rm = T)
  maxLon <- max(DataToPlot$Lon, na.rm = T)
  
  minLat <- min(DataToPlot$Lat, na.rm = T)
  maxLat <- max(DataToPlot$Lat, na.rm = T)
  
  # Note use aes-string instead of aes to pass in the names of the variables rather than the actual objects
  p <- ggplot(data = world) +
    geom_sf() +
    ggtitle( paste(Year,ChartTitle)) +
    geom_point(data = DataToPlot[DataToPlot$YearGroup==Year,], aes_string(x = "Lon", y = "Lat", size=VariableToPlot, color = VariableForColour)) +
    coord_sf(xlim = c(minLon,maxLon ), ylim = c(minLat, maxLat), expand = TRUE)
  
}

# Produces maps of effort data
plotTableI <- function(DataToCheck = NULL){
  
  # IMPORTANT! Remove the BSA rows first so we're not double counting
  DataToCheck <- DataToCheck[DataToCheck$SUB_REGION!='BSA',]
  
  DataToCheck[DataToCheck$EEZ_INDICATOR == "EU","EEZ_INDICATOR"] <- "1) EU"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "UK","EEZ_INDICATOR"] <- "2) UK"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "NA","EEZ_INDICATOR"] <- "3) NA"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "COAST","EEZ_INDICATOR"] <- "4) COAST"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "RFMO","EEZ_INDICATOR"] <- "5) RFMO"
  DataToCheck[DataToCheck$EEZ_INDICATOR == "NK","EEZ_INDICATOR"] <- "6) NK"
  
  if(nrow(DataToCheck)>0){
  
    # Change columns to numeric
    DataToCheck$TOTFISHDAYS <- as.numeric(DataToCheck$TOTFISHDAYS)
  
    # Aggregate by Year and lat/lon
    #AggDataToCheck <- aggregate(DataToCheck[,c("TOTFISHDAYS")], by=list(DataToCheck$YEAR , DataToCheck$RECTANGLE_LON, DataToCheck$RECTANGLE_LAT), FUN=sum)
    # Aggregate by Year and lat/lon and EEZ
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTFISHDAYS")], by=list(DataToCheck$YEAR , DataToCheck$RECTANGLE_LON, DataToCheck$RECTANGLE_LAT,DataToCheck$EEZ_INDICATOR), FUN=sum)
    
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "Lon"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.3"] <- "Lat"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.4"] <- "EEZ"
    
    AggDataToCheck$Lon <- as.numeric(AggDataToCheck$Lon)
    AggDataToCheck$Lat <- as.numeric(AggDataToCheck$Lat)
    
    for (myYear in sort(unique(AggDataToCheck$YearGroup))){
      p<- getMap(Year=myYear, DataToPlot=AggDataToCheck, VariableToPlot = "TOTFISHDAYS", ChartTitle = "Fishing Days", VariableForColour="EEZ")
      print(p)
    }
    
    
  } else {
    print("No data to plot")
  }
}

# Summarise Tabel J
plotTableJ <- function(DataToCheck = NULL){
  
  if (nrow(DataToCheck)>0){
  
    # Change NK to 0 so we can aggregate - (not really correct thing to do 
    # but otherwise we can't aggrgeate for columns that have some values and some NKs)
    DataToCheck[DataToCheck$TOTTRIPS=='NK',c("TOTTRIPS")] <- '0'
  
    # Change columns to numeric
    DataToCheck$TOTTRIPS <- as.numeric(DataToCheck$TOTTRIPS)
  
    # Aggregate by Year and length
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTTRIPS")], by=list(DataToCheck$YEAR , DataToCheck$VESSEL_LENGTH), FUN=sum)
    
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "VesselLengthGroup"
    
    DataToPlot <- AggDataToCheck
    
    # Plot the data
    p <- ggplot(DataToPlot, aes(x=VesselLengthGroup, y=TOTTRIPS, shape = YearGroup)) +
      scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
      geom_point() +
      labs(title="Number of trips by vessel length", x="Vessel Length", y="Number")
    print(p)
    
    # Aggregate by Year and fishing tech
    AggDataToCheck <- aggregate(DataToCheck[,c("TOTTRIPS")], by=list(DataToCheck$YEAR , DataToCheck$FISHING_TECH), FUN=sum)
    
    # Rename the columns produced by the aggregate function
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.1"] <- "YearGroup"
    names(AggDataToCheck)[names(AggDataToCheck)=="Group.2"] <- "FishingTechGroup"
    
    DataToPlot <- AggDataToCheck
    
    # Plot the data
    p <- ggplot(DataToPlot, aes(x=FishingTechGroup, y=TOTTRIPS, shape = YearGroup)) +
      scale_shape_manual(values=1:length(unique(DataToPlot$YearGroup))) +
      geom_point() +
      labs(title="Number of trips by fishing tech (log scale y axis)", x="Fishing tech", y="Number") +
      scale_y_log10()
    print(p)

  } else {
    print("No data to plot")
  }
  
}

