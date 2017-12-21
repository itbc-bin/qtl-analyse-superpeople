# only if xlsx gives error
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_131\\jre")
install.packages("rJava")
library(rJava)
install.packages("xlsx")
library(xlsx)

abData <- read.csv("CvixLerC9Rready3.csv",sep=",",header=FALSE) # Retrieve data
abTable <- t(abData)                                            # Put data in table
colnames(abTable) <- abTable[1,]                                # Use first row to create colums names 
abTable <- abTable[-1,]                                         # Remove first row

phenoData <- read.csv("CvixLerC9.qua~",sep="\t",header=FALSE)   # Phenotypedata ophalen
row.names(abTable) <- phenoData[,1]                             # Give rows the name of the plant
abTable <- cbind(abTable, phenoData[2])                         # Put data in table
colnames(abTable)[ncol(abTable)] <- "Phenotype"                 # Give the last column a name
phenoData <- suppressWarnings(transform(abTable, Phenotype = as.numeric(levels(Phenotype))[Phenotype]))
                                                                # Tell R that the data is numeric. If not: data is corrupted
                                                                # The supress warning is the warning that if a letter is in the data it will be conferted to NA
View(abTable)
abTable[abTable=="-"] <- NA                                     # Replace unavailable data with NA

# Create function that put multple strings from vector in a single string
getNames <- function(x,print=TRUE,npar=TRUE){
  p <- NULL
  for (plant in x){
    if(length(p) == 0){
      p <- plant
    } 
    else {
      p <- paste(p,plant,sep=",")
    }
  }
  return(p)
}


resultMatrix <- NULL                                          # Create empty variable to store the results
for(naam in colnames(abTable[1:ncol(abTable)-1])){
  # In this loop all the markers are isolated with the phenotype data, 
  # used for test and after isolation of different variables stored in an xlsx file
  markerTable <- abTable[,c(naam,"Phenotype")]
  containNA <- getNames(row.names(markerTable[rowSums(is.na(markerTable)) > 0,]))

  markerTable <- na.exclude(markerTable)
  markerTable <- suppressWarnings(transform(markerTable, Phenotype = as.numeric(levels(Phenotype))[Phenotype]))
  
  marker <- colnames(markerTable[1])
  result <- aov(formula = markerTable[,"Phenotype"]~markerTable[,1])
  
  pValue <- summary(result)[[1]][["Pr(>F)"]][[1]]
  fValue <- summary(result)[[1]][["F value"]][[1]]
  SSbetween <- summary(result)[[1]][["Sum Sq"]][[1]]
  SSwithin <- summary(result)[[1]][["Sum Sq"]][[2]]
  
  A <- markerTable[markerTable[,1]=="a",]
  B <- markerTable[markerTable[,1]=="b",]
  
  nPlantsA <- nrow(A)
  nPlantsB <- nrow(B)
  
  varA <- var(A[,2])
  varB <- var(B[,2])
  meanA <- mean(A[,2])
  meanB <- mean(B[,2])
  
  plantsA <- getNames(row.names(A))
  plantsB <- getNames(row.names(B))
  
  aovAllowed <- (!(varA/varB > 5 || varA/varB < -5))
  
  # If else statement to either create and add the first data row 
  # or add new data rows to the result matrix
  if(is.null(resultMatrix)){
    if(pValue<0.05){
    resultMatrix <- matrix(c(marker,aovAllowed,pValue,fValue,
                             plantsA,nPlantsA,meanA,varA,
                             plantsB,nPlantsB,meanB,varB,
                             SSbetween,SSwithin,containNA),ncol = 15)
    colnames(resultMatrix) <- c("Marker", "ANOVA allowed","ANOVA P-value",
                                "F value","plants A","n A","mean A","var.A",
                                "plants B", "n B", "mean B","var.B", 
                                "SS between groups","SS within groups",
                                "Excluded plants")
    }
  } else {
    if(pValue<0.05){
    resultMatrix <- rbind(resultMatrix, 
                        c(marker,aovAllowed,pValue,fValue,
                          plantsA,nPlantsA,meanA,varA,
                          plantsB,nPlantsB,meanB,varB,
                          SSbetween,SSwithin,containNA))
    }
  }
}
write.xlsx(resultMatrix,"resultTable.xlsx",row.names = F)         # Save the result matrix in a xlsx file without rownumbers