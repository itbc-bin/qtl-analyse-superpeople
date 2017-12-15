Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_131\\jre")
install.packages("rJava")
library(rJava)
install.packages("xlsx")
library(xlsx)

abData <- read.csv("CvixLerC9Rready3.csv",sep=",",header=FALSE) # Data met de aanwezige markers ophalen
abTable <- t(abData)                                            # Marker data opslaan in een tabel
colnames(abTable) <- abTable[1,]    # eerste rij tot collom headers maken 
abTable <- abTable[-1,]             # eerste rij verwijderen 

fenoData <- read.csv("CvixLerC9.qua~",sep="\t",header=FALSE) # fenotypedata ophalen
row.names(abTable) <- fenoData[,1]
abTable <- cbind(abTable, fenoData[2])                     # deze data opslaan in de tabel
colnames(abTable)[ncol(abTable)] <- "Fenotype"     #colommen een naam geven
fenoData <- transform(abTable, Fenotype = as.numeric(levels(Fenotype))[Fenotype])

#View(abTable)

abTable[154,ncol(abTable)] <- NA # onbekende waarde vervangen door NA
abTable[abTable=="-"] <- NA

for(naam in colnames(abTable[1:ncol(abTable)-1])){
  test <- abTable[,c(naam,"Fenotype")]
  test <- na.exclude(test)
  test <- suppressWarnings(transform(test, Fenotype = as.numeric(levels(Fenotype))[Fenotype]))
  #View(test)
  
  attach(test)
  
  result <- aov(formula = test[,"Fenotype"]~test[,1])
  print(summary(result)[[1]][["Pr(>F)"]][[1]])
  detach(test)
}

