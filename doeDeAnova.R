a <- read.csv("~/QTL/CvixLerC9Rready.csv",sep=",",header=FALSE)
b <- t(a)
colnames(b) <- b[1,]
b <- b[-1,]

c <- read.csv("~/QTL/CvixLerC9.qua")

d <- cbind("anova")

#for(i in ncol(b)){
#  d <- rbind(d, aov())
#}
