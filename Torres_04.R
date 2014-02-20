###PROBLEM SET 4
###PROBLEM SET 4
##Creating the directory
#Get the name of the file
rm(list=ls(all.names=TRUE))
options(stringsAsFactors=F)
main<-"/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4"
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4")
file<-"NetLogo.csv"
DirNetLogo<- function (file="NetLogo.csv"){
  file.name<-scan(file, what="", nlines=1, sep=".", skip=1)
  file.name<-file.name[1]
  scan(file, what="", nlines=1, sep=" ", skip=2)
  info<-scan(file, what="", nlines=1, sep=" ", skip=2) 
  date<-info[1]
  date<-gsub("/", "_", date[1])
  hour<-info[2]
  hour<- strsplit(hour, ":")
  hour<-paste(hour[[1]][1], "_", hour[[1]][2], sep="")
  name.dir<-paste(file.name,"-", date, "-", hour, sep="")
  dir.create(name.dir)
  setwd(name.dir)
  for (i in 1:3){
    names<-c("Globals", "Turtles", "Plots")
    dir.create(names[i])
  }
}
DirNetLogo("NetLogo.csv")

###Globals

setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4")
globals1<- scan(file="NetLogo.csv", what="", nlines=1, sep=",", skip=8)
globals2<- scan(file="NetLogo.csv", what="", nlines=1, sep=",", skip=9)
globals<-list()
for (i in 1:length(globals1)){
  globals[[i]]<-globals2[i]
}
globals2<-list(globals2)

for (i in 1:length(globals)){
  globals[[i]]<-gsub("\\[|\\]", "", globals[[i]])
}
globals
globals<-strsplit(as.character(globals), " ")
for (i in 1:length(globals)){
  globals[[i]]<-as.numeric(globals[[i]]) 
}
names(globals)<-globals1
dump("globals", file="/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Globals/Globals.R")

## Turtles
#Pull names of Turtles variables
turtles.names <- scan(file="NetLogo.csv", skip=12, n=38, what=" ", sep=",")
#Pull values from Turtles
turtles.data <- scan(file="NetLogo.csv", skip=13, nlines=4786, what=" ", sep=",")
#Organize values in a matrix with n rows
turtles.data<-matrix(turtles.data, nrow=4786, byrow=TRUE)
turtles.data<-turtles.data[,1:38]
colnames(turtles.data)<-turtles.names

#Remove brackets and curly brackets
turtles.data<-data.frame(turtles.data)
for (i in 1:ncol(turtles.data)){
  turtles.data[,i]<-gsub("\\[|\\]", "", turtles.data[,i])
  turtles.data[,i]<-gsub("\\{|\\}", "", turtles.data[,i])
}

#Subsetting
SubSet<- function(x){
  datalist<-list()
  var.names<- c("Districts", "Voters", "Activists", "Parties", "Candidates")
  classif<-NULL
  breed<- unlist(strsplit(as.character(x$breed), " "))
  classif<-data.frame(matrix(breed, ncol=2, byrow=TRUE))
  x<-cbind(x, classif[,2])
  colnames(x)[length(x)]<- "Type"
  datalist[[1]]<-with(x, data.frame(subset(x, Type=="districts")))
  datalist[[2]]<-with(x, data.frame(subset(x, Type=="voters")))
  datalist[[3]]<-with(x, data.frame(subset(x, Type=="activists")))
  datalist[[4]]<-with(x, data.frame(subset(x, Type=="parties")))
  datalist[[5]]<-with(x, data.frame(subset(x, Type=="cands")))
  names(datalist)<-var.names
  return(datalist)
}
datalist<-SubSet(turtles.data)

Exp.List<-function(x){
  #Delete all missing values
  x<-na.omit(x)
  #Unique values
  length.col<-apply(x, 2, FUN=function(x) length(unique(x)))
  unique.cols<-which(length.col==1)
  x<-x[,-c(unique.cols)]
  x<-data.frame(x)
  return(x)
}

data.set1<-Exp.List(datalist[[1]])
data.set2<-Exp.List(datalist[[2]])
data.set3<-Exp.List(datalist[[3]])
data.set4<-Exp.List(datalist[[4]])
data.set5<-Exp.List(datalist[[5]])

datalist<-list(data.set1, data.set2, data.set3, data.set4, data.set5)

###Function to split into vectors
MultiVals<- function(x){
  test.multi<-grep(" ",x)
  if(length(test.multi)>0){
  #Variables with multiple values
  numcol<-length(x)
      num.elem<-sapply(gregexpr(" ", x), length) +1
      num.elem<-sum(num.elem)/length(num.elem)
      vec<-unlist(strsplit(x, " "))
 names.ext<- 1:length(num.elem)    
  for (i in 1:length(num.elem)){
        names.ext[i]<-as.character(paste(colnames(x), "_", i, sep=""))
    }
      mat<-data.frame(matrix(vec, ncol=num.elem, byrow=TRUE))
      colnames(mat)<-names.ext
      x<-mat} else{}
  return(x)
}
data.set1<-data.frame(apply(datalist[[1]], 2, FUN=MultiVals))
data.set2<-data.frame(apply(datalist[[2]], 2, FUN=MultiVals))
data.set3<-data.frame(apply(datalist[[3]], 2, FUN=MultiVals))
data.set4<-data.frame(apply(datalist[[4]], 2, FUN=MultiVals))
data.set5<-data.frame(apply(datalist[[5]], 2, FUN=MultiVals))

datalist<-list(data.set1, data.set2, data.set3, data.set4, data.set5)
names(datalist)<-c("Districts", "Voters", "Activists", "Parties", "Candidates")

#Export datasets
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Turtles")

write.csv(datalist[[1]], "Districts.csv")
write.csv(datalist[[1]], "Voters.csv")
write.csv(datalist[[1]], "Activists.csv")
write.csv(datalist[[1]], "Parties.csv")
write.csv(datalist[[1]], "Candidates.csv")

#Plots
####POSITIONS
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Plots")
dir.plots<-getwd()
for (i in 1:4){
  names<-c("PositionPlot", "WinnersPlot", "PolarizationPlot", "IncumbentPercentagePlot")
  dir.create(names[i])
}
####Dimensions
#Dimension 1
setwd(main)
Dim.1 <- scan(file=file, skip=8546, nlines=169, what=" ", sep=",")
Dim.1.names<-scan(file=file, skip=8545, n=24, what=" ", sep=",")
Dim.1 <- matrix(Dim.1, nrow=169, byrow=TRUE)
Dim.1 <- Dim.1[,-c(25:84)]
Dim.1<-data.frame(Dim.1)
colnames(Dim.1)<-Dim.1.names
index.y<-c(1,2,6,10,14,18,22)
names.y<-matrix(scan(file=file, skip=8544, n=24, what=" ", sep=","), ncol=4, byrow=TRUE)
names.y<-names.y[,1]
for (i in 1: length(names.y)){
  names.y[[i]]<-gsub("\\]|\\[|^\"|\"$", "", names.y[[i]])  
}
Dim.1<-Dim.1[,index.y]
colnames(Dim.1)<- c("x", names.y)
Dim.1<-data.frame(Dim.1)
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Plots/PositionPlot")
write.csv(Dim.1, file="Dim1.csv")

#Dimension 2
setwd(main)
Dim.2 <- scan(file=file, skip=8730, nlines=169, what=" ", sep=",")
Dim.2.names<-scan(file=file, skip=8545, n=24, what=" ", sep=",")
Dim.2 <- matrix(Dim.2, nrow=169, byrow=TRUE)
Dim.2 <- Dim.2[,-c(25:84)]
Dim.2<-data.frame(Dim.2)
colnames(Dim.2)<-Dim.2.names
index.y<-c(1,2,6,10,14,18,22)
names.y<-matrix(scan(file=file, skip=8544, n=24, what=" ", sep=","), ncol=4, byrow=TRUE)
names.y<-names.y[,1]
for (i in 1: length(names.y)){
  names.y[[i]]<-gsub("\\]|\\[|^\"|\"$", "", names.y[[i]])  
}
Dim.2<-Dim.2[,index.y]
colnames(Dim.2)<- c("x", names.y)
Dim.2<-data.frame(Dim.2)
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Plots/PositionPlot")
write.csv(Dim.2, file="Dim2.csv")

#Dimension 3
setwd(main)
Dim.3 <- scan(file=file, skip=8914, nlines=169, what=" ", sep=",")
Dim.3.names<-scan(file=file, skip=8545, n=24, what=" ", sep=",")
Dim.3 <- matrix(Dim.3, nrow=169, byrow=TRUE)
Dim.3 <- Dim.3[,-c(25:84)]
Dim.3<-data.frame(Dim.3)
colnames(Dim.3)<-Dim.3.names
index.y<-c(1,2,6,10,14,18,22)
names.y<-matrix(scan(file=file, skip=8544, n=24, what=" ", sep=","), ncol=4, byrow=TRUE)
names.y<-names.y[,1]
for (i in 1: length(names.y)){
  names.y[[i]]<-gsub("\\]|\\[|^\"|\"$", "", names.y[[i]])  
}
Dim.3<-Dim.3[,index.y]
colnames(Dim.3)<- c("x", names.y)
Dim.3<-data.frame(Dim.3)
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Plots/PositionPlot")
write.csv(Dim.3, file="Dim3.csv")



#Plot, dimension 1
par(mfrow=c(3,3), mar=c(1.2,4,1.5,1.2))
plot(Dim.1$x,Dim.1$Red, col="red", main="Incumbents (D1)", 
ylim=c(-6, 6), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.1$x,Dim.1$Blue, col="blue", pch=18)
legend(1,-2, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.5)
plot(Dim.1$x,Dim.1$RedActivists, col="red", main="Activists (D1)", 
     ylim=c(-6, 6), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.1$x,Dim.1$BlueActivists, col="blue", pch=18)
legend(50,-4, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.4)
plot(Dim.1$x,Dim.1$RedVoters, col="red", main="Voters (D1)", 
     ylim=c(-6, 6), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.1$x,Dim.1$BlueVoters, col="blue", pch=18)
legend(1,-2, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.5)
#Plot, dimension 2
plot(Dim.2$x,Dim.2$Red, col="red", main="Incumbents (D2)", 
     ylim=c(-10, 10), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.2$x,Dim.2$Blue, col="blue", pch=18)
legend(1,-2, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.5)
plot(Dim.2$x,Dim.2$RedActivists, col="red", main="Activists (D2)", 
     ylim=c(-12, 12), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.2$x,Dim.2$BlueActivists, col="blue", pch=18)
legend(50,-4, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.4)
plot(Dim.2$x,Dim.2$RedVoters, col="red", main="Voters (D2)", 
     ylim=c(-6, 6), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.2$x,Dim.2$BlueVoters, col="blue", pch=18)
legend(1,-2, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.5)
#Plot, dimension 3
plot(Dim.3$x,Dim.3$Red, col="red", main="Incumbents (D3)", 
     ylim=c(-10, 10), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.3$x,Dim.3$Blue, col="blue", pch=18)
legend(1,-2, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.5)
plot(Dim.3$x,Dim.3$RedActivists, col="red", main="Activists (D3)", 
     ylim=c(-12, 12), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.3$x,Dim.3$BlueActivists, col="blue", pch=18)
legend(50,-4, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.4)
plot(Dim.3$x,Dim.3$RedVoters, col="red", main="Voters (D3)", 
     ylim=c(-6, 6), xlab="Simulation", ylab="Mean Position", pch=18)
points(Dim.2$x,Dim.2$BlueVoters, col="blue", pch=18)
legend(1,-2, legend= c("Blue", "Red"), pch=c(18,18), col=c("blue", "red"), cex=0.5)
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Plots/PositionPlot")
dev.print(device=pdf, "PositionsPlots.pdf")


###WINNERS
setwd(main)
Win <- scan(file=file, skip=9140, nlines=169, what=" ", sep=",")
Win <- matrix(Win, nrow=169, byrow=T)
Win<-Exp.List(Win)
Win<-data.frame(apply(Win, 2, FUN=MultiVals))
Win<-Win[,c(1,2,5)]
names(Win)<-c("Time", "Blue", "Red")
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Plots/WinnersPlot")
write.csv(Win, file="Winners.csv")
rest<-100-(as.numeric(Win$Blue)+as.numeric(Win$Red))
WinDem<-rbind(Win$Blue, rest, Win$Red)

#Plot Winners
barplot(WinDem,beside=FALSE,names=Win$Time, legend.text=c("Blue", "", "Red"),
        args.legend = list(x = "topright"),  ylim=c(0,100), 
        main="Winners per party", ylab="Time period", xlab="%", 
        col=c("blue", "white", "red"))
dev.print(device=pdf, "Winners.pdf")

####POLARIZATION
setwd(main)
Polarization <- scan(file=file, skip=9321, nlines=169, what=" ", sep=",")
Polarization <- matrix(Polarization, nrow=169, byrow=TRUE)
Polarization<-Exp.List(Polarization)
Polarization<-data.frame(apply(Polarization, 2, FUN=MultiVals))
Polarization<-Polarization[,c(1,2,4,6)]
names(Polarization)<-c("Time", "Total", "Voters", "Activists")
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4/4JobTalk3-10_05_2010-19_42/Plots/PolarizationPlot")
write.csv(Polarization, file="Polarization.csv")

library(scatterplot3d)
attach(Polarization)
scatterplot3d(Total,Voters,Activists, main="Polarization", highlight.3d=TRUE, 
              type="p",
              xlab="Candidates")
dev.print(device=pdf, "PolarizationPlot1.pdf")
plot(density(as.numeric(Total)), xlim=c(-2,10), ylim=c(0,0.45), 
     main="Density Polarization", xlab="Euclidian Distance between parties")
polygon(density(as.numeric(Total)), col="pink", border="pink")
par(new=TRUE)
plot(density(as.numeric(Voters)),xlim=c(-2,10), ylim=c(0,0.45), axes=FALSE,
     ylab="", xlab="", main="")
polygon(density(as.numeric(Voters)), col="lightgreen", border="lightgreen")
par(new=TRUE)
plot(density(as.numeric(Activists)),xlim=c(-2,10), ylim=c(0,0.45), axes=FALSE,
     ylab="", xlab="", main="")
polygon(density(as.numeric(Activists)), col="paleturquoise", border="paleturquoise")
legend(-1, .45, legend=c("Candidates", "Voters", "Activists"), pch=c(15,15,15),
       col=c("pink", "lightgreen", "paleturquoise"))
detach(Polarization)
dev.print(device=pdf, "PolarizationPlot2.pdf")
