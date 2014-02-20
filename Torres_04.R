###PROBLEM SET 4
###PROBLEM SET 4
##Creating the directory
#Get the name of the file
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4")
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
  #Record the name of the "grouping" variable
  name<-x$Type[1]
  #Delete the column of classification I created
  x<-x[,1:38]
  #Delete all missing values
  x<-na.omit(x)
  x<-apply(x, 2, function(x))
  #Unique values
  length.col<-apply(x, 2, FUN=function(x) length(unique(x)))
  unique.cols<-which(length.col==1)
  x<-x[,-c(unique.cols)]
  #Export dataset
  write.csv(x, paste(name, ".csv", sep=""))
}
Exp.List(x)
apply(datalist, 3, E)


