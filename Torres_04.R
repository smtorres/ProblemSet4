###PROBLEM SET 4
###PROBLEM SET 4
##Creating the directory
#Get the name of the file
rm(list=ls(all.names=TRUE))
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



