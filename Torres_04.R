###PROBLEM SET 4
###PROBLEM SET 4
##Creating the directory
#Get the name of the file
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 4/ProblemSet4")
FunNetLogo<- function (file="NetLogo.csv"){
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
FunNetLogo("NetLogo.csv")

Globals<-list()
globals1<- scan(file="/Users/macuser/NetLogo.csv", what="" , nlines=1, sep=",", skip=8)
globals2<- scan(file="/Users/macuser/NetLogo.csv", what="", nlines=1, sep=",", skip=9)
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

##
districts<-scan(file="/Users/macuser/NetLogo.csv", what="" , nlines=-1, sep=",", skip=13)
districts
