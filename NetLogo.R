###PROBLEM SET 4
##Creating the directory
dir.create("Thisisatestfolder")


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