######################
#Importing Data
######################
data<-read.csv("wpbc.data",header = FALSE)
names(data)<-c('id',"outcome","time",
               "radiusM","textureM","perimeterM","areaM","smoothnessM","compactnessM","concavityM","concavePointsM","symmetryM","fractalDimensionM",
               "radiusSE","textureSE","perimeterSE","areaSE","smoothnessSE","compactnessSE","concavitySE","concavePointsSE","symmetrySE","fractalDimensionSE",
               "radiusW","textureW","perimeterW","areaW","smoothnessW","compactnessW","concavityW","concavePointsW","symmetryW","fractalDimensionW",
               "tumorSize","lymphNodeStatus")
header<-names(data)
View(data)
dim(data)
######################
#Training Data
######################
set.seed(10)
#Assisning training data set size as 75% of total data
training_size<-round(dim(data)[1]*.75)
training_size
#Creating a random index sample 
trainingIndex<-sample(1:dim(data)[1], size = training_size,replace = FALSE)
trainingIndex
#Assigning the training set
training<-data[trainingIndex,]
View(training)
dim(training)
######################
#Test Data
######################
test<-data[-trainingIndex,]
dim(test)
View(test)
