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
nonFactor<-training[,1:3]
View(nonFactor)
factors<-data.matrix(training[,4:35], rownames.force = NA)
View(factors)
factorMean<-colMeans(factors)
factorMean
factorSd<-colSds(factors) #matrixStats Package was used for the colSds function
factorSd
factors<-scale(factors,center = TRUE,scale = TRUE)
View(factors)
training<-cbind(factors,nonFactor[,2]=='R')
View(training)

######################
#Test Data
######################
test<-data[-trainingIndex,]
View(test)
testNonFactors<-test[,1:3]
testFactors<-test[,4:35]
View(testFactors)
(testFactors[1,]-factorMean)
for(i in 1:dim(test)[2]){
  testFactors[i,]<-(testFactors[i,]-factorMean)/factorSd
}

View(testFactors)

dim(test)
View(test)
