######################
#Importing Data
######################
data<-read.csv("wdbc.data",header = FALSE)
names(data)<-c('id',"diagnosis",
               "radiusM","textureM","perimeterM","areaM","smoothnessM","compactnessM","concavityM","concavePointsM","symmetryM","fractalDimensionM",
               "radiusSE","textureSE","perimeterSE","areaSE","smoothnessSE","compactnessSE","concavitySE","concavePointsSE","symmetrySE","fractalDimensionSE",
               "radiusW","textureW","perimeterW","areaW","smoothnessW","compactnessW","concavityW","concavePointsW","symmetryW","fractalDimensionW")

# Encoding categorical data
data$diagnosis = factor(data$diagnosis,
                    levels = c('M', 'B'),
                    labels = c(1, 0))
id<-data[,1]
data<-data[,-1]        #taking the patient ids aways for modelling purpose
header<-names(data)
dim(data)

######################
#Training Data
######################
set.seed(101)

#Assisning training data set size as 75% of total data
training_size<-round(dim(data)[1]*.75)
training_size

#Creating a random index sample 
trainingIndex<-sample(1:dim(data)[1], size = training_size,replace = FALSE)

#Assigning the training set
training<-data[trainingIndex,]
dim(training)

#separating nonfactors before scaling
trainingDiagnosis<-training[,1]
trainingFactors<-data.matrix(training[,2:31], rownames.force = NA)


#preProcess
#install.packages('caret')
library(caret)
prep<-preProcess(trainingFactors,method=c('center','scale'))
summary(prep)
trainingFactors<-predict(prep,trainingFactors)
training<-cbind.data.frame(trainingFactors,trainingDiagnosis)
View(training)


######################
#Test Data
######################
test<-data[-trainingIndex,]
View(test)
dim(test)
sapply(test,class)
testDiagnosis<-test[,1]
testFactors<-data.matrix(test[,2:31], rownames.force = NA)
class(testFactors)

test<-predict(prep,testFactors)
test<-cbind.data.frame(testFactors,testDiagnosis)

dim(test)
View(test)
