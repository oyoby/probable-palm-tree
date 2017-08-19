##Set Working Directory
setwd("C:/Users/AMMA/Desktop/Data Science/Kaggle/Titanic Competition")

##Loads csv files to Environment
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

##Creates "IsTrainSet" column in both sets
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#Creates Column "Survived" filled with NA values in the test set
titanic.test$Survived <- NA


##Clean Tables
#natural inner join the two tables
titanic.full <-rbind(titanic.train, titanic.test)

#Adds values without tag to the tag with more values "S"
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'  

#Replaces NA Age values with the current median age
age.median = median(titanic.full$Age, na.rm=TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median


#Replaces NA Fare values with the current median Fare
fare.median = median(titanic.full$Fare, na.rm=TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median


##Categorical Casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

##Split sets back into train and test
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

##Build a predictive model
titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree=500, mtry=3, nodesize=0.01 * nrow(titanic.test))
#skipping cross validation etc. This is a simple example.


features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

Survived <- predict(titanic.model, newdata = titanic.test)
