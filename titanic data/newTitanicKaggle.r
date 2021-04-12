#add file into train 

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE)

#add file into test

titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE)

#checking first 6 value of the data

head(titanic.train)
head(titanic.test)

str(titanic.test)   #displaying structure of the data

#median of the age where na is removed

median(titanic.train$Age, na.rm = T)
median(titanic.test$Age, na.rm = T)

#adding a new column in both dataframes

titanic.train$IsTrainSet <- T
titanic.test$IsTrainSet <- F

#calling names of cloumns
names(titanic.train)
names(titanic.test)

#there is no column such as survived in test so we have to add one

titanic.test$Survived <- NA

#combine two dataset into one

titanic.all <- rbind(titanic.train,titanic.test)

table(titanic.all$IsTrainSet)  #tells categorical data with its frequency
table(titanic.all$Embarked)

#cleaning missing values from Embarked

titanic.all[titanic.all$Embarked=='',"Embarked"] <- 'Q'
table(titanic.all$Embarked)

table(is.na(titanic.all$Age))

#cleaning missing values from age

outline <- boxplot(titanic.all$Age)$stat[5]
boxplot(titanic.all$Age)$out
filter <- titanic.all$Age < 66
titanic.all[filter,]
str(titanic.all)
Age.model <- lm(
  formula = Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, 
  data = titanic.all[filter,]
)
library(car)

table(is.na(titanic.all$Age))
age.row <- titanic.all[
  is.na(titanic.all$Age),
  c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")
]
Age.predictions <- predict(Age.model,age.row)

titanic.all[is.na(titanic.all$Age), "Age"] <- Age.predictions
boxplot(titanic.all$Age)
quantile(titanic.all$Age,seq(0,1,0.02))

#catergorical casting of data

str(titanic.all)
titanic.all$Pclass <- as.factor(titanic.all$Pclass)
titanic.all$Sex <-as.factor(titanic.all$Sex)
titanic.all$Embarked <- as.factor(titanic.all$Embarked)

#split train and test datasets

titanic.train <- titanic.all[titanic.all$IsTrainSet==T,]
titanic.test <- titanic.all[titanic.all$IsTrainSet==F,]

titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)

model <- "Survived~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked "
model.formula <- as.formula(model)

par(mfrow=c(1,1))

library(randomForest)
titanic.model <- randomForest(formula = model.formula, data = titanic.train)

survived <- predict(titanic.model,titanic.test)
plot(titanic.model)

#titanic model prediction output
PassengerId <- titanic.test$PassengerId
output <- as.data.frame(PassengerId)
output$Survived <- survived

write.csv(output, file = "new_predictions.csv")
