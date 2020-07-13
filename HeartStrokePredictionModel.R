### Health insurance company wants to know if participant is likely to have a stroke, before the company offer them the quotes
### predic if patient will have stroke
options(warn = -1)
#Libraries
library(plyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(randomForest)



#read data set
originalData <- read.csv("stroke.csv",na.strings=c("","NA"))

#Check the dataset
head(originalData)
#Talk about the overall data
summary(originalData) # There 13292 NA values for smoking_status and 1462 NA's for bmi


dataExp <- na.omit(originalData)

#delete ID
dataExp$id <- NULL

#Convert gender, ever_married, work type, residence_type, smoking_status to numeric

dataExp$gender <- ifelse(dataExp$gender == "Male",1,0)
dataExp$ever_married <- ifelse(dataExp$ever_married == "Yes",1,0)

#Check unique work_type
unique(dataExp$work_type)
dataExp$work_type <- ifelse(dataExp$work_type == "children",1,ifelse(dataExp$work_type == "Private",2,
                                                                     ifelse(dataExp$work_type == "Never_worked",3,
                                                                     ifelse(dataExp$work_type == "Self-employed",4,
                                                                     ifelse(dataExp$work_type == "Govt_job",5,NA)))))
#Residence_type
dataExp$Residence_type <- ifelse(dataExp$Residence_type == "Rural",1,0)

#smoking status
unique(dataExp$smoking_status)
dataExp$smoking_status <- ifelse(dataExp$smoking_status == "never smoked",1,ifelse(dataExp$smoking_status == "formerly smoked",2,
                                                                            ifelse(dataExp$smoking_status == "smokes",3,NA))) 
#

#Check datatypes
sapply(dataExp,class)

#integer to numeric
dataExp$hypertension <- as.numeric(dataExp$hypertension)
dataExp$heart_disease <- as.numeric(dataExp$heart_disease)
dataExp$stroke <- as.numeric(dataExp$stroke)
dataExp$work_type <- as.numeric(dataExp$work_type)

#Check correlation
cor <- cor(dataExp)
corrplot(cor,method="number", bg = "lightgrey") #There is't any highly correlated values


#Check # of people have had stroke in the original data
table(originalData$stroke)

stroke <- originalData %>%
  filter(stroke == 1)

nonstroke <- originalData %>%
  filter(stroke == 0)
#Check distribution of age and gender of people who have had stroke
mu <- ddply(stroke, "gender", summarise, grp.mean=mean(age))

# Overlaid histograms with means
ageDist <- ggplot(stroke, aes(x=age, fill=gender)) +
  geom_histogram(binwidth=0.5, alpha=.5, position="identity") +
  geom_vline(data=mu, aes(xintercept=grp.mean,  colour=gender),
             linetype="dashed", size=1)

ageDist +scale_color_brewer(palette="Dark2")

#Means are close to each other. refer to the dashed line

# Density plots with means
ageDensity <- ggplot(stroke, aes(x=age, colour=gender)) +
  geom_density() +
  geom_vline(data=mu, aes(xintercept=grp.mean,  colour=gender),
             linetype="dashed", size=1)
ageDensity
#after 60 years old significant increases having a stroke

#avg_glucose_level, this can be request from patient before giving the quote
strokeGlu <- stroke %>%
  select(avg_glucose_level)

nonStrokeGlu <- nonstroke %>%
  select(avg_glucose_level)

strokeGlu$Stroke <- 'Yes'
nonStrokeGlu$Stroke <- 'No'

combined <- rbind(strokeGlu,nonStrokeGlu)

#densityGlu <- ggplot(combined, aes(avg_glucose_level, fill = Stroke)) + geom_density(alpha = 0.2)

#Less than 100 mg/dl sugar level is normal. People who have more than 150 mg/dl more likely to have stroke based on the dataset
ggplot(combined, aes(avg_glucose_level, fill = Stroke)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

####Build random forest
#Why Random FOrest
#It doesnt suffer from the overfitting, 
#The main reason is that it takes the average of all the predictions, which cancels out the biases.
#Not many observations 
sapply(dataExp,class)

factorData <- dataExp

#Convert them to factor since they are categorical
factorData$gender <- as.factor(factorData$gender)
factorData$hypertension <- as.factor(factorData$hypertension)
factorData$heart_disease <- as.factor(factorData$heart_disease)
factorData$ever_married <- as.factor(factorData$ever_married)
factorData$stroke <- as.factor(factorData$stroke)
factorData$Residence_type <- as.factor(factorData$Residence_type)
factorData$smoking_status <- as.factor(factorData$smoking_status)

sapply(factorData,class)

#Split Data 
sampleSize <- floor(0.75 * nrow(factorData))

set.seed(155)

trainIndex <- sample(seq_len(nrow(factorData)),size = sampleSize)

train <- factorData[trainIndex,]
test <- factorData[-trainIndex,]

#RandomForest
modelRf <- randomForest(stroke~., data = train, ntree = 500) # error rate 1.91

predict <- predict(modelRf,newdata = test)

table(predict, test$stroke)

accuracy <- 7143 / nrow(test)

#Lets create a new dataset with equal number of yes and no for stroke
table(factorData$stroke)

onlyStroke <- factorData %>%
  filter(stroke == 1)

withoutStroke <- factorData %>%
  filter(stroke == 0)

#Randomly select nonStroke rows
withoutStroke <- sample_n(withoutStroke,nrow(onlyStroke))

balancedData <- rbind(onlyStroke,withoutStroke)

#Split Data 
sampleSize_2 <- floor(0.75 * nrow(balancedData))

set.seed(155)
trainIndex_2 <- sample(seq_len(nrow(balancedData)),size = sampleSize_2)

train_2 <- balancedData[trainIndex_2,]
test_2 <- balancedData[-trainIndex_2,]

#RandomForest. Chooses 10 variables randomly
modelRf_2 <- randomForest(stroke~., data = train_2, ntree = 500) # error rate 1.91
modelRf_2
#Error rate decreases significantly after 100 trees, Since # of observation is low, having big ntree value doesn't require
#high computational power
plot(modelRf_2)

predict_2 <- predict(modelRf_2,newdata = test_2)

table(predict_2, test_2$stroke)

accuracy_2 <- (98+104) / nrow(test_2)
#So when we have equal number of yes and no for stroke, our model is still better than guessing with 74%

