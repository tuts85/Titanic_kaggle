## Loading datasets

train <- read.csv('train.csv')
test <- read.csv('test.csv')

train$isTrain <- TRUE

test$Survived <- 0
test$isTrain <- FALSE

combined <- rbind(train, test)

head(train)

# Check for missing values in each column
missing_values <- colSums(is.na(train))

# Print out the number of missing values in each column
print(missing_values)

# If you want to see the columns with missing values, you can filter them out
columns_with_missing <- names(missing_values[missing_values > 0])
columns_with_missing

install.packages("ggplot2")
library(ggplot2)

# Create the boxplot with median age
ggplot(train, aes(x = factor(Pclass), y = Age)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "red", 
               position = position_dodge(width = 0.75)) +
  labs(x = "Pclass", y = "Age", title = "Distribution of Age by Pclass")

# Subset the dataset where Pclass is 1 and calculate the mean age
mean_age_pclass_1 <- mean(combined$Age[combined$Pclass == 1], na.rm = TRUE)
mean_age_pclass_2 <- mean(combined$Age[combined$Pclass == 2], na.rm = TRUE)
mean_age_pclass_3 <- mean(combined$Age[combined$Pclass == 3], na.rm = TRUE)

# Print the mean age value
mean_age_pclass_1
mean_age_pclass_2
mean_age_pclass_3

## INputting age function
impute_age <- function(age, pclass) {
  if (is.na(age)) {
    if (pclass == 1) {
      return(39)
    } else if (pclass == 2) {
      return(29)
    } else if (pclass == 3) {
      return(25)
    } else {
      return(NA)  # If Pclass is not 1, 2, or 3, return NA
    }
  } else {
    return(age)  # If age is not missing, return the original age
  }
}

# Impute missing age values based on Pclass
combined$Age <- mapply(impute_age, combined$Age, combined$Pclass)

# Check for blank values in each column of the train dataset
blank_values <- sapply(combined, function(x) any(x == ""))

# Print out the columns with blank values
print(names(blank_values[blank_values == TRUE]))

# Assuming your dataset is named 'train'
# Replace blank values in the 'Embarked' column with 'S'
combined$Embarked[combined$Embarked == ""] <- "S"


table(combined$Embarked)

str(combined)

combined$Survived <- as.numeric(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)
combined$Embarked <- as.factor(combined$Embarked)
combined$Parch <-as.factor(combined$Parch)
combined$SibSp <- as.factor(combined$SibSp)


str(combined)

train.final <- combined[combined$isTrain == TRUE,]
test.final <- combined[combined$isTrain == FALSE,]

train.final <- subset(train.final, select = -c(isTrain, Name, Cabin, Ticket))
test.final <- subset(test.final, select = -c(isTrain, Survived, Name, Cabin, Ticket))

lm.model <- lm(Survived ~ . - PassengerId, data = train.final)

summary(lm.model)

prediction <- predict(lm.model, test.final)

str(test.final)

prediction <- round(prediction)
print(rounded_value) 

lm.pred.submission <- cbind(test.final$PassengerId, prediction)

lm.pred.submission <- as.data.frame(lm.pred.submission)

# Assuming your dataframe is named 'df' and you want to change the name of a column from 'old_name' to 'new_name'
colnames(lm.pred.submission)[colnames(lm.pred.submission) == "V1"] <- "PassengerId"
colnames(lm.pred.submission)[colnames(lm.pred.submission) == "prediction"] <- "Survived"

## Exporting the linear regression results
write.csv(lm.pred.submission, file = "linear_regression_results.csv", row.names = FALSE)


#### Converting to Factors and dividing into train and test

combined$Survived <- as.numeric(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)
combined$Embarked <- as.factor(combined$Embarked)
combined$Parch <-as.factor(combined$Parch)
combined$SibSp <- as.factor(combined$SibSp)


str(combined)

train.final <- combined[combined$isTrain == TRUE,]
test.final <- combined[combined$isTrain == FALSE,]

train.final <- subset(train.final, select = -c(isTrain, Name, Cabin, Ticket))
test.final <- subset(test.final, select = -c(isTrain, Survived, Name, Cabin, Ticket))

## glm

train.final$Survived <- as.factor(train.final$Survived)

glm.model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,  family = binomial(link = "logit"), data = train.final)

summary(glm.model)

glm.prediction <- predict(glm.model, test.final, type = 'response')

fitted.results <- ifelse(glm.prediction>0.5, 1, 0)

glm.prediction <- cbind(test.final$PassengerId, fitted.results)

glm.prediction <- as.data.frame(glm.prediction)

# Assuming your dataframe is named 'df' and you want to change the name of a column from 'old_name' to 'new_name'
colnames(glm.prediction)[colnames(glm.prediction) == "V1"] <- "PassengerId"
colnames(glm.prediction)[colnames(glm.prediction) == "fitted.results"] <- "Survived"

## Exporting the logistic regression results
write.csv(glm.prediction, file = "logistic_regression_results.csv", row.names = FALSE)

## Decision Tree
install.packages("rpart")
library(rpart)

tree.model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, method = 'class', data = train.final)

printcp(tree.model)

tree.prediction <- predict(tree.model, newdata =  test.final)

tree.prediction

tree.fitted.results <- ifelse(tree.prediction[, 2] > tree.prediction[, 1], 1, 0)

tree.prediction <- as.data.frame(cbind(test.final$PassengerId, tree.fitted.results))
colnames(tree.prediction)[colnames(tree.prediction) == "V1"] <- "PassengerId"
colnames(tree.prediction)[colnames(tree.prediction) == "tree.fitted.results"] <- "Survived"

## Exporting the decision tree results
write.csv(tree.prediction, file = "decision_tree_results.csv", row.names = FALSE)


## Random Forest
install.packages("randomForest")
library(randomForest)

rf.model <- randomForest(Survived ~ . -PassengerId, data = train.final)

rf.prediction <- predict(rf.model, newdata = test.final)

test.final$Survived <- rf.prediction

df.rf.submission <- test.final[,c("PassengerId", "Survived")]

## Exporting the random forest results
write.csv(df.rf.submission, file = "random_forest_results.csv", row.names = FALSE)
