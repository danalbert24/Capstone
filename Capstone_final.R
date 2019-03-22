#useful libraries
library(tidyverse)
library(dslabs)
library(caret)
library(dplyr)
library(rpart)
library(randomForest)

#Import Data
#Data from https://www.kaggle.com/spscientist/students-performance-in-exams
data_path <- "~/Capstone/StudentsPerformance.csv"
data_original <- read.csv(data_path)

#split it into test and train sets
set.seed(24)
test_index <- createDataPartition(y = data_original$math.score, times = 1, p = .2, list = FALSE)
train_set <- data_original[-test_index,]
test_set <- data_original[test_index,]

#Further split our training set so that we have something to test on before our final test
further_test_index <- createDataPartition(train_set$math.score, times = 1, p = .2, list = FALSE)
further_test_set <- train_set[further_test_index,]
data <- train_set[-further_test_index,]

#find some averages
mu_math <- mean(data$math.score)
mu_reading <- mean(data$reading.score)
mu_writing <- mean(data$writing.score)

averages <- c(mu_math, mu_reading, mu_writing)
#I'm going to use the averages data frame ahead, so I need each row to be those three averages
averages <- t(data.frame(averages, averages, averages, averages, averages, averages))

#look at the individual factors and see if any one has a particularly large impace
#I'll do this by grouping by each factor and recalculating the averages and comparing to the overall average
gender_averages <- data %>% group_by(gender) %>% summarize(math.gender = mean(math.score), reading.gender = mean(reading.score), writing.gender = mean(writing.score))
gender_labels <- gender_averages[,1]
gender_delta <- gender_averages[,2:4] - averages[1:2,]
gender_delta <- bind_cols(gender_labels, gender_delta)

race_averages <- data %>% group_by(race.ethnicity) %>% summarize(math.race = mean(math.score), reading.race = mean(reading.score), writing.race = mean(writing.score))
race_labels <- race_averages[,1]
race_delta <- race_averages[,2:4] - averages[1:5,]
race_delta <- bind_cols(race_labels, race_delta)


parent_averages <- data %>% group_by(parental.level.of.education) %>% summarize(math.parent = mean(math.score), reading.parent = mean(reading.score), writing.parent = mean(writing.score))
parent_labels <- parent_averages[,1]
parent_delta <- parent_averages[,2:4] - averages[1:6,]
parent_delta <- bind_cols(parent_labels, parent_delta)


lunch_averages <- data %>% group_by(lunch) %>% summarize(math.lunch = mean(math.score), reading.lunch = mean(reading.score), writing.lunch = mean(writing.score))
lunch_labels <-lunch_averages[,1]
lunch_delta <- lunch_averages[,2:4] - averages[1:2,]
lunch_delta <- bind_cols(lunch_labels, lunch_delta)


prep_averages <- data %>% group_by(test.preparation.course) %>% summarize(math.prep = mean(math.score), reading.prep = mean(reading.score), writing.prep = mean(writing.score))
prep_labels <-prep_averages[,1]
prep_delta <- prep_averages[,2:4] - averages[1:2,]
prep_delta <- bind_cols(prep_labels, prep_delta)


#Create a table with the results from various methods
#the first method will be simply predicting the average. Hopefully we'll improve upon it.
results <- data.frame(method = "Just the Average", math_rmse = RMSE(mu_math, further_test_set$math.score), reading_rmse = RMSE(mu_reading, further_test_set$reading.score), writing_rmse = RMSE(mu_writing, further_test_set$writing.score))

#Now that we see how much higher or lower than average each factor is, we can try to use that to predict.
#I'll create additional columns with these values and add them to our further test set
further_test_set <- further_test_set %>% left_join(gender_delta, by = 'gender') %>% 
  left_join(race_delta, by = 'race.ethnicity') %>%
  left_join(parent_delta, by= 'parental.level.of.education') %>%
  left_join(lunch_delta, by = 'lunch') %>%
  left_join(prep_delta, by = 'test.preparation.course')
#now I can calculate my predictions based on the averages of the factors
math_pred <- further_test_set %>%  mutate(pred = mu_math + math.gender + math.parent + math.race + math.lunch + math.prep) %>% .$pred
reading_pred <- further_test_set %>% mutate(pred = mu_reading + reading.gender + reading.race + reading.parent + reading.lunch + reading.prep) %>% .$pred
writing_pred <- further_test_set %>% mutate(pred = mu_writing + writing.gender + writing.race + writing.parent + writing.lunch + writing.prep) %>% .$pred


#We'll use RMSE as a metric
RMSE <- function(x, y){sqrt(mean((x-y)^2))}

#next, I'll add the results of the factor averages method
results <- bind_rows(results, data.frame(method = "Factor Averages", math_rmse = RMSE(math_pred, further_test_set$math.score), reading_rmse = RMSE(reading_pred, further_test_set$reading.score), writing_rmse = RMSE(writing_pred, further_test_set$writing.score)))

#Let's try to improve the results using Regression Trees and Random Forests
#I'll start by splitting up the topics
#I need to remove reading and writing for math, and so on
just_math <- select(data, -c(reading.score, writing.score))
just_reading <- select(data, -c(math.score, writing.score))
just_writing <- select(data, -c(math.score, reading.score))

#Now I'll use rpart to make predictions
fit_math <- rpart(math.score ~., data = just_math)
tree_math <- predict(fit_math, further_test_set)

fit_reading <- rpart(reading.score ~., data = just_reading)
tree_reading <- predict(fit_reading, further_test_set)

fit_writing <- rpart(writing.score ~., data = just_writing)
tree_writing <- predict(fit_writing, further_test_set)

tree_rmse <- c(RMSE(tree_math, further_test_set$math.score), RMSE(tree_reading, further_test_set$reading.score), RMSE(tree_writing, further_test_set$writing.score))
results <- bind_rows(results, data.frame(method = "Tree", math_rmse = tree_rmse[1], 
                                         reading_rmse = tree_rmse[2], 
                                         writing_rmse = tree_rmse[3]))

#so we see that this tree wasn't quite as good as the factor averages, but still better than just the averages
#maybe a random forest will improve things? Or maybe we could tweak the parameters with the tree?
rf_fit_math <- randomForest(math.score ~ ., data = just_math)
rf_pred_math <- predict(rf_fit_math, further_test_set)
rf_fit_reading <- randomForest(reading.score ~., data = just_reading)
rf_pred_reading <- predict(rf_fit_reading, further_test_set)
rf_fit_writing <- randomForest(writing.score ~., data = just_writing)
rf_pred_writing <- predict(rf_fit_writing, further_test_set)

results <- bind_rows(results, data.frame(method = "Random Forest", 
                                         math_rmse = RMSE(rf_pred_math, further_test_set$math.score), 
                                         reading_rmse = RMSE(rf_pred_reading, further_test_set$reading.score), 
                                         writing_rmse = RMSE(rf_pred_writing, further_test_set$writing.score)))

results

#Looking at the results, we can see that the factor averages, so let's redo
#those while using the full training set to train and test it on the actual test set this time.
######################################################################################################################################################
#I asked a question to the discussion board and never got a satisfactory answer, so I'm redoing this rather than
#just renaming things from earlier. I'm sorry that it adds so much code.
######################################################################################################################################################
mu_math2 <- mean(train_set$math.score)
mu_reading2 <- mean(train_set$reading.score)
mu_writing2 <- mean(train_set$writing.score)

averages2 <- c(mu_math2, mu_reading2, mu_writing2)
#I'm going to use the averages data frame ahead, so I need each row to be those three averages
averages2 <- t(data.frame(averages2, averages2, averages2, averages2, averages2, averages2))

#Computing the factor averages for use, as above
gender_averages2 <- train_set %>% group_by(gender) %>% summarize(math.gender = mean(math.score), reading.gender = mean(reading.score), writing.gender = mean(writing.score))
gender_delta2 <- gender_averages2[,2:4] - averages[1:2,]
gender_delta2 <- bind_cols(gender_labels, gender_delta2)

race_averages2 <- train_set %>% group_by(race.ethnicity) %>% summarize(math.race = mean(math.score), reading.race = mean(reading.score), writing.race = mean(writing.score))
race_delta2 <- race_averages2[,2:4] - averages[1:5,]
race_delta2 <- bind_cols(race_labels, race_delta2)


parent_averages2 <- train_set %>% group_by(parental.level.of.education) %>% summarize(math.parent = mean(math.score), reading.parent = mean(reading.score), writing.parent = mean(writing.score))
parent_delta2 <- parent_averages2[,2:4] - averages[1:6,]
parent_delta2 <- bind_cols(parent_labels, parent_delta2)


lunch_averages2 <- train_set %>% group_by(lunch) %>% summarize(math.lunch = mean(math.score), reading.lunch = mean(reading.score), writing.lunch = mean(writing.score))
lunch_delta2 <- lunch_averages2[,2:4] - averages[1:2,]
lunch_delta2 <- bind_cols(lunch_labels, lunch_delta2)


prep_averages2 <- train_set %>% group_by(test.preparation.course) %>% summarize(math.prep = mean(math.score), reading.prep = mean(reading.score), writing.prep = mean(writing.score))
prep_delta2 <- prep_averages2[,2:4] - averages[1:2,]
prep_delta2 <- bind_cols(prep_labels, prep_delta2)


#add these columns onto our test_set and make our predictions!
test_set <- test_set %>% left_join(gender_delta2, by = 'gender') %>% 
  left_join(race_delta2, by = 'race.ethnicity') %>%
  left_join(parent_delta2, by= 'parental.level.of.education') %>%
  left_join(lunch_delta2, by = 'lunch') %>%
  left_join(prep_delta2, by = 'test.preparation.course')

#now I can calculate my predictions based on the averages of the factors
math_pred2 <- test_set %>%  mutate(pred = mu_math2 + math.gender + math.parent + math.race + math.lunch + math.prep) %>% .$pred
reading_pred2 <- test_set %>% mutate(pred = mu_reading2 + reading.gender + reading.race + reading.parent + reading.lunch + reading.prep) %>% .$pred
writing_pred2 <- test_set %>% mutate(pred = mu_writing2 + writing.gender + writing.race + writing.parent + writing.lunch + writing.prep) %>% .$pred


final_results <- data.frame(method = "Factor Averages", 
                            math_rmse = RMSE(math_pred2, test_set$math.score), 
                            reading_rmse = RMSE(reading_pred2, test_set$reading.score), 
                            writing_rmse = RMSE(writing_pred2, test_set$writing.score))

final_results
