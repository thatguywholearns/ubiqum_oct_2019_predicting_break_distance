#Import all used libraries
library(readr)
library(ggplot2)

#Import dataset_cars
dataset_cars <- read.csv('./R Tutorial Data Sets/cars.csv', header = TRUE)

#Inspect the the structure, variables and summary statistics of the dataset_cars
names(dataset_cars)
attributes(x = dataset_cars)
str(dataset_cars)
summary(dataset_cars)

#Rename variables
names(dataset_cars) <- c("name_of_car", "speed_of_car", "distance_of_car")

#Generate boxplsot to gain better udnerstanding of distribution of the dependant variable
ggplot(dataset_cars, aes(y = distance_of_car)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)

#We observe one outlier with a distance greater than 100, we remove this outlier from our dataset
#as it will bias the out prediction
outliers <- boxplot(dataset_cars$distance_of_car, plot=FALSE)$out
dataset_cars <- dataset_cars[-which(dataset_cars$distance_of_car %in% outliers),]

#Generate plot in order to see patterns and correlations
plot(dataset_cars$distance_of_car, dataset_cars$speed_of_car)

#We see that there is a quadratic relationship between speed of car and distance of car.
#Transform the data in such a way that the relationship becomes linear. To do so, we need to
#square the independant variale car speed.
dataset_cars$speed_of_car_squared <- dataset_cars$speed_of_car*dataset_cars$speed_of_car

#Check whether transformation was succesful
names(dataset_cars)
str(dataset_cars$speed_of_car_squared)

#Regenerate plots in order to see if transformation worked
plot(dataset_cars$distance_of_car, dataset_cars$speed_of_car_squared)

#Set random seed of the script to 123
set.seed(256)

#Create training set and test set by using a 70/30 split 
train_size <- round(nrow(dataset_cars)*0.70)
test_size <- nrow(dataset_cars) - train_size
training_indices <- sample(seq_len(nrow(dataset_cars)), size = train_size)
training_set <- dataset_cars[training_indices,]
test_set <- dataset_cars[-training_indices,]

#Fit linear model to training data
lm_model1 <- lm(distance_of_car~ speed_of_car_squared, training_set)
lm_model2 <- lm(distance_of_car~ speed_of_car, training_set)

#Summary for both statistics model
output_lm_model1 <- summary(lm_model1)
output_lm_model2 <- summary(lm_model2)

#Model 1 has the greatest R squared

#Store coefficients and equation of final model
lm_model1_coef <- coefficients(lm_model1)
lm_model1_intercept <- lm_model1_coef[1]
lm_model1_coef1 <- intercept <- lm_model1_coef[2]
lm_model1_eq <- paste("estimated model: y = ", round(lm_model1_intercept, 2), " + ", round(lm_model1_coef1, 2), " *x" )

#Make predictions on test set
lm_model1_test <- lm(distance_of_car~ speed_of_car_squared, test_set)
output_lm_model1_test <- summary(lm_model1_test)
print(output_lm_model1_test)
predictions_model1 <- predict(lm_model1, test_set)

#Add predictions and prediction error to dataframe test data and plot predictions against real values
test_set$prediction <- predictions_model1
test_set$error <- test_set$distance_of_car - predictions_model1
test_set$relative_error <- paste(round(((test_set$error / test_set$distance_of_car)*100), digits = 2), "%")
ggplot(test_set, aes(x = speed_of_car_squared, y = distance_of_car)) + geom_point() + geom_abline(intercept = lm_model1_intercept, slope = lm_model1_coef1) + ggtitle(lm_model1_eq)

#Error Analysis
ggplot(test_set, aes(error)) + geom_histogram(binwidth = 1)
print(mean(test_set$error))

