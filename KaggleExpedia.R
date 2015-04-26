#For my first attempt, I will be using the caret package to make my prediction.
#I decided to use 8 of the variables that seemed most likely to make a difference
#in if a person might or might not book a hotel room.

#Bring in train data and load caret
Expedia = read.csv("train.csv")
Expedia1 = Expedia[c(5,6,9,10,11,15,16,23,51)]

install.packages("caret")
library(caret)

#Use data splitting
DataSplit1 <- createDataPartition(y=Expedia1$booking_bool, p=0.75, list=FALSE)

train1 <- Expedia1[DataSplit1,]
test1 <- Expedia1[-DataSplit1]

Ex1 <- train(booking_bool~visitor_hist_starrating+visitor_hist_adr_usd+prop_starrating+
               prop_review_score+prop_brand_bool+price_usd+promotion_flag+
               srch_saturday_night_bool, data = Expedia1, method = "glm")
summary(Ex1)

#  Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)               4.887e-03  1.338e-03   3.652 0.000260 ***
#  visitor_hist_starrating   3.048e-03  8.605e-04   3.541 0.000398 ***
#  visitor_hist_adr_usd     -8.334e-06  1.424e-05  -0.585 0.558438    
#  prop_starrating           1.586e-03  3.058e-04   5.186 2.15e-07 ***
#  prop_review_score         3.039e-03  3.094e-04   9.824  < 2e-16 ***
#  prop_brand_bool           3.083e-03  6.444e-04   4.785 1.71e-06 ***
#  price_usd                -2.236e-07  2.200e-07  -1.016 0.309549    
#  promotion_flag            1.325e-02  7.456e-04  17.772  < 2e-16 ***
#  srch_saturday_night_bool  2.112e-03  6.024e-04   3.506 0.000456 ***

#From this data, I can see that visitor_hist_adr_usd and price_usd have high p-values 
#that make them not very reliable.

#Load data to be tested
Expedia_Test = read.csv("test.csv")

#Prepare ID
Expedia_ID <- paste(Expedia_Test$srch_id, Expedia_Test$prop_id, sep = "-", collapse = NULL)

#Predict
Expedia_Predictions1 <- predict(Ex1, newdata = Expedia_Test)

#Output predictions
Expedia_Info1 <- data.frame("srch-prop_id" = Expedia_ID, "booking_bool" = Expedia_Predictions1)
write.csv(Expedia_Info1, "Expedia_Predict1.csv", row.names = FALSE)

#FIRST TRY, 0.57877


#For my second attempt, I am going to use the same variables, but will replace the zeros
#of two of the variables, which represent missing values, with the average of the 
#values given.

#Bring in train data 
Expedia2 = Expedia[c(5,6,9,10,11,15,16,23,51)]

#Install Hmisc to use impute
install.packages("Hmisc")
library(Hmisc)

#Get rid of zeros in history of the visitors star rating and replace with mean of the rest
Expedia2$visitor_hist_starrating[Expedia2$visitor_hist_starrating == 0] <- NA
Expedia2$visitor_hist_starrating <- impute(Expedia2$visitor_hist_starrating, mean)

#Get rid of zeros in history of the visitors price per night and replace with mean of the rest
Expedia2$visitor_hist_adr_usd[Expedia2$visitor_hist_adr_usd == 0] <- NA
Expedia2$visitor_hist_adr_usd <- impute(Expedia2$visitor_hist_adr_usd, mean)

#Not getting rid of zeros in the star rating, since not being able to see a review
#should have a negative effect on people, same with review score

#Brand bool, price, promotion flag, and Saturday bool do not have zeros that need to be replaced

#Use data splitting
library(caret)
DataSplit2 <- createDataPartition(y=Expedia2$booking_bool, p=0.75, list=FALSE)

train2 <- Expedia2[DataSplit2,]
test2 <- Expedia2[-DataSplit2]

Ex2 <- train(booking_bool~visitor_hist_starrating+visitor_hist_adr_usd+prop_starrating+
               prop_review_score+prop_brand_bool+price_usd+promotion_flag+
               srch_saturday_night_bool, data = Expedia2, method = "glm")

summary(Ex2)

#  Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)               1.259e-02  6.986e-03   1.802  0.07152 .  
#  visitor_hist_starrating  -1.964e-03  2.318e-03  -0.847  0.39680    
#  visitor_hist_adr_usd     -2.454e-06  1.475e-05  -0.166  0.86791    
#  prop_starrating           1.669e-03  3.063e-04   5.450 5.04e-08 ***
#  prop_review_score         3.007e-03  3.094e-04   9.719  < 2e-16 ***
#  prop_brand_bool           2.970e-03  6.450e-04   4.605 4.13e-06 ***
#  price_usd                -2.285e-07  2.201e-07  -1.038  0.29914    
#  promotion_flag            1.318e-02  7.456e-04  17.671  < 2e-16 ***
#  srch_saturday_night_bool  1.892e-03  6.016e-04   3.145  0.00166 ** 

#Both of the variables with the zeros replaced show large p-values. Without the 
#zeros replaced, visitor_hist_starrating did not have a high p-value. The reason 
#behind this could be that users with no history may be less ikely to book in the end.
#Since I have 5 tries per day, I decided to try this even if it seems less likely to
#be accurate than the first attempt.

#Load data to be tested
Expedia_Test = read.csv("test.csv")

#Prepare ID
Expedia_ID <- paste(Expedia_Test$srch_id, Expedia_Test$prop_id, sep = "-", collapse = NULL)

#Predict
Expedia_Predictions2 <- predict(Ex2, newdata = Expedia_Test)

#Output predictions
Expedia_Info2 <- data.frame("srch-prop_id" = Expedia_ID, "booking_bool" = Expedia_Predictions2)
write.csv(Expedia_Info2, "Expedia_Predict2.csv", row.names = FALSE)

#SECOND TRY, 0.57208


#My third attempt is very similar to my second, with the only difference being that I
#am replacing zeros with the medians instead of the means. I don't expect too much of
#a difference, but they will be better if the data points are skewed.

#Bring in train data
Expedia = read.csv("train.csv")
Expedia3 = Expedia[c(5,6,9,10,11,15,16,23,51)]

#Get rid of zeros in history of the visitors star rating and replace with median of the rest
library(Hmisc)
Expedia3$visitor_hist_starrating[Expedia3$visitor_hist_starrating == 0] <- NA
Expedia3$visitor_hist_starrating <- impute(Expedia3$visitor_hist_starrating, median)

#Get rid of zeros in history of the visitors price per night and replace with median of the rest
Expedia3$visitor_hist_adr_usd[Expedia3$visitor_hist_adr_usd == 0] <- NA
Expedia3$visitor_hist_adr_usd <- impute(Expedia3$visitor_hist_adr_usd, median)

#Not getting rid of zeros in the star rating, since not being able to see a review
#should have a negative effect on people, same with review score

#Brand bool, price, promotion flag, and Saturday bool do not have zeros that need to be replaced

#Use data splitting
library(caret)
DataSplit3 <- createDataPartition(y=Expedia3$booking_bool, p=0.75, list=FALSE)

train3 <- Expedia3[DataSplit3,]
test3 <- Expedia3[-DataSplit3]

Ex3 <- train(booking_bool~visitor_hist_starrating+visitor_hist_adr_usd+prop_starrating+
               prop_review_score+prop_brand_bool+price_usd+promotion_flag+
               srch_saturday_night_bool, data = Expedia3, method = "glm")

summary(Ex3)

#  Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)               2.124e-02  7.094e-03   2.994  0.00276 ** 
#  visitor_hist_starrating  -5.733e-03  2.249e-03  -2.550  0.01078 *  
#  visitor_hist_adr_usd      2.612e-05  1.413e-05   1.849  0.06452 .  
#  prop_starrating           1.684e-03  3.063e-04   5.500 3.79e-08 ***
#  prop_review_score         2.999e-03  3.094e-04   9.695  < 2e-16 ***
#  prop_brand_bool           2.945e-03  6.450e-04   4.566 4.97e-06 ***
#  price_usd                -2.286e-07  2.201e-07  -1.039  0.29891    
#  promotion_flag            1.318e-02  7.456e-04  17.677  < 2e-16 ***
#  srch_saturday_night_bool  1.902e-03  6.016e-04   3.162  0.00157 ** 

#The mean does seem to work better, as it provides much lower p-values for the two
#variables altered, with both being significant at the 0.07 level.

#Load data to be tested
Expedia_Test = read.csv("test.csv")

#Prepare ID
Expedia_ID <- paste(Expedia_Test$srch_id, Expedia_Test$prop_id, sep = "-", collapse = NULL)

#Predict
Expedia_Predictions3 <- predict(Ex3, newdata = Expedia_Test)

#Output predictions
Expedia_Info3 <- data.frame("srch-prop_id" = Expedia_ID, "booking_bool" = Expedia_Predictions3)
write.csv(Expedia_Info3, "Expedia_Predict3.csv", row.names = FALSE)

#THIRD TRY, 0.56645


#My fourth attempt did not work, as it was giving me the same probabilities for everything.
#Decided to omit code.


#For my fifth attempt, I decided to omit variables that were not significant at a 5%
#significance level.

#Bring in train data
Expedia5 = Expedia[c(5,6,9,10,11,15,16,23,51)]


#Use data splitting
library(caret)
DataSplit5 <- createDataPartition(y=Expedia5$booking_bool, p=0.75, list=FALSE)

train5 <- Expedia5[DataSplit5,]
test5 <- Expedia5[-DataSplit5]

#Analyzed in Ex1, Ex2, and Ex3 that the p-values of visitor_hist_adr_usd and price_usd are
#not significant at p=0.05

#Remove variables
Ex5 <- train(booking_bool~visitor_hist_starrating+prop_starrating+
               prop_review_score+prop_brand_bool+promotion_flag+
               srch_saturday_night_bool, data = Expedia5, method = "glm")
summary(Ex5)

#  Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)              0.0048992  0.0013381   3.661 0.000251 ***
#  visitor_hist_starrating  0.0025981  0.0003835   6.775 1.25e-11 ***
#  prop_starrating          0.0015743  0.0003056   5.151 2.59e-07 ***
#  prop_review_score        0.0030351  0.0003093   9.812  < 2e-16 ***
#  prop_brand_bool          0.0030890  0.0006444   4.793 1.64e-06 ***
#  promotion_flag           0.0132540  0.0007456  17.777  < 2e-16 ***
#  srch_saturday_night_bool 0.0021064  0.0006023   3.497 0.000471 ***

#All values now significant at 0.001 significance level

#Load data to be tested
Expedia_Test = read.csv("test.csv")

#Prepare ID
Expedia_ID <- paste(Expedia_Test$srch_id, Expedia_Test$prop_id, sep = "-", collapse = NULL)

#Predict
Expedia_Predictions5 <- predict(Ex5, newdata = Expedia_Test)

#Output predictions
Expedia_Info5 <- data.frame("srch-prop_id" = Expedia_ID, "booking_bool" = Expedia_Predictions5)
write.csv(Expedia_Info5, "Expedia_Predict5.csv", row.names = FALSE)

#FIFTH TRY, 0.57705


#I'm a math major yet counted from 5 to 7, oops.


#For my seventh attempt, I decided to replace the zeros in visitor_hist_adr_usd and replace
#with the median, since that lowered the p-value to approximately 0.065. I decided to 
#also remove price_usd, since that p-value was always high.

#Bring in train data and load caret
Expedia = read.csv("train.csv")
Expedia7 = Expedia[c(5,6,9,10,11,15,16,23,51)]

#Install Hmisc to use impute
install.packages("Hmisc")
library(Hmisc)

#Don't get rid of zeros in history of the visitors star rating since p-value was lower
#when zeros were left in.

#Get rid of zeros in history of the visitors price per night and replace with median of the rest
Expedia7$visitor_hist_adr_usd[Expedia7$visitor_hist_adr_usd == 0] <- NA
Expedia7$visitor_hist_adr_usd <- impute(Expedia7$visitor_hist_adr_usd, median)

#Not getting rid of zeros in the star rating, since not being able to see a review
#should have a negative effect on people, same with review score

#Brand bool, price, promotion flag, and Saturday bool do not have zeros that need to be replaced

#Use data splitting
library(caret)
DataSplit7 <- createDataPartition(y=Expedia7$booking_bool, p=0.75, list=FALSE)

train7 <- Expedia7[DataSplit7,]
test7 <- Expedia7[-DataSplit7]

Ex7 <- train(booking_bool~visitor_hist_starrating+visitor_hist_adr_usd+prop_starrating+
               prop_review_score+prop_brand_bool+promotion_flag+
               srch_saturday_night_bool, data = Expedia7, method = "glm")

summary(Ex7)

#  Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)               7.667e-03  2.414e-03   3.176  0.00149 ** 
#  visitor_hist_starrating   2.758e-03  4.008e-04   6.883 5.88e-12 ***
#  visitor_hist_adr_usd     -1.757e-05  1.275e-05  -1.378  0.16835    
#  prop_starrating           1.583e-03  3.057e-04   5.179 2.23e-07 ***
#  prop_review_score         3.038e-03  3.093e-04   9.821  < 2e-16 ***
#  prop_brand_bool           3.077e-03  6.445e-04   4.775 1.80e-06 ***
#  promotion_flag            1.325e-02  7.456e-04  17.775  < 2e-16 ***
#  srch_saturday_night_bool  2.114e-03  6.024e-04   3.509  0.00045 ***

#The p-value for visitor_hist_adr_usd is high and my guess is this fit will not be 
#an improvement, but I decided to try it anyways and see.

#Load data to be tested
Expedia_Test = read.csv("test.csv")

#Prepare ID
Expedia_ID <- paste(Expedia_Test$srch_id, Expedia_Test$prop_id, sep = "-", collapse = NULL)

#Predict
Expedia_Predictions7 <- predict(Ex7, newdata = Expedia_Test)

#Output predictions
Expedia_Info7 <- data.frame("srch-prop_id" = Expedia_ID, "booking_bool" = Expedia_Predictions7)
write.csv(Expedia_Info7, "Expedia_Predict7.csv", row.names = FALSE)

#SEVENTH TRY, 0.57746


#My eigth attempt did not work and I was not able to get predictions from it.
#I decided to omit the code.


#For my ninth attempt, I decided to fit a cross validated model with all the variables 
#from the first attempt, since it had been the most successful so far.

#Bring in train data
Expedia9 = Expedia[c(5,6,9,10,11,15,16,23,51)]


#using 10-fold CV
install.packages("caret")
library(caret)
set.seed(123)
Expedia9_control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = TRUE)

#gbm model
set.seed(123)
gbmGrid9 <-  expand.grid(interaction.depth = c(1, 2, 3, 4), n.trees = seq(200,10000, by=200), shrinkage = c(0.1,.05))
gbm_fit9 <- train(booking_bool~visitor_hist_starrating+visitor_hist_adr_usd+prop_starrating+
                      prop_review_score+prop_brand_bool+price_usd+promotion_flag+
                      srch_saturday_night_bool, data=Expedia9, method = "gbm", trControl = Expedia9_control, verbose = FALSE, tuneGrid = gbmGrid9)

#Load data to be tested
Expedia_Test = read.csv("test.csv")

#Prepare ID
Expedia_ID <- paste(Expedia_Test$srch_id, Expedia_Test$prop_id, sep = "-", collapse = NULL)

#Predict
Expedia_Predictions9 <- predict(gbm_fit9, newdata = Expedia_Test)

#Output predictions
Expedia_Info9 <- data.frame("srch-prop_id" = Expedia_ID, "booking_bool" = Expedia_Predictions9)
write.csv(Expedia_Info9, "Expedia_Predict9.csv", row.names = FALSE)

#NINTH TRY, 0.62998

