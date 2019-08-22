################################################################
# Blackwell Electronics
# Multiple Regression in R, sales prediction
#
# Using SVM algorithm (Pre-processing and model creation)
#
# Created by Eirik Espe
################################################################

source("s1Initial exploration and Pre-processing.R")

#---Outliers removal----

# Creating a dataset where outliers are removed
xoutliers <- filter(scaledData, Volume < 3000 & 
                      PositiveServiceReview < 2.2 & x1StarReviews < 2 &
                      Recommendproduct > -3 & NegativeServiceReview < 3, 
                      !ProductNum %in% c(134:141))
#Product number
  #118           Higher positive service reviews than typical for accessories
  #123           Higher number of 1 star reviews compared to Software products
                 # in general
  #129           Low recommend product score compared to general printer supplies
  #150           Much higher sales volume than other products
  #167           High number negative service reviews
  #198           Higher sales volume than other game consoles, as well as much
                 # higher sales volume than other products
  #134:141       Extended warranties that have same number of ratings on all 
                 # features except price. Might be a copy paste error
    


#---New training and test sets----

#Set seed
set.seed(123)

#Define a 75%/25% train/test split of the dataset
inTraining <- createDataPartition(xoutliers$Volume, 
                                  p = .75, 
                                  list = FALSE)
training <- xoutliers[inTraining,]
testing <- xoutliers[-inTraining,]

#---Cross validation----

#Setting a 10-fold cross validation to use on the model
Control <- tune.control(sampling = "cross", 
                        cross = 10,
                        nrepeat = 3)

#---Training model----


#Train a support vector machine regression model
mod_svm2 <- svm(Volume ~ ., 
               data = training,
               tunecontrol = Control,
               kernel = "radial")


#---Results 2nd model----

#Check results on the training set
train_results_2nd_svm <- predict(object = mod_svm2, newdata = training)
postResample(training$Volume, train_results_2nd_svm)
# RMSE 144.17
# R2    97.43%    (R squared)

#Results on testing set
test_results_2nd_svm <- predict(object = mod_svm2, newdata = testing)
postResample(testing$Volume, test_results_2nd_svm)
# RMSE 198.96
# R2    85.82%


##-- 2nd error check: error visualization
testing$Pred_volume1 <- test_results_2nd_svm

#--Creating a data set for making charts
chartDf2 <- inner_join(testing, prod_type, by="ProductNum")

#Creating a variable for differences between predictions and actual sales volume
chartDf2$Diff1  <- abs(testing$Pred_volume1-testing$Volume)

# Plotting the errors
ggplot(chartDf2, aes(x = ProductNum, y = Volume)) +
  geom_point(aes(color = Diff1, size = Diff1)) +
  labs(title = "2nd SVM model errors", x="Product #") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=ifelse(Diff1 > 100, ProductType, '')), vjust = -1.3, size=3) +
  scale_size(breaks = c(20, 50, 100, 200, 400)) + 
  scale_colour_gradient(low = "#FFC0CB", high = "#9a0707", 
                        breaks = c(20, 50, 100, 200, 400)) + 
  guides(color = guide_legend("Deviation\nfrom actual"),
         size = guide_legend("Deviation\nfrom actual"))




#---New training model with feature selection----

# Varables kept are: 4 star reviews, 1 star reviews, positive service review
#                    negative service reviews and whether consumers would 
#                    recommend product

#Train a support vector machine regression model
mod_svm3 <- svm(Volume ~ Price + x4StarReviews + x1StarReviews + 
                  Recommendproduct + PositiveServiceReview + 
                  NegativeServiceReview, 
                data = training,
                tunecontrol = Control,
                kernel = "radial")


#Check results on the training set
train_results_3rd_svm <- predict(object = mod_svm3, newdata = training)
postResample(training$Volume, train_results_3rd_svm)
#RMSE 161.36
#R2    94.25%

#Results on testing set
test_results_3rd_svm <- predict(object = mod_svm3, newdata = testing)
postResample(testing$Volume, test_results_3rd_svm)
#RMSE 223.47
#R2    78.46%

