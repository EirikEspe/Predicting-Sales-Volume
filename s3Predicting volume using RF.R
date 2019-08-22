################################################################
# Blackwell Electronics
# Multiple Regression in R
#
# Using Random Forest algorithm
#
# Created by Eirik Espe
################################################################

source("s2Predicting volume using svm (feature selection).R")


#Chart for sales volume per product type
ggplot(existProd, aes(x = ProductType, y = Volume)) + 
  geom_point(aes(color = ProductType), position = "jitter") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Sales volume per product type") + 
  theme(plot.title = element_text(hjust = 0.5))

#---Cross validation----

#Setting a 10-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 1)


#Train a Random Forest model with tunelength=10
rfFit1 <- train(Volume~., data = training, 
                method = "rf",
                trControl = Control, 
                tuneLength = 10)

#---Results RF model----

#Check results on the training set
train_results_1st_rf <- predict(object = rfFit1, newdata = training)
postResample(training$Volume, train_results_1st_rf)
# RMSE 58.57
# R2   99.19%     (R squared)

#Results on testing set
test_results_1st_rf <- predict(object = rfFit1, newdata = testing)
postResample(testing$Volume, test_results_1st_rf)
# RMSE 67.45
# R2   97.99%     (R squared)


##-- 1st error check RF: error visualization
testing$Pred_volume3 <- test_results_1st_rf

#Creating a variable for differences between predictions and actual sales volume
chartDf2$Diff3  <- abs(testing$Pred_volume3-testing$Volume)


# Plot the errors
ggplot(chartDf2, aes(x = ProductNum, y = Volume)) +
  geom_point(aes(color = Diff3, size = Diff3)) +
  labs(title = "1st RF model errors", x = "Product #") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=ifelse(Diff3 > 100, ProductType, '')), vjust = -1.3, size=3) +
  scale_size(breaks = c(20, 50, 100, 200, 400)) + 
  scale_colour_gradient(low = "#FFC0CB", high = "#9a0707", 
                        breaks = c(20, 50, 100, 200, 400)) + 
  guides(color = guide_legend("Deviation\nfrom actual"),
         size = guide_legend("Deviation\nfrom actual"))


#---New training model with feature selection----

# Varables kept are: Product types, 4 star reviews, 1 star reviews, 
#                    positive service review, negative service reviews and 
#                    whether consumers would recommend product

#Train a Random Forest regression model, with tunelength=10
rfFit2 <- train(Volume ~ ProductTypeAccessories + ProductTypeDisplay + 
                  ProductTypeExtendedWarranty + ProductTypeGameConsole +
                  ProductTypeLaptop + ProductTypeNetbook + ProductTypePC +
                  ProductTypePrinter + ProductTypePrinterSupplies + 
                  ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet +
                  Price + x4StarReviews + x1StarReviews + Recommendproduct +
                  PositiveServiceReview + NegativeServiceReview, 
                data = training, 
                method = "rf",
                trControl = Control,
                importance = TRUE,
                tuneLength = 10)

#---Results 2nd RF model----

#Check results on the training set
train_results_2nd_rf <- predict(object = rfFit2, newdata = training)
postResample(training$Volume, train_results_2nd_rf)
# RMSE 88.72
# R2   97.87%

#Results on testing set
test_results_2nd_rf <- predict(object = rfFit2, newdata = testing)
postResample(testing$Volume, test_results_2nd_rf)
# RMSE 135.55
# R2    91.62%


##-- 2nd error check RF: error visualization
testing$Pred_volume4 <- test_results_2nd_rf

#Creating a variable for differences between predictions and actual sales volume
chartDf2$Diff4  <- abs(testing$Pred_volume4-testing$Volume)


ggplot(chartDf2, aes(x = ProductNum, y = Volume)) +
  geom_point(aes(color= Diff4, size = Diff4)) +
  labs(title = "2nd RF model errors", x="Product #") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=ifelse(Diff4 > 100, ProductType, '')), vjust = -1.3, size=3) +
  scale_size(breaks = c(20, 50, 100, 200, 400)) + 
  scale_colour_gradient(low = "#FFC0CB", high = "#9a0707", 
                        breaks = c(20, 50, 100, 200, 400)) + 
  guides(color = guide_legend("Deviation\nfrom actual"),
         size = guide_legend("Deviation\nfrom actual"))


#---New training model with adjustments----


#Train a Random Forest regression model, with tunelength=10 (removed Price)
rfFit3 <- train(Volume ~ ProductTypeAccessories + ProductTypeDisplay + 
                  ProductTypeExtendedWarranty + ProductTypeGameConsole +
                  ProductTypeLaptop + ProductTypeNetbook + ProductTypePC +
                  ProductTypePrinter + ProductTypePrinterSupplies + 
                  ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet +
                  x4StarReviews + x1StarReviews + Recommendproduct +
                  PositiveServiceReview + NegativeServiceReview, 
                data = training, 
                method = "rf",
                trControl = Control,
                importance = TRUE,
                tuneLength = 10)

#---Results 3rd RF model----

#Check results on the training set
train_results_3rd_rf <- predict(object = rfFit3, newdata = training)
postResample(training$Volume, train_results_3rd_rf)
# RMSE 93.21
# R2   97.77%

#Results on testing set
test_results_3rd_rf <- predict(object = rfFit3, newdata = testing)
postResample(testing$Volume, test_results_3rd_rf)
# RMSE 143.18
# R2    90.57%


##-- 3rd error check RF: error visualization
testing$Pred_volume5 <- test_results_3rd_rf

#Creating a variable for differences between predictions and actual sales volume
chartDf2$Diff5  <- abs(testing$Pred_volume5-testing$Volume)


#Plot of the errors
ggplot(chartDf2, aes(x = ProductNum, y = Volume)) +
  geom_point(aes(color= Diff5, size = Diff5)) +
  labs(title = "3rd RF model errors", x="Product #") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=ifelse(Diff5 > 100, ProductType, '')), vjust = -1.3, size=3) +
  scale_size(breaks = c(20, 50, 100, 200, 400)) + 
  scale_colour_gradient(low = "#FFC0CB", high = "#9a0707", 
                        breaks = c(20, 50, 100, 200, 400)) + 
  guides(color = guide_legend("Deviation\nfrom actual"),
         size = guide_legend("Deviation\nfrom actual"))


#---New training model with adjustments----

# Same feature selection as the 2nd RF model, but less product types 
# included in the model

#Train a Random Forest regression model, with tunelength=10
rfFit4 <- train(Volume ~ ProductTypeAccessories + ProductTypeDisplay + 
                  ProductTypeExtendedWarranty + ProductTypeGameConsole +
                  ProductTypeLaptop + ProductTypeNetbook + ProductTypePC +
                  ProductTypeSmartphone + ProductTypeSoftware +
                  Price + x4StarReviews + x1StarReviews + Recommendproduct +
                  PositiveServiceReview + NegativeServiceReview, 
                data = training, 
                method = "rf",
                trControl = Control,
                importance = TRUE,
                tuneLength = 10)


#---Results 4th RF model----

#Check results on the training set
train_results_4th_rf <- predict(object = rfFit4, newdata = training)
postResample(training$Volume, train_results_4th_rf)
# RMSE 92.97
# R2   97.76%

#Results on testing set
test_results_4th_rf <- predict(object = rfFit4, newdata = testing)
postResample(testing$Volume, test_results_4th_rf)
# RMSE 138.38
# R2    91.19%


##-- 4th error check RF: error visualization
testing$Pred_volume6 <- test_results_4th_rf

#Creating a variable for differences between predictions and actual sales volume
chartDf2$Diff6  <- abs(testing$Pred_volume6 - testing$Volume)


#Plot of the errors
ggplot(chartDf2, aes(x = ProductNum, y = Volume)) +
  geom_point(aes(color = Diff6, size = Diff6)) +
  labs(title = "4th RF model errors", x="Product #") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=ifelse(Diff6 > 100, ProductType, '')), vjust = -1.3, size=3) +
  scale_size(breaks = c(20, 50, 100, 200, 400)) + 
  scale_colour_gradient(low = "#FFC0CB", high = "#9a0707", 
                        breaks = c(20, 50, 100, 200, 400)) + 
  guides(color = guide_legend("Deviation\nfrom actual"),
         size = guide_legend("Deviation\nfrom actual"))
