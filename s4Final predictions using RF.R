################################################################
# Blackwell Electronics
# Multiple Regression in R
#
# Final predictions
#
# Created by Eirik Espe
################################################################

source("s3Predicting volume using RF.R")

#Import data set with new products

library(readr)
newProd <- read_csv("newproductattributes2017.csv")


#---Pre-processing----

#Create binary values for product type
dummify2 <- dummyVars("~ .", data = newProd)
newProd_binary <- data.frame(predict(dummify2, newdata = newProd))



#Change data type for dummy variables and product number. Data type set to factor.
newProd_binary$ProductTypeAccessories <- 
  as.factor(newProd_binary$ProductTypeAccessories)
newProd_binary$ProductTypeDisplay <- 
  as.factor(newProd_binary$ProductTypeDisplay)
newProd_binary$ProductTypeExtendedWarranty <- 
  as.factor(newProd_binary$ProductTypeExtendedWarranty)
newProd_binary$ProductTypeGameConsole <- 
  as.factor(newProd_binary$ProductTypeGameConsole)
newProd_binary$ProductTypeLaptop <- 
  as.factor(newProd_binary$ProductTypeLaptop)
newProd_binary$ProductTypeNetbook <- 
  as.factor(newProd_binary$ProductTypeNetbook)
newProd_binary$ProductTypePC <- 
  as.factor(newProd_binary$ProductTypePC)
newProd_binary$ProductTypePrinter <- 
  as.factor(newProd_binary$ProductTypePrinter)
newProd_binary$ProductTypePrinterSupplies <- 
  as.factor(newProd_binary$ProductTypePrinterSupplies)
newProd_binary$ProductTypeSmartphone <- 
  as.factor(newProd_binary$ProductTypeSmartphone)
newProd_binary$ProductTypeSoftware <- 
  as.factor(newProd_binary$ProductTypeSoftware)
newProd_binary$ProductTypeTablet <- 
  as.factor(newProd_binary$ProductTypeTablet)
newProd_binary$ProductNum <- as.factor(newProd_binary$ProductNum)

# Alternatively
# newProd_binary <- mutate_at(newProd_binary, 
#                               vars(starts_with("ProductType"), ProductNum), 
#                               ~as.factor(.))




#Scaling the numeric variables
ScaledVarNew <- as.data.frame(scale(select_if(newProd_binary, is.numeric) %>% 
                                   select(-contains('volume'))))


#Checking the mean and the standard deviation of the data set
mean(ScaledVarNew$Price)
sd(ScaledVarNew$Price)


#Combine scaled variables to rest of varables
scaledNewData <- cbind(select_if(newProd_binary, is.factor), 
                    ScaledVarNew, newProd_binary$Volume)


#Renaming Volume variable
names(scaledNewData)[names(scaledNewData) == "newProd_binary$Volume"] <- "Volume"




#---Predictions----
finalPred <- round(predict(rfFit4, newdata = scaledNewData))


#Add predictions to new products dataset, named output
output <- newProd
output$Predictions <- finalPred


write.csv(output, file="New products' predicted sales volume.csv", row.names = TRUE)

