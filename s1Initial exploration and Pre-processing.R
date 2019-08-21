################################################################
# Blackwell Electronics
# Multiple Regression in R, sales prediction
#
# Using SVM algorithm (pipeline + first run of the model)
#
# Created by Eirik Espe
################################################################

#Calling on packages. Install the packages if you do not have them already.
library(readr)
library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)
library(e1071)

#Upload data - Existing Product Attributes
existProd <- read_csv("existingproductattributes2017.csv")

#View the first rows of the data. The data set has 9898 observations.
head(existProd)

#View the structure of the data
str(existProd)

#Create binary values for product type
dummify <- dummyVars("~ .", data = existProd)
existProd_binary <- data.frame(predict(dummify, newdata = existProd))

#View the structure of the data
str(existProd_binary)

#Summary of the data
summary(existProd_binary)

#Remove attributes with missing values
existProd_binary$BestSellersRank <- NULL

#Make a correlation matrix
correlationMatrix <- cor(existProd_binary)

#Look for highly correlated attributes
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.85)
# print indexes of highly correlated attributes
print(highlyCorrelated)
#View the names of higly correlated attributes
colnames(existProd_binary)[highlyCorrelated]

#Make a plot of the observations
corrplot(correlationMatrix, tl.cex = 0.5)
corrplot(correlationMatrix, tl.cex = 0.4, type = "lower", order = "FPC")
corrplot(correlationMatrix, tl.cex = 0.4, tl.col = "black", tl.srt = 68, 
         type = "lower", order = "FPC")
##With numbers
corrplot(correlationMatrix, tl.cex = 0.4, number.cex = 10/ncol(correlationMatrix),
         method = "number", bg = "lemonchiffon4", type = "lower", order = "FPC")

#Change data type for dummy variables and product number. Data type set to factor.
existProd_binary$ProductTypeAccessories <- 
  as.factor(existProd_binary$ProductTypeAccessories)
existProd_binary$ProductTypeDisplay <- 
  as.factor(existProd_binary$ProductTypeDisplay)
existProd_binary$ProductTypeExtendedWarranty <- 
  as.factor(existProd_binary$ProductTypeExtendedWarranty)
existProd_binary$ProductTypeGameConsole <- 
  as.factor(existProd_binary$ProductTypeGameConsole)
existProd_binary$ProductTypeLaptop <- 
  as.factor(existProd_binary$ProductTypeLaptop)
existProd_binary$ProductTypeNetbook <- 
  as.factor(existProd_binary$ProductTypeNetbook)
existProd_binary$ProductTypePC <- 
  as.factor(existProd_binary$ProductTypePC)
existProd_binary$ProductTypePrinter <- 
  as.factor(existProd_binary$ProductTypePrinter)
existProd_binary$ProductTypePrinterSupplies <- 
  as.factor(existProd_binary$ProductTypePrinterSupplies)
existProd_binary$ProductTypeSmartphone <- 
  as.factor(existProd_binary$ProductTypeSmartphone)
existProd_binary$ProductTypeSoftware <- 
  as.factor(existProd_binary$ProductTypeSoftware)
existProd_binary$ProductTypeTablet <- 
  as.factor(existProd_binary$ProductTypeTablet)
existProd_binary$ProductNum <- as.factor(existProd_binary$ProductNum)

# Alternatively
# existProd_binary <- mutate_at(existProd_binary, 
#                               vars(starts_with("ProductType"), ProductNum), 
#                               ~as.factor(.))



#---Visualization----

#Relationship between 5StarReviews and sales volume
ggplot(existProd_binary, aes(x = x5StarReviews, y = Volume)) + geom_point() + 
  geom_smooth(method = "lm", size = 0.3) + 
  labs(title = "Relationship sales volume and number of 5 star reviews", 
       x = "Number of 5 star reviews")

#Sales volume per product number
ggplot(existProd, aes(x = ProductNum, y = Volume)) + 
  geom_point(aes(color = ProductType)) + 
  geom_text(aes(label = ifelse(Volume > 3000, ProductNum, '')), 
            vjust = -1, size = 3) + 
  labs(title = "Sales volume per product number", x="Product#", "Volume") + 
  ylim(0, 11500) +
  theme(plot.title = element_text(hjust = 0.5))

#Sales volume and profit margin per product type
ggplot(existProd, aes(x = ProductType, y = Volume )) + 
  geom_point(aes(size = ProfitMargin, color = ProductType)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Sales volume and profit margin per product type") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = FALSE)

#Overview of product types
ggplot(existProd, aes(x = Recommendproduct, y = Price)) + 
  geom_point(aes(color = ProductType, size = Volume)) + 
  facet_wrap(~ProductType) + 
  labs(title = "Overview of product types") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = FALSE)




#---Scaling / normalization----

#Check for normality
qqnorm(existProd_binary$Volume)
#Creating dataset without sales volume outliers
temp <- filter(existProd_binary, Volume < 3000)
#Check for normality without sales volume outliers
qqnorm(temp$Volume)

#Density plot
ggplot(existProd_binary) + geom_density(aes(x = Volume))
#Density plot without outliers
ggplot(temp) + geom_density(aes(x = Volume))


#Shapiro-Wilk normality test

#--The null hypothesis of these tests is that “sample distribution is normal”. 
#--If the test is significant, the distribution is non-normal.
shapiro.test(existProd$Volume)          #P-value = 5.785e-16
                                        #Not normally distributed

#Shapiro-Wilk normality test after removing sales volume outliers
shapiro.test(temp$Volume)               #P-value = 1.615e-09
                                        #Not normally distributed


# Creating dataset with scaled variables

#Scaling the numeric variables
ScaledVar <- as.data.frame(scale(select_if(existProd_binary, is.numeric) %>% 
                     select(-contains('volume'))))

#Checking the mean and the standard deviation of the scaled dataset
mean(ScaledVar$Price)
sd(ScaledVar$Price)


# Normalize data (creating dataset with normalized variables)

#--Function for normalizing
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

#Use function on numeric variables
normVar <- as.data.frame(apply(select_if(existProd_binary, is.numeric) %>% 
                                 select(-contains('Volume')),
                               2, normalize))

#Combine scaled variables to rest of varables
scaledData <- cbind(select_if(existProd_binary, is.factor), 
                    ScaledVar, existProd_binary$Volume)
normData <- cbind(select_if(existProd_binary, is.factor), 
                  normVar, existProd_binary$Volume)

#Renaming Volume variable
names(scaledData)[names(scaledData) == "existProd_binary$Volume"] <- "Volume"
names(normData)[names(normData) == "existProd_binary$Volume"] <- "Volume"

#---Training and test sets----

#Set seed
set.seed(123)

#Define a 75%/25% train/test split of the dataset
inTraining <- createDataPartition(scaledData$Volume, 
                                  p = .75, 
                                  list = FALSE)
training <- scaledData[inTraining,]
testing <- scaledData[-inTraining,]

#---Cross validation----

#Setting a 10-fold cross validation to use on the model
Control <- tune.control(sampling = "cross", 
                        cross = 10,
                        nrepeat = 3)

#---Training model----


#Train a support vector machine regression model
mod_svm <- svm(Volume ~ ., 
               data = training,
               tunecontrol = Control,
               kernel = "radial")

#Check results on the training set
train_results_1st_svm <- predict(object = mod_svm, newdata = training)
postResample(training$Volume, train_results_1st_svm)
# RMSE 1017.09
# R2    89.86%   (R squared)

#Results on testing set
test_results_1st_svm <- predict(object = mod_svm, newdata = testing)
postResample(testing$Volume, test_results_1st_svm)
# RMSE 295.67
# R2   82.63%

##-- 1st error check: error visualization
testing$Pred_volume1 <- test_results_1st_svm

#--Creating a data set for making charts
prod_type <- existProd[,c("ProductNum", "ProductType")]
prod_type$ProductNum <- as.factor(prod_type$ProductNum)
chartDf <- inner_join(testing, prod_type, by="ProductNum")

#Creating a variable for differences between predictions and actual sales volume
chartDf$Diff1  <- abs(testing$Pred_volume1 - testing$Volume)

#plot1
ggplot(chartDf, aes(x = ProductNum, y = Volume)) + 
  geom_point(aes(color = (Volume == Pred_volume1), size=Diff1)) + 
  labs(title = "SVM model errors") + 
  scale_color_manual(name = "Prediction = actual", values = c("red","white")) + 
  scale_size(name = "Difference", breaks = c(50, 100, 200, 400)) + 
  theme(plot.title = element_text(hjust = 0.5))



#--Including product type  
ggplot(chartDf, aes(x = ProductNum, y = Volume)) + 
  geom_point(aes(color = ProductType, size=Diff1)) + 
  labs(title = "SVM model errors") + 
  scale_size(name = "Deviation", breaks = c(50, 100, 200, 400)) + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(chartDf, aes(x=ProductNum, y = Volume)) + 
  geom_point(aes(color= Diff1, size = Diff1)) +
  labs(title = "SVM model errors") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=ifelse(Diff1 > 200, ProductType, '')), vjust=-1.3, size=3) + 
  scale_color_continuous(breaks = c(50, 100, 200, 400)) + 
  scale_size(breaks = c(50, 100, 200, 400)) + 
  guides(color = guide_legend("Deviation\nfrom actual"), 
         size = guide_legend("Deviation\nfrom actual"))


# Plot with the size of the prediction error indicated by the size of the point
# and the intensity of the colour. Big errors are named with product type.
ggplot(chartDf, aes(x = ProductNum, y = Volume)) +
  geom_point(aes(color= Diff1, size = Diff1)) +
  labs(title = "SVM model errors", x="Product #") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=ifelse(Diff1 > 200, ProductType, '')), vjust=-1.3, size=3) +
  scale_size(breaks = c(50, 100, 200, 400)) + 
  scale_colour_gradient(low = "#FFC0CB", high = "#9a0707", 
                        breaks = c(50, 100, 200, 400)) + 
  guides(color=guide_legend("Deviation\nfrom actual"),
         size=guide_legend("Deviation\nfrom actual"))


#Continues...