# Import the data and look at the first six rows
train <- read.csv(file = 'train.csv')
head(train)
test <- read.csv(file = 'test.csv')
head(test)

# --------- getting to know data --------- #
str(train)
str(test)
# 949 obs. of  81 variables
summary(train)
summary(test)
# We can find there are missing values 

# --------- deal with missing data for both train/test data--------- #
# --------- make the category variables as factor variables for both data--------- #

# Find the percentage of data is missing from each variable
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(train, 2, p)
# LotFrontage 18.76%, Alley 94.84%, MasVnrType contains 0.63%, MasVnrArea contains 0.63%,
# BsmtQual contains 2.21%, BsmtCond contains 2.21%, BsmtExposure contains 2.21%
# , BsmtFinType1 contains 2.21%, 
# BsmtFinType2 contains 2.32%, FireplaceQu contains 45.94%, GarageType contains 4.53%
# , GarageYrBlt contains 4.53%, GarageFinish contains 4.53%
# GarageQual contains 4.53%, GarageCond contains 4.53%, PoolQC contains 99.58%
# , Fence contains 80.93%, MiscFeature contains 95.68%

# We should just drop those variables with too many missing values: Alley 94.84%
# , FireplaceQu contains 45.94%, Fence contains 80.93%, MiscFeature contains 95.68%
# , PoolQC contains 99.58%


drops <- c("Alley","FireplaceQu","Fence","MiscFeature", "PoolQC")
train_drop = train[ , !(names(train) %in% drops)]
test_drop = test[ , !(names(test) %in% drops)]


find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}


train2 <- train_drop # Duplicate data frame
for(i in 2:ncol(train2)) {
  if (class(train2[,i]) == "factor"|class(train2[,i]) == "character"|class(train2[,i]) == "integer") {
    missing = which(is.na(train2[ , i]))
    md <- find_mode(train2[,i][-missing])
    train2[ , i][missing] <- md
  } # Replace NA in all columns
  else {
    missing = which(is.na(train2))
    m <- mean(train2[ , i], na.rm = TRUE)
    train2[ , i][missing] <- m
    scale(train2[,i])}
}

test2 <- test_drop # Duplicate data frame
for(i in 2:ncol(test2)) {
  if (class(test2[,i]) == "factor"|class(test2[,i]) == "character"|class(test2[,i]) == "integer") {
    missing = which(is.na(test2[ , i]))
    md <- find_mode(test2[,i][-missing])
    test2[ , i][missing] <- md
  } # Replace NA in all columns
  else {
    missing = which(is.na(test2[ , i]))
    m <- mean(test2[ , i], na.rm = TRUE)
    test2[ , i][missing] <- m
    scale(test2[,i])}
}


for(i in 2:ncol(train2)) {
  if (class(train2[,i]) == "factor"|class(train2[,i]) == "character") {
    train2[,i] <- as.factor(train2[,i])
  } 
  }

for(i in 2:ncol(test2)) {
  if (class(test2[,i]) == "factor"|class(test2[,i]) == "character") {
    test2[,i] <- as.factor(test2[,i])
  } 
}  

summary(train2)
summary(test2)

str(train2)
str(test2)





# create new features to be used instead of the originial ones
train2$Porch <- train2$OpenPorchSF + train2$EnclosedPorch + train2$X3SsnPorch + train2$ScreenPorch
train2$bath_total <- train2$FullBath + 0.5*train2$HalfBath + train2$BsmtFullBath + 0.5*train2$BsmtHalfBath
train2$bsmtfinsf = train2$BsmtFinSF1 + train2$BsmtFinSF2 - train2$BsmtUnfSF
train2$finsf = train2$X1stFlrSF + train2$X2ndFlrSF - train2$LowQualFinSF

test2$Porch <- test2$OpenPorchSF + test2$EnclosedPorch + test2$X3SsnPorch + test2$ScreenPorch
test2$bath_total <- test2$FullBath + 0.5*test2$HalfBath + test2$BsmtFullBath + 0.5*test2$BsmtHalfBath
test2$bsmtfinsf = test2$BsmtFinSF1 + test2$BsmtFinSF2 - test2$BsmtUnfSF
test2$finsf = test2$X1stFlrSF + test2$X2ndFlrSF - test2$LowQualFinSF


# --------- drop useless features ---------#

drops <- c("Id","Utilities", "OpenPorchSF", "EnclosedPorch" , "X3SsnPorch", "ScreenPorch"
           ,"FullBath", "HalfBath", "BsmtFullBath", "BsmtHalfBath", "BsmtFinSF1",
           "BsmtFinSF2", "BsmtUnfSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF","Street","Heating")

train2_drop <- train2[ , !(names(train2) %in% drops)]
test2_drop <- test2[ , !(names(test2) %in% drops)]

# find the number of column == 0 and drop the ones that have too many 0s
colSums(train2_drop==0)
# MasVnrArea 554, WoodDeckSF 504, PoolArea 945, MiscVal 909 
drops <- c("MasVnrArea", "WoodDeckSF", "PoolArea", "MiscVal")

train2_drop <- train2_drop[ , !(names(train2_drop) %in% drops)]
test2_drop <- test2_drop[ , !(names(test2_drop) %in% drops)]




# --------- check each feature again to see if all the type are correct ---------#
str(train2_drop)
str(test2_drop)


# --------- dealing with year columns ---------#


# year columns
train2_drop$YearBuilt <- round((train2_drop$YearBuilt)/5)*5
train2_drop$YearRemodAdd <- round((train2_drop$YearRemodAdd)/5)*5
train2_drop$GarageYrBlt <- round((train2_drop$GarageYrBlt)/5)*5
train2_drop$YrSold <- round((train2_drop$YrSold)/5)*5

test2_drop$YearBuilt <- round((test2_drop$YearBuilt)/5)*5
test2_drop$YearRemodAdd <- round((test2_drop$YearRemodAdd)/5)*5
test2_drop$GarageYrBlt <- round((test2_drop$GarageYrBlt)/5)*5
test2_drop$YrSold <- round((test2_drop$YrSold)/5)*5

# --------- check if there is any year in train dataset and not in test dataset or vice versa ---------#
sort(unique(train2_drop$YearBuilt))
sort(unique(test2_drop$YearBuilt))

test2_drop$YearBuilt[test2_drop$YearBuilt == 1875] <- 1880
test2_drop$YearBuilt[test2_drop$YearBuilt == 1895] <- 1900

sort(unique(train2_drop$YearRemodAdd))
sort(unique(test2_drop$YearRemodAdd))
sort(unique(train2_drop$GarageYrBlt))
sort(unique(test2_drop$GarageYrBlt))
sort(unique(train2_drop$YearBuilt))
sort(unique(test2_drop$YearBuilt))
sort(unique(train2_drop$YrSold))
sort(unique(test2_drop$YrSold))
sort(unique(train2_drop$MoSold))
sort(unique(test2_drop$MoSold))



train2_drop$YearBuilt <- as.factor(train2_drop$YearBuilt)
train2_drop$YearRemodAdd <- as.factor(train2_drop$YearRemodAdd)
train2_drop$GarageYrBlt <- as.factor(train2_drop$GarageYrBlt)
train2_drop$YrSold <- as.factor(train2_drop$YrSold)
train2_drop$MoSold <- as.factor(train2_drop$MoSold)

test2_drop$YearBuilt <- as.factor(test2_drop$YearBuilt)
test2_drop$YearRemodAdd <- as.factor(test2_drop$YearRemodAdd)
test2_drop$GarageYrBlt <- as.factor(test2_drop$GarageYrBlt)
test2_drop$YrSold <- as.factor(test2_drop$YrSold)
test2_drop$MoSold <- as.factor(test2_drop$MoSold)



train2_drop$HouseStyle[train2_drop$HouseStyle == '1.5Fin'] <- '1.5Unf'
train2_drop$HouseStyle[train2_drop$HouseStyle == '2.5Fin'] <- '2.5Unf'
test2_drop$HouseStyle[test2_drop$HouseStyle == '1.5Fin'] <- '1.5Unf'
test2_drop$HouseStyle[test2_drop$HouseStyle == '2.5Fin'] <- '2.5Unf'


train2_drop$Exterior1st[train2_drop$Exterior1st == 'Stone'|train2_drop$Exterior1st == 'PreCast'|train2_drop$Exterior1st == 'ImStucc'] <- 'BrkComm'
test2_drop$Exterior1st[test2_drop$Exterior1st == 'Stone'|test2_drop$Exterior1st == 'PreCast'|test2_drop$Exterior1st == 'ImStucc'] <- 'BrkComm'


train2_drop$Exterior2nd[train2_drop$Exterior2nd == 'PreCast'|train2_drop$Exterior2nd == 'Other'] <- 'CBlock'
test2_drop$Exterior2nd[test2_drop$Exterior2nd == 'PreCast'|test2_drop$Exterior2nd == 'Other'] <- 'CBlock'



# --------- some features better to be ordinal ---------#
train2_drop$ExterQual<- recode(train2_drop$ExterQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
train2_drop$ExterCond<-recode(train2_drop$ExterCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
train2_drop$BsmtQual<-recode(train2_drop$BsmtQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
train2_drop$BsmtExposure<-recode(train2_drop$BsmtExposure, Po = 1, Mn = 2, Av = 3, Gd = 4)
train2_drop$BsmtFinType1<-recode(train2_drop$BsmtFinType1, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6)
train2_drop$BsmtFinType2<-recode(train2_drop$BsmtFinType2, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6)
train2_drop$HeatingQC<-recode(train2_drop$HeatingQC, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
train2_drop$KitchenQual<-recode(train2_drop$KitchenQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
train2_drop$GarageFinish<-recode(train2_drop$GarageFinish, Unf = 1, RFn = 2, Fin = 3)
train2_drop$GarageQual<-recode(train2_drop$GarageQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)

test2_drop$ExterQual<- recode(test2_drop$ExterQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
test2_drop$ExterCond<-recode(test2_drop$ExterCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
test2_drop$BsmtQual<-recode(test2_drop$BsmtQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
test2_drop$BsmtExposure<-recode(test2_drop$BsmtExposure, Po = 1, Mn = 2, Av = 3, Gd = 4)
test2_drop$BsmtFinType1<-recode(test2_drop$BsmtFinType1, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6)
test2_drop$BsmtFinType2<-recode(test2_drop$BsmtFinType2, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6)
test2_drop$HeatingQC<-recode(test2_drop$HeatingQC, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
test2_drop$KitchenQual<-recode(test2_drop$KitchenQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)
test2_drop$GarageFinish<-recode(test2_drop$GarageFinish, Unf = 1, RFn = 2, Fin = 3)
test2_drop$GarageQual<-recode(test2_drop$GarageQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5)

# --------- drop unique value > 90% features ---------#

drops <- c("Street","LandContour", "LandSlope", "RoofMatl" , "BsmtCond", "Heating"
           ,"CentralAir", "Electrical", "Functional", "GarageQual", "GarageCond",
           "PavedDrive")

train2_drop <- train2_drop[ , !(names(train2_drop) %in% drops)]
test2_drop <- test2_drop[ , !(names(test2_drop) %in% drops)]

str(train2_drop)
str(test2_drop)


install.packages('gbm')                    # for fitting the gradient boosting model
install.packages('caret')       # for general data preparation and model fitting

library(gbm)
library(ggplot2)
library(lattice)
library(caret)



#normalize the target variable
#train2_drop$SalePrice_ln <- log(train2_drop$SalePrice)
train_final <- train2_drop
test_final <- test2_drop
#train_final <- train_final[ , !(names(train_final) %in% ('SalePrice'))]

# --------- Split train data into training and validation sets --------- #
## set the seed to make your partition reproducible
set.seed(123)
## 80% of the sample size
smp_size <- floor(0.8 * nrow(train_final))


train_ind <- sample(seq_len(nrow(train_final)), size = smp_size)

train_train <-  train_final[train_ind, ]
train_test <- train_final[-train_ind, ]


# --------- Fit Gradient Boosting Model --------- #
model_gbm = gbm(train_train$SalePrice ~.,
                data = train_train,
                distribution = "gaussian",
                #interaction.depth = 1,
                cv.folds = 10,
                shrinkage = .1,
                n.minobsinnode = 10,
                n.trees = 100)

print(model_gbm)

summary(model_gbm)


# --------- Predict the validation set with the model --------- #

test_y <- train_test$SalePrice
pred_y = predict.gbm(model_gbm, train_test)
pred_y

residuals = test_y - pred_y
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

y_test_mean = mean(test_y)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

# visualize the model, actual and predicted data
x_ax = 1:length(pred_y)
plot(x_ax, test_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_y, col="red", pch=20, cex=.9) 


# --------- Re-train the model with all the training data given --------- #
# --------- Fit Gradient Boosting Model --------- #
model2_gbm = gbm(train_final$SalePrice ~.,
                 data = train_final,
                 distribution = "gaussian",
                 #interaction.depth = 1,
                 cv.folds = 10,
                 shrinkage = .1,
                 n.minobsinnode = 10,
                 n.trees = 100)

print(model2_gbm)

summary(model2_gbm)



# --------- Predict the test data with Gradient Boosting --------- #

pred_final = predict.gbm(model2_gbm, test_final)
pred_final

# --------- Generate final dataset --------- #
df <- data.frame(matrix(unlist(pred_final), nrow=length(pred_final), byrow=TRUE))
df$Id <- test$Id
df2 <- df[ncol(df):1]
colnames(df2) <- c('Id','SalePrice')
df2

write.csv(df2,"Submission_1208.csv", row.names = FALSE)









library(xgboost)

#put into the xgb matrix format
dtrain = xgb.DMatrix(data =  as.matrix(train_train), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(train_test), label = test_y)

