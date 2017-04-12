
library(dplyr)
library(ggplot2)
library(data.table)

addNoneFactor <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "None")))
  return(x)
}

convertNaToNoneFactor <- function(x){
  if(is.factor(x))
  {
    
      x = factor(x, levels=c(levels(x), "None"))
      x[ is.na(x)==T] = as.factor('None')
    
  }
    
  return(x)
}



data_path <- 'C:/Users/2011940/Documents/ProjectsFolders/R Projects/Git_HousePrices/data/' 

test_data <- read.csv(paste0(data_path, 'test.csv'), na.strings=c("NA"," ","") )

test_data$MSSubClass <- as.factor(test_data$MSSubClass)
test_data <- as.data.frame(lapply(test_data, convertNaToNoneFactor))
test_data$LotFrontage[ is.na(test_data$LotFrontage)==T] = 0
test_data$MasVnrArea[ is.na(test_data$MasVnrArea)==T] = 0
test_data$GarageYrBlt <- ifelse( is.na(test_data$GarageYrBlt),  test_data$YearBuilt, test_data$GarageYrBlt )


train_data <- read.csv( paste0(data_path, 'train.csv') , na.strings=c("NA"," ",""))

train_data$MSSubClass <- as.factor(train_data$MSSubClass)

train_data <- as.data.frame(lapply(train_data, convertNaToNoneFactor))
train_data$LotFrontage[ is.na(train_data$LotFrontage)==T] = 0
train_data$MasVnrArea[ is.na(train_data$MasVnrArea)==T] = 0
train_data$GarageYrBlt <- ifelse( is.na(train_data$GarageYrBlt),  train_data$YearBuilt, train_data$GarageYrBlt )



train.fit <- lm(SalePrice ~., train_data, na.action = NULL)

apply(train_data, 2, function(x)  sum(is.na(x)) )

#test_data$MSZoning <- NULL
#train_data$MSZoning <- NULL
train.fit <- lm(SalePrice~., data=train_data)

summary(train.fit)


predict(train.fit, newdata = test_data , type='response')

x <- (train_data$MSSubClass)

y <- (test_data$MSSubClass)


subset( y, !(y %in% x) )


convertNewFactorsToNone <- function(idx){
  train_vector <- train_data[,idx]
  test_vector <- test_data[,idx]
  
  if(is.factor(test_vector))
  {
    new_items <- subset(test_vector, !(test_vector %in% train_vector ))
    
    test_vector[ test_vector %in% new_items  ] <- 'None'
    
  }
  
  return(test_vector)
}

unique( convertNewFactorsToNone(2) )
