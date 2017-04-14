
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
      if( sum(is.na(x)  ) >0 )
      {
        x = factor(x, levels=c(levels(x), "None"))
        x[ is.na(x)==T] = as.factor('None')
      }
  }
    
  return(x)
}



#data_path <- 'C:/Users/2011940/Documents/ProjectsFolders/R Projects/Git_HousePrices/data/' 
data_path <- './data/' 

test_data <- read.csv(paste0(data_path, 'test.csv'), na.strings=c("NA"," ","") )

test_data$MSSubClass <- as.factor(test_data$MSSubClass)
test_data <- as.data.frame(lapply(test_data, convertNaToNoneFactor))
test_data$LotFrontage[ is.na(test_data$LotFrontage)==T] = 0
test_data$MasVnrArea[ is.na(test_data$MasVnrArea)==T] = 0
test_data$GarageYrBlt <- ifelse( is.na(test_data$GarageYrBlt),  test_data$YearBuilt, test_data$GarageYrBlt )

test_data[is.na(test_data)] <- 0

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
train.fit <- lm(SalePrice~LotArea + MSZoning + Street + LandSlope + 
                  Neighborhood  + OverallQual + 
                  OverallCond + YearBuilt + RoofMatl + ExterQual + BsmtQual+ 
                  BsmtExposure + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + X2ndFlrSF
                  , data=train_data)

summary(train.fit)


inputeNewFactors <- function(idx){
  train_vector <- train_data[,idx]
  test_vector <- test_data[,idx]
  
  if(is.factor(test_vector))
  {
    new_items <- subset(test_vector, !(test_vector %in% train_vector ))
    
    if(sum(levels(train_vector) == "None")>0)
    {
      replacementValue <- 'None'
    }
    else{
      replacementValue <- Mode(train_vector)
    }
    
    test_vector[ test_vector %in% new_items  ] <- replacementValue
    
  }
  
  return(test_vector)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

test_data_2 <- as.data.frame(lapply(1:ncol(test_data), inputeNewFactors))

colnames(test_data_2) <- colnames(test_data)

output <- data.frame( Id =test_data_2$Id,   SalePrice=   predict(train.fit, newdata = test_data_2 , type='response'))

write.csv(output, paste0( data_path, "predictions2.csv" ), row.names=F, quote=F)

unique(test_data$Functional)
unique(train_data$Functional)

unique(inputeNewFactors(3))


