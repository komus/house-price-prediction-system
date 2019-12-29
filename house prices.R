#load the required packages, if not available then download
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Boruta)) install.packages("Boruta", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
#read the data
train_set <- read.csv("https://projects.smartsolutaris.com/train.csv", stringsAsFactors=FALSE)
test_set <- read.csv("https://projects.smartsolutaris.com/test.csv", stringsAsFactors=FALSE)

dim(train_set)
dim(test_set)
#train_set is a data frame
class(train_set)
#we need to understand the structure of the dataset. To gain insight the keyword str is used.
str(train_set)
#description of factors can be found here 	https://projects.smartsolutaris.com/data_description.txt	

#Feature ANalysis and Data Cleanup
Identifier <- "Id"
Target <- "SalePrice"
# deterimine data types
Features <- setdiff(names(train_set),c(Identifier,Target))
Features_datatypes <- sapply(Features,function(x){class(train_set[[x]])})
table(Features_datatypes)

# categorize data types in the data set?
unique_classes <- unique(Features_datatypes)

attr_datatypes <- lapply(unique_classes,function(x){names(Features_datatypes[Features_datatypes==x])})
names(attr_datatypes) <- unique_classes

# pull out the response variable
response <- train_set$SalePrice

# missing values are handled as follows: * missing numeric data is set to -1 * missing character data is set to 0 since
for (x in attr_datatypes$integer){
  train_set[[x]][is.na(train_set[[x]])] <- -1
}

for (x in attr_datatypes$character){
  train_set[[x]][is.na(train_set[[x]])] <- 0
}

for (x in attr_datatypes$integer){
  test_set[[x]][is.na(test_set[[x]])] <- -1
}

for (x in attr_datatypes$character){
 test_set[[x]][is.na(test_set[[x]])] <- 0
}
# remove identifier and response variables
train_df <- train_set[Features]
 #use Boruta to compute important attributes for SalePrice prediction
set.seed(13)
bor.results <- Boruta(train_df,response,
                      maxRuns=101,
                      doTrace=0)
bor.results

#get the important attributes
getSelectedAttributes(bor.results)
plot(bor.results)
#From the result above, notice that 50 attributes are confirmed important.
attStats(bor.results)
comp_boruta <- arrange(cbind(attr=rownames(attStats(bor.results)), attStats(bor.results)),desc(medianImp))
confirmed_columns <- comp_boruta %>% filter(decision == "Confirmed") %>% select(attr)
removed_columns <- comp_boruta %>% filter(decision != "Confirmed") %>% select(attr)

#creating a confirmed dataset from Boruta Analysis to create the trainset
final_trainset <- train_set %>% select(Id, GrLivArea,OverallQual,X2ndFlrSF,TotalBsmtSF,X1stFlrSF,GarageArea,
                                       GarageCars,YearBuilt,ExterQual,YearRemodAdd,GarageYrBlt,LotArea,
                                       FireplaceQu,FullBath,Fireplaces,MSSubClass,BsmtFinSF1,
                                       BsmtQual,TotRmsAbvGrd,GarageType,Neighborhood,HalfBath,GarageFinish,BldgType,
                                       Foundation,BedroomAbvGr,HouseStyle,OpenPorchSF,BsmtUnfSF,CentralAir,
                                       BsmtFinType1,MasVnrArea,GarageCond,GarageQual,KitchenAbvGr,OverallCond,
                                       BsmtCond,BsmtFullBath,PavedDrive,WoodDeckSF,
                                       LandContour,BsmtFinType2,Fence,SaleCondition,SalePrice)

final_testset <- test_set %>% select(Id, GrLivArea,OverallQual,X2ndFlrSF,TotalBsmtSF,X1stFlrSF,GarageArea,
                                     GarageCars,YearBuilt,ExterQual,YearRemodAdd,GarageYrBlt,LotArea,
                                     FireplaceQu,FullBath,Fireplaces,MSSubClass,BsmtFinSF1,
                                     BsmtQual,TotRmsAbvGrd,GarageType,Neighborhood,HalfBath,GarageFinish,BldgType,
                                     Foundation,BedroomAbvGr,HouseStyle,OpenPorchSF,BsmtUnfSF,CentralAir,
                                     BsmtFinType1,MasVnrArea,GarageCond,GarageQual,KitchenAbvGr,OverallCond,
                                     BsmtCond,BsmtFullBath,PavedDrive,WoodDeckSF,
                                     LandContour,BsmtFinType2,Fence,SaleCondition)

# #Regression - understanding the relationship between variables
# #finding the correlation between Selling Price and  the overall condition of the house
# #Since both distributions are well approximated by the normal distribution, we could use the two averages and two standard deviations as summaries:
#   
# final_train_set %>% summarize(mean(SalePrice), sd(SalePrice), mean(OverallCond), sd(OverallCond))
# #get the OverallCondition of the hignest SalesPrice
# train_set$OverallCond[which.max(train_set$SalePrice)]
# #This summary fails to describe an important characteristic of the data: The higher the overall condition, the higher the price
# train_set %>% ggplot(aes(OverallCond, SalePrice)) +
#   geom_point(alpha = 0.5)
# train_set %>% summarize(r = cor(OverallCond, SalePrice)) %>% pull(r)
# #from the graph, it can be seen that the highest range price have Overall condition of 5
# 
# ##### correlation between Selling Price and year of sale
# #compute the average and SD
# train_set %>% summarize(mean(SalePrice), sd(SalePrice), mean(YrSold), sd(YrSold))
# train_set %>% ggplot(aes(YrSold, SalePrice)) +
#   geom_point(alpha = 0.5)
# train_set %>% summarize(r = cor(YrSold, SalePrice)) %>% pull(r)



#for cross validation purpose, there is need to divide the train_set into training and validation.
#creating index for division
index <- createDataPartition(y = final_trainset$SalePrice, times = 1, p = 0.1, list = FALSE)
#using the index to split into training and validation
training <- final_trainset[-index,]
validation <- final_trainset[index,]

#Convert all character variable into factor in one line:
training <- training %>% mutate_if(is.character, as.factor)
validation <- validation %>% mutate_if(is.character, as.factor)

## Evaluation of predicted sale price

RMSE <- function(predicted_val, actual_value)
{
  log(predicted_val) - log(actual_value)
}



##K-Nearest Neighbour
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(SalePrice ~ ., method="knn", data = training, tuneGrid = data.frame(k = seq(1, 71, 2)),
                   trControl = control)
ggplot(train_knn)
train_knn$bestTune
knn_y_hat <- predict(train_knn, validation)

rmse_knn <- RMSE(knn_y_hat, validation$SalePrice)
mean(rmse_knn)
#creating a table to aggregate RMSE Models 
rmse_results <- tibble(method = "K-Nearest Neighbour", RMSE = mean(rmse_knn))

## Regression Trees
train_rpart <- train(SalePrice ~., method="rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 100)),
                     data = training)
plot(train_rpart)
rpart_yhat <- predict(train_rpart, validation)
rmse_rpart <- RMSE(rpart_yhat, validation$SalePrice)
mean(rmse_rpart)
rmse_results <- bind_rows(rmse_results, tibble(method = "Regression Tree", RMSE = mean(rmse_rpart)))


#Random Forest

control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <- train(SalePrice ~ .,
                  method = "rf",
                  data = training,
                  ntree = 150,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

rf_yhat <- predict(train_rf, validation, type = "raw")
rmse_rf <- RMSE(rf_yhat, validation$SalePrice)
mean(rmse_rf)
rmse_results<- bind_rows(rmse_results, tibble(method = "Random Forest", RMSE = mean(rmse_rf)))

## Neural Network Model
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
control <- trainControl(method="repeatedcv", number=5,
                                 repeats=1,
                                 verboseIter=FALSE)
train_nnet <- train(SalePrice ~., method="nnet", data = training, trcontrol = control)


nnet_yhat <- predict(train_nnet, validation, type = "raw")
rmse_nnet <- RMSE(nnet_yhat, validation$SalePrice)
mean(rmse_nnet)
rmse_results<- bind_rows(rmse_results, tibble(method = "Random Forest", RMSE = mean(rmse_nnet)))
#Ensembles
#one can usually greatly improve the final results by combining the results of different algorithms.
#we compute new class probabilities by taking the average of random forest and kNN

p <- (knn_y_hat +rpart_yhat + rf_yhat)/3
rmse_em <- RMSE(p, validation$SalePrice)
rmse_results<- bind_rows(rmse_results, tibble(method = "Ensemble", RMSE = mean(rmse_em)))

rmse_results %>% knitr::kable()

## From the results above KNN has the least RMSE. 
#train the full test dataset using KNN

control <- trainControl(method = "cv", number = 10, p = .9)
train_final<- train(SalePrice ~ ., method="knn", data = final_trainset, tuneGrid = data.frame(k = seq(1, 71, 2)),
                   trControl = control)


#Prepare the prediction data
prediction <- predict(train_final, final_testset)
#Bind the result to the test dataset
output <- cbind(final_testset, prediction)
#prepare the final prediction result for Submission File Format
final_prep <- data.frame(Id= output$Id, SalePrice = output$prediction)

#Write the fianl result to a text file
write.csv(final_prep, file = "housePrice_submission1.csv", row.names = F)

#train the full test dataset using regression tree
final_rpart <- train(SalePrice ~., method="rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 100)),
                     data = final_trainset)

#Prepare the prediction data
prediction_rpart <- predict(final_rpart, final_testset)
#Bind the result to the test dataset
output <- cbind(final_testset, prediction_rpart)
#prepare the final prediction result for Submission File Format
final_prep <- data.frame(Id= output$Id, SalePrice = output$prediction_rpart)

#Write the fianl result to a text file
write.csv(final_prep, file = "housePrice_submission2.csv", row.names = F)