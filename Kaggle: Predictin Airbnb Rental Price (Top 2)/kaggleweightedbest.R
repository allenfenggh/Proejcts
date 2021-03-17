library(ggplot2)
library(dplyr)
library(readr)
library(caret)
library(caTools)
library(dlookr)
library(zipcodeR)
library(xgboost)
library(tidyverse)
setwd('C:/Users/allen/Desktop/5200/kaggle')
origin_train = read.csv('data.csv', stringsAsFactors=T)
origin_test = read.csv('test.csv', stringsAsFactors=T)

train = origin_train; test = origin_test
dim(train);dim(test);nrow(train)

#preprocessing
#output a correlation heatmap
co = train%>%select(where(is.numeric))%>%cor(use="complete.obs")
write.csv(co, 'corr1.csv',row.names = F)

#use feature selection 
fs= train%>%select(where(is.numeric))
fs = fs %>%  mutate_if(is.numeric,function(x){replace(x, is.na(x), mean(x, na.rm = T))})

start_mod = lm(price~., data=fs)
empty_mod = lm(price~1, data=fs)
full_mod = lm(price~., data=fs)
backwardStepwise = step(start_mod, scope=list(upper=full_mod, lower=empty_mod),
                        direction = 'backward')
summary(backwardStepwise)





#feature engineering
df = rbind(train[,-47],test)
dim(df)


# select useful features
df1 = df%>%select(host_response_time, neighbourhood_cleansed, neighbourhood_group_cleansed,property_type,room_type,								
                  bed_type,	amenities, 	cancellation_policy, zipcode,last_review, 
                  #numeric
                  accommodates,square_feet, reviews_per_month, beds, 
                  bathrooms, bedrooms,security_deposit,	cleaning_fee,
                  number_of_reviews_ltm, review_scores_rating, review_scores_value,
                  maximum_maximum_nights, host_listings_count,
                  calculated_host_listings_count_entire_homes,
                  calculated_host_listings_count_private_rooms,
                  guests_included,extra_people,	availability_365, availability_30)

#detect NA columns
colnames(df1)[colSums(is.na(df1)) > 0]



#impute missing value with mice package
library(mice)
df_mice = df%>% select(square_feet, weekly_price, security_deposit,
                       bathrooms, bedrooms,room_type,accommodates,property_type)
df_mice_month = df%>%select(reviews_per_month, number_of_reviews)



df_mice1 = complete(mice(df_mice))
df_mice_month1 = complete(mice(df_mice_month))

# cor(train$price, df_mice1$square_feet[1:nrow(train)])
# cor(train$price, df_mice1$weekly_price[1:nrow(train)])
# cor(train$price, df_mice1$security_deposit[1:nrow(train)])

df1$square_feet = df_mice1$square_feet
df1$montly_price = df_mice1$square_feet
df1$security_deposit = df_mice1$security_deposit
df1$reviews_per_month = df_mice_month1$reviews_per_month






#numerical columns, mean imputation
library(tidyverse)
df1clean = df1 %>%mutate(last_review = as.numeric(last_review))%>%
  mutate_if(is.numeric,function(x){replace(x, is.na(x), mean(x, na.rm = T))})%>%
  # mutate_if(is.numeric,function(x){replace(x, is.na(x), 0)})%>%
  # mutate(square_feet = ifelse( is.na(square_feet), mean(square_feet, na.rm = T), square_feet))%>%
  # mutate(security_deposit = ifelse( is.na(security_deposit), mean(security_deposit), security_deposit))%>%
  # mutate(cleaning_fee = replace(cleaning_fee, is.na(cleaning_fee), mean(cleaning_fee)))%>%
  # mutate(reviews_per_month = replace(reviews_per_month, is.na(reviews_per_month), mean(reviews_per_month)))%>%
  mutate(amenities=sapply(amenities, function(x){x=length(stringr::str_split(x, ",")[[1]])}))

# df1clean = df1clean%>%mutate(weekly_price = binning(weekly_price, type = "kmeans"))%>%
#   mutate(square_feet = binning(square_feet, type = "kmeans"))
# df1clean = df1clean%>%mutate_if(is.numeric, function(x){binning(x,type = "kmeans")})


#clean zipcode
zi = as.character(df1clean$zipcode)
zi[zi=='11249\n11249']='11249';zi[zi=='11103-3233']='11103'
zi[zi=='11413-3220']='11413';zi[zi=='11385-2308']='11385'
zi[str_count(zi)!=5]='0';zi[is.na(zi)]='0'
df1clean$zipcode=zi

# 
#translate zip to lat and lng
zdb=zip_code_db
df1clean= df1clean%>% mutate(lat=sapply(zipcode, function(x){ifelse(x!='0',zdb$lat[zdb$zipcode==x], '0' )}))%>%
  mutate(lng=sapply(zipcode, function(x){ifelse(x!='0',zdb$lng[zdb$zipcode==x], '0' )}))

#impute empty lat and lng
df1clean$lat=with(df1clean, ifelse(df1clean$lat =='0'| is.na(df1clean$lat) ==T| df1clean$lat=='NaN',
                                   mean(as.numeric(df1clean$lat[df1clean$neighbourhood_cleansed==df1clean$neighbourhood_cleansed& is.na(df1clean$lat)==F])),
                                   df1clean$lat))

df1clean$lng=with(df1clean, ifelse(df1clean$lng =='0'| is.na(df1clean$lng) ==T| df1clean$lat=='NaN',
                                   mean(as.numeric(df1clean$lng[df1clean$neighbourhood_cleansed==df1clean$neighbourhood_cleansed& is.na(df1clean$lng)==F])),
                                   df1clean$lng))
df1clean$lat = as.numeric(df1clean$lat);df1clean$lng = as.numeric(df1clean$lng)


# drop zipcode
df1clean = subset(df1clean, select = -c(zipcode))

# table(df1clean$property_type)

# table(df1clean$neighbourhood_cleansed)
# train%>% group_by(neighbourhood_cleansed)%>%summarise(n=n(), mean = median(price))%>%
#   arrange(mean)



# colnames(df1clean)[colSums(is.na(df1clean)) > 0]

# category #imputation 
# group rare category for neighbourhood cleansed and property type
rarepro = names(which(table(df1clean$property_type)<10));#rarepro
rareneighbourhood = names(which(table(df1clean$neighbourhood_cleansed)<3));#rareneighbourhood
df1clean=df1clean%>%
  mutate(property_type = fct_collapse(property_type, rarecate=rarepro))

# # table(df1clean$host_response_time)
df1clean = df1clean%>%
  mutate(host_response_time =  fct_collapse(host_response_time, missing=c('','N/A')))%>%
  mutate(neighbourhood_cleansed = fct_collapse(neighbourhood_cleansed, rare=rareneighbourhood))
# 




#split origin analysisData and socringData
lm_train1=df1clean[1:nrow(train),];lm_train1$price = train$price
start = nrow(train)+1
lm_test=df1clean[start:nrow(df1clean),]
dim(lm_train1);dim(lm_test)




#split
set.seed(1031)
split = createDataPartition(y=lm_train1$price,p=0.8,list=F,groups=200)
lm_train1_train = lm_train1[split,]
lm_train1_test = lm_train1[-split,]
mean(lm_train1_train$price)
mean(lm_train1_test$price)




#encode train1
x=select(lm_train1_train, -price)
y=lm_train1_train$price
dmy = dummyVars(" ~ .", data = x, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = x))
#matrix train1
library(xgboost)
dtrain = xgb.DMatrix(data = as.matrix(dat_transformed), label= y)
#encode test1
x1=select(lm_train1_test, -price)
y1=lm_train1_test$price
dmy1 <- dummyVars(" ~ .", data = x1, fullRank = T)
dat_transformed1 <- data.frame(predict(dmy1, newdata = x1))
#matrix test1
dtest <- xgb.DMatrix(data = as.matrix(dat_transformed1), label = y1)


default_param<-list(
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=6, #best=6
  min_child_weight=6, #best=6
  subsample=0.8,
  colsample_bytree=0.8
)
watchlist = list(train=dtrain, test = dtest)

xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 10000, nfold = 10,
                 showsd = T, stratified = T, print_every_n = 40, watchlist = watchlist,
                 early_stopping_rounds = 100, maximize = F)




xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 926)
xgb_mod

#evaluate testset
XGBpred <- predict(xgb_mod, dtest)

sqrt(mean((XGBpred-lm_train1_test$price)^2))

sse = sum((XGBpred-lm_train1_test$price)^2)
sst = sum((mean(lm_train1_train$price) - lm_train1_test$price)^2)
r2_test = 1 - sse/sst;r2_test


library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:25], rel_to_first = TRUE)







#rf train test

library(randomForest);library(ranger)

trControl = trainControl(method='cv', number =5)
tuneGrid = expand.grid(mtry=10, splitrule = 'variance', min.node.size = seq(1,5,by=0.5)) #10,25
set.seed(1031)
cvModel=train(price ~., data = lm_train1_train, method ='ranger', num.trees = 500, trControl=trControl,
              tuneGrid = tuneGrid, importance='impurity')
cvModel
# plot(cvModel)



cv_forest_ranger = ranger(price~., data = lm_train1_train, num.trees = 1000,
                          mtry=cvModel$bestTune$mtry,
                          min.node.size = cvModel$bestTune$min.node.size,
                          splitrule = cvModel$bestTune$splitrule, importance = 'impurity')
1+1
cv_forest_ranger
# sort(cv_forest_ranger$variable.importance)
tunepred = predict(cv_forest_ranger, data = lm_train1_test, num.trees = 1000)
rmse_cv_forest_ranger = sqrt(mean((tunepred$predictions-lm_train1_test$price)^2)); rmse_cv_forest_ranger
#57.27
cv_forest_ranger$variable.importance




#loop for best weight
for (i in seq(0,1,by=0.1)){
  weightedpred = XGBpred*i + tunepred$predictions*(1-i)
  weightedrmse = sqrt(mean((weightedpred-lm_train1_test$price)^2)); print(paste(i, 'rmse',round(weightedrmse,3)))
}
#best 0.7xgb 0.3rf 56. 


#-------------------------------------------------------


#real model
#encode train
realx=select(lm_train1, -price)
realy=lm_train1$price
realdmy = dummyVars(" ~ .", data = realx, fullRank = T)
real_trans <- data.frame(predict(realdmy, newdata = realx))
#matrix train
library(xgboost)
real_dtrain = xgb.DMatrix(data = as.matrix(real_trans), label= realy)

#encode test
pred_x=lm_test
length(lm_test)
pred_dmy = dummyVars(" ~ .", data = pred_x, fullRank = T)
pred_trans = data.frame(predict(pred_dmy, newdata = pred_x))
#matrix test
real_dtest <- xgb.DMatrix(data = as.matrix(pred_trans))
dim(real_dtest);dim(real_dtrain)



# grid
real_xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.05, 0.08,0.1), # 0.05
  max_depth = c(6),# 6
  gamma = 0,
  colsample_bytree=c(0.8,1),#0.8
  min_child_weight=c(5,6,7),#567-6
  subsample=c(0.8) #0.8
)
my_control <-trainControl(method="cv", number=5)
xgb_caret <- train(x=real_dtrain, y=lm_train1$price, method='xgbTree', trControl= my_control, tuneGrid=real_xgb_grid)
xgb_caret$bestTune




#real xgb model
default_param<-list(
  booster = "gbtree",
  eta=0.05, 
  max_depth=6, 
  min_child_weight=6, 
  colsample_bytree=0.8,
  subsample=0.8
)

#xgb
real_xgbcv <- xgb.cv( params = default_param, data = real_dtrain, nrounds = 10000, nfold = 10,
                      showsd = T, stratified = T, print_every_n = 40,
                      early_stopping_rounds = 100, maximize = F)

real_xgbcv
#build best model
real_xgb_mod <- xgb.train(data = real_dtrain, params=default_param, nrounds = 1088 )
real_xgb_mod 

#prediction
real_XGBpred <- predict(real_xgb_mod, real_dtest)
length(real_XGBpred)
submissionFile = data.frame(id = test$id, price = real_XGBpred)
dim(test);dim(submissionFile)

write.csv(submissionFile, 'sample_submissionlast.csv',row.names = F)




library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(real_dtrain),model = real_xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:10], rel_to_first = TRUE)




#real rf ------------------------------------------------------
trControl = trainControl(method='cv', number = 5)
tuneGrid = expand.grid(mtry=7:10, splitrule = 'variance', min.node.size = c(1.5,2.5,3.5))
set.seed(1031)
cvModel1=train(price ~., data = lm_train1, method ='ranger', num.trees = 500, trControl=trControl,
               tuneGrid = tuneGrid)


cvModel1


cv_forest_ranger1 = ranger(price~., data = lm_train1, num.trees = 1000,
                           mtry=cvModel1$bestTune$mtry,
                           min.node.size = cvModel1$bestTune$min.node.size,
                           splitrule = cvModel1$bestTune$splitrule,
                           importance = 'impurity')

cv_forest_ranger1
#feature importance
cv_forest_ranger1$variable.importance
tunepred1 = predict(cv_forest_ranger1, data = lm_test, num.trees = 1000)




#weighted average
stackpred = real_XGBpred*0.7 + tunepred1$predictions*0.3
length(stackpred)
submissionFile18 = data.frame(id = test$id, price = stackpred)
dim(test);dim(submissionFile18)

write.csv(submissionFile18, 'sample_submissionlastweight.csv',row.names = F)




