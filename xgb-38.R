
# Any results you write to the current directory are saved as output.
#In this version, el7 removed the outliers ISA
#this is modofocation over version 32 ISA
#nrounds to 3000 el7amdleAllah worked perfect
# max_depth = 4 worked fine el7amdleAllah
#changing the min_child_weight from 15 did not work
#changing subsample ISA from 0.7 to 0.8 did not work
#In this version, will try  to change the eta from 0.005 to 0.004
#ya Mosahel ya Rab
#Bism Allah AlRahman Al Rahim

library(data.table)
library(caret)
library(xgboost)
#library(dplyr)

prop <- fread('../input/properties_2016.csv', stringsAsFactors = FALSE)
train <- fread('../input/train_2016_v2.csv', stringsAsFactors = FALSE)

## Data Preparation
prop$hashottuborspa <- ifelse(prop$hashottuborspa == 'true', 1, 0)
prop$fireplaceflag <- ifelse(prop$fireplaceflag == 'true', 1, 0)
prop$taxdelinquencyflag <- ifelse(prop$taxdelinquencyflag == 'Y', 1, 0)
prop$propertycountylandusecode <- as.numeric(as.factor(prop$propertycountylandusecode))
prop$propertyzoningdesc <- as.numeric(as.factor(prop$propertyzoningdesc))



#To handle the NA in column by column
#1.aircon


prop$aircon[is.na(prop$aircon)] <- 1





#3.story



prop$story[is.na(prop$story)] <- 7



#4.censustractandblock



prop <- subset( prop, select = -censustractandblock )


#5.heating



prop$heating[is.na(prop$heating)] <- 2


#6.Num_garage



prop$num_garage[is.na(prop$num_garage)] <- 1


#6.area_garage



#to get how frequent each unique number is repeated
summary(sort(as.factor(prop$area_garage),decreasing = TRUE))
prop$area_garage[is.na(prop$area_garage)] <- 346


#7.area_basement



#8.area_base




#9.area_total_finished  

summary(prop$area_total_finished)
prop <- subset( prop, select = -area_total_finished )


#10.deck


summary(prop$deck)
prop <- subset( prop, select = -deck )


#11.architectural_style 


summary(prop$architectural_style)
prop <- subset( prop, select = -architectural_style )


#12.area_firstfloor_finished 



summary(prop$area_firstfloor_finished)
prop <- subset( prop, select = -area_firstfloor_finished )


#13.area_liveperi_finished 


summary(prop$area_liveperi_finished )
prop <- subset( prop, select = -area_liveperi_finished  )


#14.area_unknown     


summary(prop$area_unknown )
prop <- subset( prop, select = -area_unknown  )


#15.num_fireplace      



summary(prop$num_fireplace )
prop <- subset( prop, select = -num_fireplace  )


#16.area_pool 


summary(prop$area_pool )
prop <- subset( prop, select = -area_pool  )


#17.spa          


summary(prop$spa  )
prop <- subset( prop, select = -spa   )


#18.pool_w_spa   



summary(prop$pool_w_spa  )
prop <- subset( prop, select = -pool_w_spa   )


#19.material    



summary(prop$material  )
prop <- subset( prop, select = -material   )


#20.area_patio       


summary(prop$area_patio  )
prop <- subset( prop, select = -area_patio   )


#21.area_shed        



summary(prop$area_shed  )
prop <- subset( prop, select = -area_shed   )


#22.num_story    



summary(prop$num_story  )
prop <- subset( prop, select = -num_story   )


#23.tax_delinquency_year


summary(prop$tax_delinquency_year  )
prop <- subset( prop, select = -tax_delinquency_year   )


#24.num_75_bath       


summary(prop$num_75_bath  )
prop <- subset( prop, select = -num_75_bath   )


#25.num_bathroom    


summary(prop$num_bathroom  )
prop$num_bathroom[is.na(prop$num_bathroom)] <- 2


#26.num_bedroom        


summary(prop$num_bedroom  )
prop$num_bedroom[is.na(prop$num_bedroom)] <- 3


#27.quality      


summary(prop$quality   )
prop$quality[is.na(prop$quality )] <- 5.57   


#28.num_bathroom_calc


summary(prop$num_bathroom_calc  )
prop$num_bathroom_calc[is.na(prop$num_bathroom_calc)] <- 2



#29.area_total_calc 

summary(prop$area_total_calc  )
prop$area_total_calc[is.na(prop$area_total_calc)] <- 1773   


#30.area_live_finished


summary(prop$area_live_finished  )
prop$area_live_finished[is.na(prop$area_live_finished)] <- 1745   


#31.fips


summary(prop$fips  )
prop$fips[is.na(prop$fips)] <- 6049      


#32.num_bath        


summary(prop$num_bath  )
prop$num_bath[is.na(prop$num_bath)] <- 2      


#33.num_room          


summary(prop$num_room  )
prop$num_room[is.na(prop$num_room)] <- 0      


#34.region_zip        


summary(prop$region_zip  )
prop$region_zip[is.na(prop$region_zip)] <- 96393         



#35.num_unit        


summary(prop$num_unit  )
prop$num_unit[is.na(prop$num_unit)] <- 1         


#36.build_year     

summary(prop$build_year  )
prop$build_year[is.na(prop$build_year)] <- 1969         


#37.tax_building       


summary(prop$tax_building  )
prop$tax_building[is.na(prop$tax_building)] <- 180093            



#38.tax_total           


summary(prop$tax_total  )
prop$tax_total[is.na(prop$tax_total)] <- 457673            


#40.tax_year   


summary(prop$tax_year)
prop$tax_year[is.na(prop$tax_year)] <- 2015  
summary(prop$tax_year)


#41.tax_land         


summary(prop$tax_land  )
prop$tax_land[is.na(prop$tax_land)] <- 278335            


#42.tax_property


summary(prop$tax_property  )
prop$tax_property[is.na(prop$tax_property)] <- 5984            



#43.area_live_finished


summary(prop$area_live_finished  )
prop$area_live_finished[is.na(prop$area_live_finished)] <- 1745            



#44.pool_wo_spa    

summary(prop$pool_wo_spa  )
prop <- subset( prop, select = -pool_wo_spa   )


#45.num_pool       

summary(prop$num_pool  )
prop <- subset( prop, select = -num_pool   )


#46. the rest will be replaced with the mean

prop$latitude[is.na(prop$latitude)] <- 34005411
prop$longitude[is.na(prop$longitude)] <- -118198868
prop$area_lot[is.na(prop$area_lot)] <- 29110  
prop$zoning_landuse  [is.na(prop$zoning_landuse  )] <- 261.8  
prop$rawcensustractandblock  [is.na(prop$rawcensustractandblock  )] <- 60491795     
prop$region_city       [is.na(prop$region_city       )] <- 33761     
prop$region_county    [is.na(prop$region_county    )] <- 2525     
prop$region_neighbor   [is.na(prop$region_neighbor   )] <- 190647     



#prop$year <- rep(2016,nrow(prop)) # make new column 


prop <- subset(prop, select=c(parcelid,  structuretaxvaluedollarcnt,taxamount,
                              finishedsquarefeet12,calculatedfinishedsquarefeet,
                              taxvaluedollarcnt,latitude,regioinidzip,lotsizesquarefeet,
                              yearbuilt))


train <- subset(train,!(train$logerror > quantile(train$logerror, probs=c(.01, .99))[2] | train$logerror < quantile(train$logerror, probs=c(.01, .91))[1]) ) 



results<-NULL


setkey(prop, parcelid)
setkey(train, parcelid)

# join on parcelid
training <- prop[train]

###################
# xgboost set up
###################

target <- training$logerror 

dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]

feature_names <- names(dtrain)

dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)


################
# Final model
################
param <- list(
  objective="reg:linear",
  eval_metric = "mae",
  eta = .004,
  max_depth = 4,
  min_child_weight = 15,
  subsample = 0.7,
  colsample_bytree = 0.5
)
xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     #nrounds=nrounds,
                     nrounds=3000,
                     #verbose=1,
                     print_every_n = 5)
                     
###############
# Results
###############
                     
# Feature Importance
importance_matrix <- xgb.importance(feature_names,model=xgb_mod)
xgb.plot.importance(importance_matrix[1:20,])

# Predict
preds <- predict(xgb_mod,dtest)

# For now, use same predictions for each time period. 
results$parcelid<- prop$parcelid
results$`201610`<-preds
                      
training$transactiondate<-"2016-11-01"
dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]
dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)
xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     #nrounds=nrounds,
                     nrounds=3000,
                     #verbose=1,
                     print_every_n = 5)
preds <- predict(xgb_mod,dtest)
results$`201611`<-preds

training$transactiondate<-"2016-12-01"
dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]
dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)
xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     #nrounds=nrounds,
                     nrounds=3000,
                     #verbose=1,
                     print_every_n = 5)
preds <- predict(xgb_mod,dtest)
results$`201612`<-preds

training$transactiondate<-"2017-10-01"
dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]
dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)
xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     #nrounds=nrounds,
                     nrounds=3000,
                     #verbose=1,
                     print_every_n = 5)
preds <- predict(xgb_mod,dtest)
results$`201710`<-preds

training$transactiondate<-"2017-11-01"
dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]
dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)
xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     #nrounds=nrounds,
                     nrounds=3000,
                     #verbose=1,
                     print_every_n = 5)
preds <- predict(xgb_mod,dtest)
results$`201711`<-preds

training$transactiondate<-"2017-12-01"
dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]
dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)
xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     #nrounds=nrounds,
                     nrounds=3000,
                     #verbose=1,
                     print_every_n = 5)
preds <- predict(xgb_mod,dtest)
results$`201712`<-preds


#Write results to csv
fwrite(results, file='../submissionxgboostMine38.csv', row.names=FALSE)
