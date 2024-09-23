
library(dplyr)
library(Cubist)

set_1_training_3 <- read_csv("C:/Users/clemene/OneDrive - LVWATER - Las Vegas Valley Water District/ML_data_clean_training.csv")
testregression <- read_csv("C:/Users/clemene/OneDrive - LVWATER - Las Vegas Valley Water District/ML_data_clean_test.csv")

trainregression <- na.omit(set_1_training_3)
testregression <- na.omit(testregression)



# change facility and type to string 
testregression2 <- mutate(testregression, Facility_letter = ifelse(Facility == 1, "a",
                                                                  ifelse(Facility == 2, "b",
                                                                         ifelse(Facility == 3, "c",
                                                                                ifelse(Facility == 4, "d",
                                                                                       ifelse(Facility == 5, "e",
                                                                                              ifelse(Facility == 6, "f",
                                                                                                     ifelse(Facility == 7, "g",
                                                                                                            ifelse(Facility == 8, "h", NA)))))))))
testregression3 <- mutate(testregression2, type_letter = ifelse(type == 1, "HFUF",
                                                                   ifelse(type == 2, "CC",
                                                                          ifelse(type == 3, "HFUF_CC", NA))))

trainregression2 <- mutate(trainregression, Facility_letter = ifelse(Facility == 1, "a",
                                                                   ifelse(Facility == 2, "b",
                                                                          ifelse(Facility == 3, "c",
                                                                                 ifelse(Facility == 4, "d",
                                                                                        ifelse(Facility == 5, "e",
                                                                                               ifelse(Facility == 6, "f",
                                                                                                      ifelse(Facility == 7, "g",
                                                                                                             ifelse(Facility == 8, "h", NA)))))))))

trainregression3 <- mutate(trainregression2, type_letter = ifelse(type == 1, "HFUF",
                                                               ifelse(type == 2, "CC",
                                                                      ifelse(type == 3, "HFUF_CC", NA))))

### Model 93: cubist

#facility and method (HFUF or CC) string
set.seed(1)
train_cubist <- train(recovery ~ old.recovery +	Facility_letter + type_letter + storage.time.days +	CP56_nc	+ HF183_nc + HF183.esv + AdV.esv + EnT.esv + NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + 
                        AdV_nc +	EnT_nc +	NoV.GIA_nc +	NoV.GIB_nc +	NoV.GII_nc +	PMMoV_nc +	 
                        PMMoV.nc.Hamza +	T_avg_F	+ HF183.qc +	AdV.qc +	EnT.qc + NoV.GIA.qc +	NoV.GIB.qc	+ NoV.GII.qc, 
                      method = "cubist", 
                      data = trainregression3)

y_hat_cubist_reg <- predict(train_cubist, testregression3)
a<-postResample(pred = y_hat_cubist_reg, obs = testregression3$recovery)


table_recov_all_cubist <- tibble(Abb. = "cubist", 
                                 Preprocessing = "both string",
                                 `Tune Grid` = "Default",
                                 `Variables Excluded` = "None",
                                 `RMSE` = toString(a[1]), 
                                 `R^2` = toString(a[2]),
                                 `MAE` = toString(a[3]))


table_cubist_sum <-bind_rows(table_cubist_sum, table_recov_all_cubist)

imp<-varImp(train_cubist)
imp[1:1] 

#facility and method (HFUF or CC) string, excluding HF183.qc, NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + 
set.seed(1)
train_cubist <- train(recovery ~ old.recovery +	Facility_letter + type_letter + storage.time.days +	CP56_nc	+ HF183_nc + HF183.esv + AdV.esv + EnT.esv + 
                        AdV_nc +	EnT_nc +	NoV.GIA_nc +	NoV.GIB_nc +	NoV.GII_nc +	PMMoV_nc +	 
                        PMMoV.nc.Hamza +	T_avg_F	+ AdV.qc +	EnT.qc + NoV.GIA.qc +	NoV.GIB.qc	+ NoV.GII.qc, 
                      method = "cubist", 
                      data = trainregression3)

y_hat_cubist_reg <- predict(train_cubist, testregression3)
a<-postResample(pred = y_hat_cubist_reg, obs = testregression3$recovery)


table_recov_all_cubist <- tibble(Abb. = "cubist", 
                                 Preprocessing = "both string",
                                 `Tune Grid` = "Default",
                                 `Variables Excluded` = "NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + HF183.qc",
                                 `RMSE` = toString(a[1]), 
                                 `R^2` = toString(a[2]),
                                 `MAE` = toString(a[3]))


table_cubist_sum <-bind_rows(table_cubist_sum, table_recov_all_cubist)
# equally accurate without NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + HF183.qc
imp<-varImp(train_cubist)
imp[1:1] #AdV.qc is least important 

#facility and method (HFUF or CC) string, excluding AdV.qc +	HF183.qc, NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + 
set.seed(1)
train_cubist <- train(recovery ~ old.recovery +	Facility_letter + type_letter + storage.time.days +	CP56_nc	+ HF183_nc + HF183.esv + AdV.esv + EnT.esv + 
                        AdV_nc +	EnT_nc +	NoV.GIA_nc +	NoV.GIB_nc +	NoV.GII_nc +	PMMoV_nc +	 
                        PMMoV.nc.Hamza +	T_avg_F	+ EnT.qc + NoV.GIA.qc +	NoV.GIB.qc	+ NoV.GII.qc, 
                      method = "cubist", 
                      data = trainregression3)

y_hat_cubist_reg <- predict(train_cubist, testregression3)
a<-postResample(pred = y_hat_cubist_reg, obs = testregression3$recovery)


table_recov_all_cubist <- tibble(Abb. = "cubist", 
                                 Preprocessing = "both string",
                                 `Tune Grid` = "Default",
                                 `Variables Excluded` = "AdV.qc + NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + HF183.qc",
                                 `RMSE` = toString(a[1]), 
                                 `R^2` = toString(a[2]),
                                 `MAE` = toString(a[3]))


table_cubist_sum <-bind_rows(table_cubist_sum, table_recov_all_cubist)
# better without AdV.qc
imp<-varImp(train_cubist)
imp[1:1] # NoV.GII.qc is least important 

export_data <- cbind(y_hat_cubist_reg, testregression3$recovery)


#facility and method (HFUF or CC) string, excluding NoV.GII.qc + AdV.qc +	HF183.qc, NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + 
set.seed(1)
train_cubist <- train(recovery ~ old.recovery +	Facility_letter + type_letter + storage.time.days +	CP56_nc	+ HF183_nc + HF183.esv + AdV.esv + EnT.esv + 
                        AdV_nc +	EnT_nc +	NoV.GIA_nc +	NoV.GIB_nc +	NoV.GII_nc +	PMMoV_nc +	 
                        PMMoV.nc.Hamza +	T_avg_F	+ EnT.qc + NoV.GIA.qc +	NoV.GIB.qc, 
                      method = "cubist", 
                      data = trainregression3)

y_hat_cubist_reg <- predict(train_cubist, testregression3)
a<-postResample(pred = y_hat_cubist_reg, obs = testregression3$recovery)


table_recov_all_cubist <- tibble(Abb. = "cubist", 
                                 Preprocessing = "both string",
                                 `Tune Grid` = "Default",
                                 `Variables Excluded` = "NoV.GII.qc + AdV.qc + NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + HF183.qc",
                                 `RMSE` = toString(a[1]), 
                                 `R^2` = toString(a[2]),
                                 `MAE` = toString(a[3]))


table_cubist_sum <-bind_rows(table_cubist_sum, table_recov_all_cubist)
# equal without NoV.GII.qc
imp<-varImp(train_cubist)
imp[1:1] # HF183_nc is least important 

# best model 

plot_data <- cbind(y_hat_cubist_reg, testregression3$recovery)
plot(plot_data)
# 0 neighbors and 20 committees 
export_data <- cbind(testregression3$Sample, y_hat_cubist_reg, testregression3$recovery)

write.csv(export_data, file = "SML_predictions_recov.csv")

##predicting data 
predictdata <- read_csv("C:/Users/clemene/OneDrive - LVWATER - Las Vegas Valley Water District/Pathogen_Data_to_be_estimated_ML 3.csv")

predictdata <- mutate(predictdata, Facility_letter = ifelse(Facility == 1, "a",
                                                                     ifelse(Facility == 2, "b",
                                                                            ifelse(Facility == 3, "c",
                                                                                   ifelse(Facility == 4, "d",
                                                                                          ifelse(Facility == 5, "e",
                                                                                                 ifelse(Facility == 6, "f",
                                                                                                        ifelse(Facility == 7, "g",
                                                                                                               ifelse(Facility == 8, "h", NA)))))))))

predictdata <- mutate(predictdata, type_letter = ifelse(type == 1, "HFUF",
                                                                  ifelse(type == 2, "CC",
                                                                         ifelse(type == 3, "HFUF_CC", NA))))

y_hat_cubist_pred <- predict(train_cubist, predictdata)
export_data2 <- cbind(predictdata$Sample, y_hat_cubist_pred)

write.csv(export_data2, file = "SML_predictions_estimation.csv")





#facility and method (HFUF or CC) string, excluding HF183_nc +  NoV.GII.qc + AdV.qc +	HF183.qc, NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + 
set.seed(1)
train_cubist <- train(recovery ~ old.recovery +	Facility_letter + type_letter + storage.time.days +	CP56_nc	+ HF183.esv + AdV.esv + EnT.esv + 
                        AdV_nc +	EnT_nc +	NoV.GIA_nc +	NoV.GIB_nc +	NoV.GII_nc +	PMMoV_nc +	 
                        PMMoV.nc.Hamza +	T_avg_F	+ EnT.qc + NoV.GIA.qc +	NoV.GIB.qc, 
                      method = "cubist", 
                      data = trainregression3)

y_hat_cubist_reg <- predict(train_cubist, testregression3)
a<-postResample(pred = y_hat_cubist_reg, obs = testregression3$recovery)


table_recov_all_cubist <- tibble(Abb. = "cubist", 
                                 Preprocessing = "both string",
                                 `Tune Grid` = "Default",
                                 `Variables Excluded` = "HF183_nc + NoV.GII.qc + AdV.qc + NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + HF183.qc",
                                 `RMSE` = toString(a[1]), 
                                 `R^2` = toString(a[2]),
                                 `MAE` = toString(a[3]))


table_cubist_sum <-bind_rows(table_cubist_sum, table_recov_all_cubist)
# worse without HF183_nc
imp<-varImp(train_cubist)
imp[1:1]



# hyperparameter tuning, facility and method (HFUF etc) string, 

grid <- expand.grid(committees = c(1, 10, 20, 30, 50, 100), neighbors = c(0, 1, 3, 5, 7, 9))
set.seed(1)
train_cubist <- train(recovery ~ old.recovery +	Facility_letter + type_letter + storage.time.days +	CP56_nc	+ HF183_nc + HF183.esv + AdV.esv + EnT.esv + 
                        AdV_nc +	EnT_nc +	NoV.GIA_nc +	NoV.GIB_nc +	NoV.GII_nc +	PMMoV_nc +	 
                        PMMoV.nc.Hamza +	T_avg_F	+ EnT.qc + NoV.GIA.qc +	NoV.GIB.qc, 
                      method = "cubist", 
                      tuneGrid = grid,
                      trControl = trainControl(method = "cv"),
                      data = trainregression3)

y_hat_cubist_reg <- predict(train_cubist, testregression3)
a<-postResample(pred = y_hat_cubist_reg, obs = testregression3$recovery)


table_recov_all_cubist <- tibble(Abb. = "cubist", 
                                 Preprocessing = "both string",
                                 `Tune Grid` = "Committee (1-100) and neightbor (0-9) tuning",
                                 `Variables Excluded` = "NoV.GII.qc + AdV.qc + NoV.GIA.esv + NoV.GIB.esv + NoV.GII.esv + PMMoV.esv + HF183.qc",
                                 `RMSE` = toString(a[1]), 
                                 `R^2` = toString(a[2]),
                                 `MAE` = toString(a[3]))


table_cubist_sum <-bind_rows(table_cubist_sum, table_recov_all_cubist)
# equal without NoV.GII.qc
imp<-varImp(train_cubist)
imp[1:1] # HF183_nc is least important 
# not helpful--optimal model had 20 committees and 0 neighbors 




