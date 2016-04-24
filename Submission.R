# Assumes ML-Analysis.R has been run and all the variables are available 
# in the R environment
datawithoutshots <- subset(alldata, is.na(shot_made_flag) == TRUE)

my_sub <- datawithoutshots
my_sub_prediction_gbm <- predict(my_gbm, my_sub)

# Setting limits to probabilities ----------------------
my_sub_prediction_gbm_mod <- ifelse(my_sub_prediction_gbm > 1, 1, 
                                ifelse(my_sub_prediction_gbm < 0, 0, my_sub_prediction_gbm))

tobesub <- cbind(my_sub$shot_id, my_sub_prediction_gbm_mod)

write.csv(tobesub, file = "gbm_submission.csv", sep = " ", row.names = FALSE)