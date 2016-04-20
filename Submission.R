# Assumes ML-Analysis.R has been run and all the variables are available 
# in the R environment
datawithoutshots <- subset(alldata, is.na(shot_made_flag) == TRUE)

# Defining new feature dayssincestart
gamedate <- as.vector(datawithoutshots$game_date)
gamedate <- as.Date.character(gamedate)
dayssincestart <- as.numeric(gamedate - as.Date.character(min(gamedate)))

# Defining new feature timeleftingame
timeleftingame <- (4-ifelse(datawithoutshots$period>4, 4, datawithoutshots$period))*15 + datawithoutshots$minutes_remaining + as.integer(datawithoutshots$seconds_remaining/60)

# Defining new feature discrete positions
loc_x_disc <- as.integer((datawithoutshots$loc_x-min_x)/30)
loc_y_disc <- as.integer((datawithoutshots$loc_y-min_y)/30)

datawithoutshots <- cbind(datawithoutshots, timeleftingame, loc_x_disc, loc_y_disc, dayssincestart)
datawithoutshots <- datawithoutshots[,!names(datawithoutshots) %in% c("team_name", "team_id", "game_id")]

my_sub <- datawithoutshots
my_sub_prediction_gbm <- predict(my_gbm, my_sub)

tobesub <- cbind(my_sub$shot_id, my_sub_prediction_gbm)

write.csv(tobesub, file = "gbm_submission.csv", sep = " ", row.names = FALSE)