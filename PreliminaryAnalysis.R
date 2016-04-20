alldata <- read.csv("data.csv")
datawithshots <- subset(alldata, is.na(shot_made_flag) == FALSE)

# par(mfrow=c(2,1))
# hist(datawithshots$game_id, main="Game_id", breaks = 2000)
# hist(datawithshots$game_event_id, main="Game_event_id", breaks = 2000)
# par(mfrow=c(1,1))
# 
# plot(x=datawithshots$game_id, y=datawithshots$game_event_id)
# datawithshots$game_event_id[datawithshots$game_id==20000012]
# 
# library(ggplot2)
# p <- ggplot(datawithshots, aes(x=action_type,y=combined_shot_type))
# p + geom_point(aes(colour = shot_made_flag))
# 
# p <- ggplot(datawithshots, aes(x=minutes_remaining*60 + seconds_remaining, y=period))
# p + geom_point(aes(colour = shot_made_flag))
# 

p <- ggplot(datawithshots, aes(x=loc_x, y=loc_y))
p + geom_point(aes(colour = shot_made_flag))
# 
# p <- ggplot(datawithshots, aes(x=game_date,y=game_id))
# p + geom_point(aes(colour = shot_made_flag))
# 
# table(datawithshots$game_id)

# avgshotpercentage <- function(thres=0){
#   newlist<-c()
#   for(i in 1:length(takenshot)){
#    if (takenshot[i] > thres){
#      newlist<-c(newlist,percentmade[i])
#    }
#   }
#   mean(newlist)
# }

# shotpercentgate<-function(param){
#   madeshots<-c()
#   takenshot<-c()
#   for(x in unique(datawithshots$param)){
#     madeshots<-c(madeshots,sum(datawithshots$shot_made_flag[datawithshots$param==x]))
#     takenshot<-c(takenshot,sum(!is.na(datawithshots$shot_made_flag[datawithshots$param==x])))
#   }
#   percentmade<-madeshots*100/takenshot
#   head(percentmade)
# }

# Defining new feature dayssincestart
gamedate <- as.vector(unique(datawithshots$game_date))
gamedate <- sort(as.Date.character(gamedate))
dayssincestart <- as.numeric(gamedate - gamedate[1])

# Defining new feature timeleftingame
timeleftingame <- (4-ifelse(datawithshots$period>4, 4, datawithshots$period))*15 + datawithshots$minutes_remaining + as.integer(datawithshots$seconds_remaining/60)

# Defining new feature discrete positions
loc_x_disc <- as.integer(datawithshots$loc_x/10)
loc_y_disc <- as.integer(datawithshots$loc_y/10)

datawithshots <- cbind(datawithshots, timeleftingame, loc_x_disc, loc_y_disc)

madeshots<-c()
takenshot<-c()

# Decide what parameter to use
# param<-"action_type"
# param<-"game_date"
param<-"timeleftingame"

for(x in unique(datawithshots[[param]])){
    madeshots<-c(madeshots,sum(datawithshots$shot_made_flag[datawithshots[[param]]==x]))
    takenshot<-c(takenshot,sum(!is.na(datawithshots$shot_made_flag[datawithshots[[param]]==x])))
}
percentmade<-madeshots*100/takenshot
head(percentmade)

# Decide what to use on x-axis in the next plot
x_axis <- as.numeric(unique(datawithshots$action_type))
x_axis <- dayssincestart
x_axis <-unique(datawithshots$timeleftingame)

library(ggpmisc)
# Plot of percentage shots done versus dayssincestart 
permade <- data.frame(xaxis = x_axis, percenmade = as.vector(percentmade))
permade <- permade[order(permade$xaxis),]
my.formula <- y ~ x

ggplot(permade, aes(x=xaxis, y=percenmade)) + 
  geom_point(shape = 1, col="blue") +
  xlab("Time left in game") +
  ylab("shot made percentage") +
   geom_smooth(method=lm, formula = my.formula) +
   stat_poly_eq(formula = my.formula,
                eq.with.lhs = "italic(h)~`=`~",
                eq.x.rhs = "~italic(z)",
                aes(label = ..eq.label..), 
                parse = TRUE)  #  (by default includes 95% confidence region)

 