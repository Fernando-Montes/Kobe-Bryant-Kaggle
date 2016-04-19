alldata <- read.csv("data.csv")
datawithshots <- subset(alldata, is.na(shot_made_flag) == FALSE)

par(mfrow=c(2,1))
hist(datawithshots$game_id, main="Game_id", breaks = 2000)
hist(datawithshots$game_event_id, main="Game_event_id", breaks = 2000)
par(mfrow=c(1,1))

plot(x=datawithshots$game_id, y=datawithshots$game_event_id)
datawithshots$game_event_id[datawithshots$game_id==20000012]

library(ggplot2)
p <- ggplot(datawithshots, aes(x=action_type,y=combined_shot_type))
p + geom_point(aes(colour = shot_made_flag))

p <- ggplot(datawithshots, aes(x=minutes_remaining*60 + seconds_remaining, y=period))
p + geom_point(aes(colour = shot_made_flag))

p <- ggplot(datawithshots, aes(x=sqrt(loc_x^2 + loc_y^2),y=shot_distance))
p + geom_point(aes(colour = shot_made_flag))

p <- ggplot(datawithshots, aes(x=game_date,y=game_id))
p + geom_point(aes(colour = shot_made_flag))

table(datawithshots$game_id)

madeshots<-c()
takenshot<-c()
for(x in unique(datawithshots$game_date)){
  madeshots<-c(madeshots,sum(datawithshots$shot_made_flag[datawithshots$game_date==x]))
  takenshot<-c(takenshot,sum(!is.na(datawithshots$shot_made_flag[datawithshots$game_date==x])))
}

percentmade<-madeshots*100/takenshot

avgshotpercentage <- function(thres=0){
  newlist<-c()
  for(i in 1:length(takenshot)){
   if (takenshot[i] > thres){
     newlist<-c(newlist,percentmade[i])
   }
  }
  mean(newlist)
}

library(ggpmisc)
permade <- data.frame(time = unique(datawithshots$game_date), percenmade = as.vector(percentmade))
my.formula <- y ~ x
ggplot(permade, aes(x=time, y=percenmade)) + 
  geom_point(shape = 1, col="blue") +
  geom_smooth(method=lm, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               aes(label = ..eq.label..), 
               parse = TRUE) 
#  (by default includes 95% confidence region)

 