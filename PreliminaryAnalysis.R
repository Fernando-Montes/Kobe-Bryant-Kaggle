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
p <- ggplot(datawithshots, aes(x=game_date,y=game_id))
p + geom_point(aes(colour = shot_made_flag))
# 
# p <- ggplot(datawithshots, aes(x=minutes_remaining*60 + seconds_remaining, y=period))
# p + geom_point(aes(colour = shot_made_flag))
# 
# p <- ggplot(datawithshots, aes(x=loc_x, y=loc_y))
# p + geom_point(aes(colour = shot_made_flag))
# 
# p <- ggplot(datawithshots, aes(x=game_date,y=game_id))
# p + geom_point(aes(colour = shot_made_flag))
# 
# table(datawithshots$game_id)

# Defining new features ----------------------------------------------------

# Defining new feature dayssincestart
gamedate <- as.vector(unique(datawithshots$game_date))
gamedate <- sort(as.Date.character(gamedate))
dayssincestart <- as.numeric(gamedate - gamedate[1])

# Defining new feature timeleftingame
timeleftingame <- (4-ifelse(datawithshots$period>4, 4, datawithshots$period))*12 + datawithshots$minutes_remaining + as.integer(datawithshots$seconds_remaining/60)
timeintogame<-c()
for (x in seq_along(datawithshots$period)){
  if (datawithshots$period[x]<5) {
    tig<-datawithshots$period[x]*12 + (12-datawithshots$minutes_remaining[x]) +  (1-datawithshots$seconds_remaining[x]/60.)
    timeintogame<-c(timeintogame,as.integer(tig))
  }
  else {
    tig<-(48 + 5*(datawithshots$period[x]-5) + (5-datawithshots$minutes[x])) + 1 - datawithshots$seconds_remaining[x]/60.
    timeintogame<-c(timeintogame,as.integer(tig))
  }
}

# Defining new feature discrete positions
min_x <- min(datawithshots$loc_x)
min_y <- min(datawithshots$loc_y)
loc_x_disc <- as.integer((datawithshots$loc_x-min_x)/30)
loc_y_disc <- as.integer((datawithshots$loc_y-min_y)/30)

datawithshots <- cbind(datawithshots, timeleftingame, loc_x_disc, loc_y_disc, timeintogame)

# Creating percentage shots made for one feature ----------------------------

madeshots<-c()
takenshot<-c()

# Decide what parameter to use
# param<-"action_type"
# param<-"game_date"
# param<-"timeleftingame"
#param<-"playoffs"
param<-"matchup"

for(x in unique(datawithshots[[param]])){
    madeshots<-c(madeshots,sum(datawithshots$shot_made_flag[datawithshots[[param]]==x]))
    takenshot<-c(takenshot,sum(!is.na(datawithshots$shot_made_flag[datawithshots[[param]]==x])))
}
percentmade<-madeshots*100/takenshot
head(percentmade)

# Decide what to use on x-axis in the next plot
# x_axis <- as.numeric(unique(datawithshots$action_type))
# x_axis <- dayssincestart
# x_axis <-unique(datawithshots$timeleftingame)
x_axis <-unique(datawithshots$matchup)

library(ggpmisc)
# Plot of percentage shots done versus dayssincestart 
permade <- data.frame(xaxis = x_axis, percenmade = as.vector(percentmade))
permade <- permade[order(permade$xaxis),]
my.formula <- y ~ x

ggplot(permade, aes(x=xaxis, y=percenmade)) + 
  geom_point(shape = 1, col="blue") +
  xlab("Matchup") +
  ylab("shot made percentage") 
# +
#    geom_smooth(method=lm, formula = my.formula) +
#    stat_poly_eq(formula = my.formula,
#                 eq.with.lhs = "italic(h)~`=`~",
#                 eq.x.rhs = "~italic(z)",
#                 aes(label = ..eq.label..), 
#                 parse = TRUE)  #  (by default includes 95% confidence region)

# Creating percentage shots made for two features ---------------------------

madeshots<-matrix(data = 0, nrow = max(datawithshots$loc_y_disc), ncol = max(datawithshots$loc_x_disc))
takenshot<-matrix(data = 0, nrow = max(datawithshots$loc_y_disc), ncol = max(datawithshots$loc_x_disc))

param1<-"loc_x_disc"
param2<-"loc_y_disc"

for(i in 1:dim(datawithshots)[1]){
  takenshot[datawithshots$loc_y_disc[i], datawithshots$loc_x_disc[i]] = takenshot[datawithshots$loc_y_disc[i], datawithshots$loc_x_disc[i]] + 1
  if (datawithshots$shot_made_flag[i] == 1) madeshots[datawithshots$loc_y_disc[i], datawithshots$loc_x_disc[i]] = madeshots[datawithshots$loc_y_disc[i], datawithshots$loc_x_disc[i]] + 1
}
percentmade<-madeshots*100/takenshot
percentmade<-ifelse(is.nan(percentmade), 0, percentmade)

library(gplots) # for colorpanel()
library(lattice) # for levelplot()
levelplot(percentmade, scales=list(tck=0, x=list(rot=90)), 
          col.regions=colorpanel(8, "white", "blue"), at=c(0,10,20,30,40,45,50,60,70), 
          main="Court position shot percentage", xlab="Court length [/3 ft]", ylab="Court width [/3 ft]")


# Alternative ways to plot shots -----------------------
# ------------------------------------------------------

library(dplyr)
library(ggplot2)
data <- read.csv("data.csv", stringsAsFactors = TRUE)

train <- data[!is.na(data$shot_made_flag),]
test <- data[is.na(data$shot_made_flag),]

# train$shot_made_flag <- as.factor(train$shot_made_flag)
train$shot_made_flag <- factor(train$shot_made_flag, levels = c("1", "0"))

#a plot to see accuracy by feature
pplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = train, aes_q(x = feat)) +
    geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    ggtitle(paste("accuracy by", feat))
}

# a plot to see position of the shot by feature
courtplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = train, aes(x = lon, y = lat)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    ggtitle(paste(feat)) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +     
    theme_void() 
}
