#Reminder to update all plots using ggplot2 to make more presentable for poster


#reading in data from a csv file
nhl_data <- read.csv("NHL-CombinedStats.csv", header=TRUE)

nhl_data
#splitting the data into multiple frames dependent on the Season
Season_2014 <- nhl_data[nhl_data$Season == 20142015,]

Season_2014

Season_2015 <- nhl_data[nhl_data$Season == 20152016,]
Season_2016 <- nhl_data[nhl_data$Season == 20162017,]
Season_2017 <- nhl_data[nhl_data$Season == 20172018,]
Season_2018 <- nhl_data[nhl_data$Season == 20182019,]

#quick look at overall change in penalty minutes
total_penalty_minutes_2014 <- sum(Season_2014$PIM)
total_penalty_minutes_2015 <- sum(Season_2015$PIM)
total_penalty_minutes_2016 <- sum(Season_2016$PIM)
total_penalty_minutes_2017 <- sum(Season_2017$PIM)
total_penalty_minutes_2018 <- sum(Season_2018$PIM)

penalty_minutes <- data.frame(c(2014,2015,2016,2017,2018), c(total_penalty_minutes_2014,total_penalty_minutes_2015,total_penalty_minutes_2016,
                                total_penalty_minutes_2017, total_penalty_minutes_2018))

penalty_minutes
help("plot.data.frame")
plot(penalty_minutes, xlab ="Season", ylab = "Total Penalty Minutes")
ggplot(penalty_minutes, aes(x=penalty_minutes[,1],y=penalty_minutes[,2])) + geom_bar(stat = "identity",fill = "darkblue") + labs(x="Season",y="Total Penalty Minutes")


library(ggplot2)

#look at totals and change in 5-minute majors
#aka number of fights as this is how I am tracking them
majors_2014 <- sum(Season_2014$Major)
majors_2015 <- sum(Season_2015$Major)
majors_2016 <- sum(Season_2016$Major)
majors_2017 <- sum(Season_2017$Major)
majors_2018 <- sum(Season_2018$Major)

total_majors <- data.frame(c(2014,2015,2016,2017,2018), c(majors_2014,majors_2015,majors_2016,majors_2017,majors_2018))
total_majors

plot(total_majors, xlab = "Season", ylab = "Total Majors")
ggplot(total_majors, aes(x=total_majors[,1],y=total_majors[,2])) + geom_bar(stat = "identity",fill = "darkblue") + labs(x="Season",y="Total Majors")

#look at total scoring(so total points goals + assists)
scoring_2014 <- sum(Season_2014$P)
scoring_2015 <- sum(Season_2015$P)
scoring_2016 <- sum(Season_2016$P)
scoring_2017 <- sum(Season_2017$P)
scoring_2018 <- sum(Season_2018$P)

total_scoring <- data.frame(c(2014,2015,2016,2017,2018), c(scoring_2014,scoring_2015,scoring_2016,scoring_2017,scoring_2018))
total_scoring

plot(total_scoring, xlab = "Season", ylab = "Total Scoring")
ggplot(total_scoring, aes(x=total_scoring[,1],y=total_scoring[,2])) + geom_bar(stat = "identity",fill = "darkblue") + labs(x="Season",y="Total Scoring")+coord_cartesian(ylim=c(15000,22000))

#might look at 2015 season more seems like an outlier where penalties increased yet total scoring decreased
#quick check to see if an increase in shots corresponds to increase in scoring
shooting_2014 <- sum(Season_2014$S)
shooting_2015 <- sum(Season_2015$S)
shooting_2016 <- sum(Season_2016$S)
shooting_2017 <- sum(Season_2017$S)
shooting_2018 <- sum(Season_2018$S)

total_shooting <- data.frame(c(2014,2015,2016,2017,2018), c(shooting_2014,shooting_2015,shooting_2016,shooting_2017,shooting_2018))
total_shooting

plot(total_shooting, xlab = "Season", ylab = "Total Shooting")
ggplot(total_shooting, aes(x=total_shooting[,1],y=total_shooting[,2])) + geom_bar(stat = "identity",fill = "darkblue") + labs(x="Season",y="Total Shots on Net")

#filtering out all players with less than 20 games played as that could mess up the model

Season_2014 <- Season_2014[Season_2014$GP >= 20,]
Season_2015 <- Season_2015[Season_2015$GP >= 20,]
Season_2016 <- Season_2016[Season_2016$GP >= 20,]
Season_2017 <- Season_2017[Season_2017$GP >= 20,]
Season_2018 <- Season_2018[Season_2018$GP >= 20,]

#classifying whether or not a player is a fighter
majors <- nhl_data[nhl_data$Major != 0,]
summary(majors$Major)
majors$Major


Season_2014$Major <- cut(Season_2014$Major, br=c(-1,2,22), labels = c("Not", 'Fighter'))
Season_2015$Major <- cut(Season_2015$Major, br=c(-1,2,22), labels = c("Not", 'Fighter'))
Season_2016$Major <- cut(Season_2016$Major, br=c(-1,2,22), labels = c("Not", 'Fighter'))
Season_2017$Major <- cut(Season_2017$Major, br=c(-1,2,22), labels = c("Not", 'Fighter'))
Season_2018$Major <- cut(Season_2018$Major, br=c(-1,2,22), labels = c("Not", 'Fighter'))

#checking change in number of fighters
fighter_2014 <- table(Season_2014$Major)
fighter_2015 <- table(Season_2015$Major)
fighter_2016 <- table(Season_2016$Major)
fighter_2017 <- table(Season_2017$Major)
fighter_2018 <- table(Season_2018$Major)


total_fighter <- data.frame(c(2014,2015,2016,2017,2018), c(fighter_2014["Fighter"],fighter_2015["Fighter"],fighter_2016["Fighter"],fighter_2017["Fighter"],fighter_2018["Fighter"]))
total_fighter

plot(total_fighter, xlab = "Season", ylab = "Total # of Fighters")
ggplot(total_fighter, aes(x=total_fighter[,1],y=total_fighter[,2])) + geom_bar(stat = "identity",fill = "darkblue") + labs(x="Season",y="Number of Fighters")

#building a random forest model to try and predict fighter or not
#first building a training set

set.seed(1955)

modified <- nhl_data[nhl_data$GP >= 20,]
modified$Major <- cut(modified$Major, br=c(-1,2,22), labels = c("Not", 'Fighter'))

train <- sample(nrow(modified), 0.7*nrow(modified),replace = FALSE)
TrainSet <- modified[train,]
ValidSet <- modified[-train,]

str(TrainSet)
#building the first model
library(randomForest)
initial_model <- randomForest(Major ~ P + X... + PIM + S + Minor,data=TrainSet, 
                              importance = TRUE,mtry = 5, ntree = 800)

initial_model

predTrain <- predict(initial_model, TrainSet, type = "class")
predtable <- table(predTrain, TrainSet$Major)
sum(diag(predtable))/sum(predtable)*100

predValid <- predict(initial_model, ValidSet, type = "class")
validtable <- table(predValid, ValidSet$Major)
sum(diag(validtable))/sum(validtable)*100

#quite good results
#checking importance

importance(initial_model)
varImpPlot(initial_model)

#using loop to test multiple mtry values
a = c()
i = 3
for (i in 3:5) {
  model2 <- randomForest(Major ~ P + X... + PIM + S + Minor, data = TrainSet, 
                         importance = TRUE, mtry = i, ntree = 800)
  predValid2 <- predict(model2, ValidSet, type="class")
  a[i-2] = mean(predValid2 == ValidSet$Major)
}
a
plot(3:5,a)

#looks like initial model is all good.

#second model adding games played
model2 <- randomForest(Major ~ GP + P + X... + PIM + S + Minor,data=TrainSet, 
                              importance = TRUE,mtry = 5, ntree = 800)

model2

predTrain2 <- predict(model2, TrainSet, type = "class")
predtable <- table(predTrain2, TrainSet$Major)
sum(diag(predtable))/sum(predtable)*100

predValid2 <- predict(model2, ValidSet, type = "class")
validtable <- table(predValid2, ValidSet$Major)
sum(diag(validtable))/sum(validtable)*100

#testing different mtry
a = c()
i = 3
for (i in 3:6) {
  model2 <- randomForest(Major ~ GP + P + X... + PIM + S + Minor, data = TrainSet, 
                         importance = TRUE, mtry = i, ntree = 800)
  predValid2 <- predict(model2, ValidSet, type="class")
  a[i-2] = mean(predValid2 == ValidSet$Major)
}
a
plot(3:6,a)

#retry with higher mtry
model2 <- randomForest(Major ~ GP + P + X... + PIM + S + Minor,data=TrainSet, 
                       importance = TRUE,mtry = 6, ntree = 800)

model2

predTrain2 <- predict(model2, TrainSet, type = "class")
predtable <- table(predTrain2, TrainSet$Major)
sum(diag(predtable))/sum(predtable)*100

predValid2 <- predict(model2, ValidSet, type = "class")
validtable <- table(predValid2, ValidSet$Major)
sum(diag(validtable))/sum(validtable)*100


#Only PIM 
PIM_model <- randomForest(Major ~ GP + P + X... + PIM + S,data=TrainSet, 
                       importance = TRUE,mtry = 5, ntree = 800)

PIM_model

predTrain3 <- predict(PIM_model, TrainSet, type = "class")
predtable3 <- table(predTrain3, TrainSet$Major)
sum(diag(predtable3))/sum(predtable3)*100

predValid3 <- predict(PIM_model, ValidSet, type = "class")
validtable3 <- table(predValid3, ValidSet$Major)
sum(diag(validtable3))/sum(validtable3)*100


#Only Minor
Minor_model <- randomForest(Major ~ GP + P + X... + Minor + S,data=TrainSet, 
                          importance = TRUE,mtry = 5, ntree = 800)

Minor_model

predTrain4 <- predict(Minor_model, TrainSet, type = "class")
predtable4 <- table(predTrain4, TrainSet$Major)
sum(diag(predtable4))/sum(predtable4)*100

predValid4 <- predict(Minor_model, ValidSet, type = "class")
validtable4 <- table(predValid4, ValidSet$Major)
sum(diag(validtable4))/sum(validtable4)*100


#Replacing points with goals and assists
str(nhl_data)
model3 <- randomForest(Major ~ GP + G + A + X... + PIM + S + Minor,data=TrainSet, 
                       importance = TRUE,mtry = 6, ntree = 800)

model3

predTrain5 <- predict(model3, TrainSet, type = "class")
predtable5 <- table(predTrain5, TrainSet$Major)
sum(diag(predtable5))/sum(predtable5)*100

predValid5 <- predict(model3, ValidSet, type = "class")
validtable5 <- table(predValid5, ValidSet$Major)
sum(diag(validtable5))/sum(validtable5)*100


#testing new mtry
a = c()
i = 3
for (i in 3:7) {
  model3 <- randomForest(Major ~ GP + G + A + X... + PIM + S + Minor, data = TrainSet, 
                         importance = TRUE, mtry = i, ntree = 800)
  predValid3 <- predict(model3, ValidSet, type="class")
  a[i-2] = mean(predValid3 == ValidSet$Major)
}
a
plot(3:7,a)

#testing on season data
season_2014 <- predict(model2, Season_2014, type = "class")
predtable2014 <- table(season_2014, Season_2014$Major)
sum(diag(predtable2014))/sum(predtable2014)*100

season_2015 <- predict(model2, Season_2015, type = "class")
predtable2015 <- table(season_2015, Season_2015$Major)
sum(diag(predtable2015))/sum(predtable2015)*100

season_2016 <- predict(model2, Season_2016, type = "class")
predtable2016 <- table(season_2016, Season_2016$Major)
sum(diag(predtable2016))/sum(predtable2016)*100

season_2017 <- predict(model2, Season_2017, type = "class")
predtable2017 <- table(season_2017, Season_2017$Major)
sum(diag(predtable2017))/sum(predtable2017)*100

season_2018 <- predict(model2, Season_2018, type = "class")
predtable2018 <- table(season_2018, Season_2018$Major)
sum(diag(predtable2018))/sum(predtable2018)*100





