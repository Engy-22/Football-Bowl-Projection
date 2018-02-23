# Prior to the college football bowl season, I thought it would be an interesting task to create a college
# football bowl game score prediction algorithm.  Unlike in professional sports, where sample sizes are large
# and the likelihood of teams playing a significant number of other teams over the course of the season, college
# football gives us a relatively small number of data points to go by.  Teams in the FBS generally play 10-13 
# games over the course of the season against other FBS schools; given that there are over 120 FBS teams in the 
# NCAA, that means that each team will only play approximately 10% of all other teams that they will be ranked
# against.  The very nature of this has lead to much controversy over the years, and makes it difficult to 
# determine which teams are better than others simply based on win and loss totals.

# There are no advanced football metrics in any of the data used here.  All of the information provided is related
# to date of games played, where the games are played, the score, and the teams that are playing.


# Installing (if necessary) and loaded the required packages

if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"rvest" %in% installed.packages()) install.packages("rvest")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"sqldf" %in% installed.packages()) install.packages("sqldf")
if (!"caret" %in% installed.packages()) install.packages("caret")

require(tidyverse)
require(rvest)
require(lubridate)
require(sqldf)
require(caret)

# MasseyRatings.com is an excellent source of scoring data for a wide variety of sports.  We'll read in college 
# football scores from every week in 2017 from this site and combine it into one data.frame.

base.url <- "https://www.masseyratings.com/scores.php?s=295489&sub=12801&dt=2017"
dates <- c("0903", "0910", "0917", "0924", "1001", "1008", "1015", "1022", "1029", "1105", "1112", "1119", "1126", "1203", "1211")

url.list <- paste(base.url, dates, sep = "")

allweeks <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 1))

for (i in 1:length(dates)) {
      current.week <- read_lines(url.list[i], skip = 9)
      current.week <- gsub("<.*?>", "", current.week)
      current.week <- data.frame(current.week, stringsAsFactors = FALSE)
      allweeks <- rbind(allweeks, current.week)
}

#  Cleaning and normalizing the data

teams <- readLines("https://www.masseyratings.com/cf/compare.htm", skip = 5)
teams <- gsub("<.*?>", "", teams)
teams <- teams[76:241]
teams <- gsub("\\d+", "", teams)
teams <- gsub("\\s", "", teams)
teams <- teams[-which(teams == "")]
teams <- teams[-c(11,22,33,44,55,66,77,88,99,110,121,132)]
teams <- sub("-.*", "", teams)
teams <- sub("P$|B$|MWC|CUSA|MAC|SBC|AAC|ACC|SEC|FBSI|MWC", "", teams)
teams <- sub("[[:punct:]]", "", teams)

rm(current.week); rm(url.list); rm(dates); rm(base.url); rm(i)

# Because the data is stored on website that I am not affiliated with, I have gone ahead and saved the current
# workspace to a local directory so that subsequent re-running of the code doesn't ping the 3rd party website.

# Loading the workspace, as needed

load("C:/Users/Bryan/Google Drive/CFB2017/data2017.RData")

# Continuing to clean the data by removing extraneous characters and whitespace

allweeks <- apply(allweeks, 2, as.character)
allweeks <- trimws(allweeks)
allweeks <- data.frame(allweeks, stringsAsFactors = FALSE)
allweeks <- data.frame(apply(allweeks, 1, function(x) gsub("\\s+", " ", x)), stringsAsFactors = FALSE)
names(allweeks) <- c("gameinfo")
allweeks$gameinfo <- sub(" P$", "", allweeks$gameinfo)

# Feature generation; separating game data (such as score, location, number of overtimes, etc) into multiple fields 

allweeks <- allweeks %>% mutate(gamedate = substr(gameinfo, 1, 11),
                                result = substr(gameinfo, 12, length(gameinfo)))
allweeks <- allweeks %>% mutate(num.ots = as.numeric(str_sub(str_extract(allweeks$result, "O[0-9]"), 2, 2)),
                                result = gsub(" O[0-9]", "", result),
                                n.loc = unlist(lapply(1:nrow(allweeks), function(x) trimws(unlist(str_split(allweeks$result[x], str_match(allweeks$result[x], ".*[0-9]+"))))[2])))                
allweeks$result <- as.character(allweeks$result)
allweeks <- allweeks %>% mutate(homegame = ifelse(str_sub(result, 1, 1) == "@", 1, 0))

# Removing additional extraneous information and characters

allweeks$result <- gsub("PAC 12", "", allweeks$result, ignore.case = TRUE)
allweeks$result <- gsub("BIG 12", "", allweeks$result, ignore.case = TRUE)
allweeks$result <- gsub("BIG 10", "", allweeks$result, ignore.case = TRUE)
allweeks$result <- gsub("@|\\s|\\.|\\'|-|\\&", "", allweeks$result)
allweeks$result <- str_match(allweeks$result, "^[:alpha:]+[0-9]+[:alpha:]+[0-9]+")
allweeks$result <- as.character(allweeks$result)
remove <- which(is.na(allweeks$result))
allweeks <- allweeks[-remove,]
rm(remove)
allweeks$num.ots[which(is.na(allweeks$num.ots) == TRUE)] <- 0
allweeks$n.loc <- ifelse(allweeks$n.loc == "", 0, 1)

# Determining the winning and losing team and setting that to factor variables

allweeks <- allweeks %>% mutate(winning = str_extract(allweeks$result, "^[:alpha:]+\\d+"),
                                losing = str_extract(allweeks$result, "[:alpha:]+\\d+$"))
allweeks <- within(allweeks, rm(gameinfo, result))

# Because of the way that aggregating functions work, it is necessary to recombine all of the results with 
# themselves in order to more easily obtain aggregate totals.  For example, one record will capture the following
# information:  

# Team A scored 42 points, Team B scored 14 points, Team A was the home team, Team A was the winning team and 
# Team B was the losing team.  

# However, when generating aggregate team data, it is easier to have all teams and all games represented in each 
# feature field, so the code below will the state the above data in a slightly different way to represent the same 
# result but in a functionally different way.

# Team B scored 14 points, Team A scored 42 points, Team B was NOT the home team, Team B was the losing team and 
# Team A was the winning team.

allweeks <- allweeks %>% mutate(teamresult = ifelse(homegame == 1, winning, losing),
                                oppresult = ifelse(homegame == 0, winning, losing))
allweeks <- within(allweeks, rm(winning, losing, homegame))
allweeks$home <- 1
duplicate <- allweeks
duplicate <- duplicate[,c(1,2,3,5,4,6)]
names(duplicate) <- names(allweeks)
duplicate$home <- 0
allweeks <- rbind(allweeks, duplicate)
rm(duplicate)

# Continuing to clean/separate/format the data in separate fields

allweeks <- allweeks %>% mutate(team = str_extract(teamresult, "^[:alpha:]+"),
                                teamscore = str_extract(teamresult, "\\d+$"),
                                opp = str_extract(oppresult, "^[:alpha:]+"),
                                oppscore = str_extract(oppresult, "\\d+$"))
allweeks$teamscore <- as.integer(allweeks$teamscore)
allweeks$oppscore <- as.integer(allweeks$oppscore)
allweeks <- within(allweeks, rm(teamresult, oppresult))

# Games that go into overtime are rare but have a tendency to skew final game scores to be much higher
# than games that end in regulation.  Because variance in score increases as the number of individual scores
# (touchdowns and field goals) increase, lower scoring games tend to end in ties more often than higher scoring
# games.  As such, scores in overtime games need to be normalized to a total that better reflects how the
# game actually played out.

allweeks <- allweeks %>% mutate(teamscore = ifelse(num.ots > 0, teamscore - 7*(num.ots - 1), teamscore),
                                oppscore = ifelse(num.ots > 0, oppscore - 7*(num.ots - 1), oppscore))
allweeks <- within(allweeks, rm(num.ots))
allweeks <- allweeks %>% mutate(win = ifelse(teamscore > oppscore, 1, 0),
                                loss = ifelse(teamscore < oppscore, 1, 0))
fbsgames <- intersect(which(allweeks$team %in% teams), which(allweeks$opp %in% teams))
allweeks <- allweeks[fbsgames,]
rm(fbsgames)

# Feature engineering.  Simply going by wins, losses, and scores against opponents without any indication of how 
# that opponent normally plays against other teams doesn't provide much insight.  Features need to be created 
# that give more information about how good each opponent actually is; these features include average points scored, 
# average points scored against, winning percentage, average margin of victory, etc.  These team stats are
# normalized and unusable data will be imputed with average values from all other teams.

teamstats <- allweeks %>% 
      group_by(team) %>% 
      summarize(team.avg.point = mean(teamscore), 
                team.sd.point = sd(teamscore), 
                opp.avg.point = mean(oppscore), 
                opp.sd.point = sd(oppscore), 
                score.diff.avg = mean(teamscore - oppscore), 
                score.diff.sd = sd(teamscore - oppscore), 
                wins = sum(win),
                losses = sum(loss),
                winpct = wins/(wins+losses))

nalist <- which(colSums(is.na(teamstats)) > 0)
cm <- colMeans(teamstats[,nalist], na.rm = TRUE)
teamstats$team.sd.point[is.na(teamstats$team.sd.point)] <- cm[1]
teamstats$opp.sd.point[is.na(teamstats$opp.sd.point)] <- cm[2]
teamstats$score.diff.sd[is.na(teamstats$score.diff.sd)] <- cm[3]
teamstats <- teamstats %>% mutate(normalteamscore = pnorm(as.numeric(team.avg.point), mean = mean(teamstats$team.avg.point), sd = sd(teamstats$team.avg.point)),
                                  normaloppscore = pnorm(as.numeric(opp.avg.point), mean = mean(teamstats$opp.avg.point), sd = sd(teamstats$opp.avg.point)),
                                  normalscorediff = pnorm(as.numeric(score.diff.avg), mean = mean(score.diff.avg), sd = sd(teamstats$score.diff.avg))) 

# Merging team and opponent feature data into main dataset


teamschedule <- select(allweeks, team, opp)
teamschedule <- teamschedule[order(teamschedule$team),]
teamschedule <- sqldf("SELECT a.team, a.opp, b.opp FROM teamschedule a, teamschedule b WHERE a.opp = b.team")
names(teamschedule) <- c("team", "opp", "oppopp")
oppstats <- sqldf("SELECT * FROM teamschedule, teamstats WHERE teamschedule.oppopp = teamstats.team")
oppstats <- oppstats[,-c(3:4,13:15)]
names(oppstats) <- c("team", "opp", "oppavgpoint", "oppsdpoint", "oppoppavgpoint", "oppoppsdpoint", "oppscorediffavg", "oppscorediffsd", "oppwins", "opplosses", "oppwinpct")

# In the same way that it's extremely useful to combine team and opponent aggregate data into the main dataset, it's
# also very important to find out just how well the opponents' opponents stack up, so we will create the same type
# of features for opponents' opponents as we did in the steps above and merge that data into the main dataset.

oppstatsagg <- oppstats %>%
      group_by(opp) %>%
      summarize(oppaggavgpoint = mean(oppavgpoint),
                oppaggsdpoint = sd(oppavgpoint),
                oppaggavgpoint = mean(oppoppavgpoint),
                oppaggscorediffavg = mean(oppscorediffavg),
                oppaggavgscorediffsd = mean(oppscorediffsd),
                aggavgoppwin = mean(oppwins),
                aggavgopploss = mean(opplosses),
                aggavgwinpct = mean(oppwinpct))
rm(teamschedule)

nalist <- which(colSums(is.na(oppstatsagg)) > 0)
cm <- colMeans(oppstatsagg[,nalist], na.rm = TRUE)
oppstatsagg$oppaggsdpoint[is.na(oppstatsagg$oppaggsdpoint)] <- cm

oppstatsagg <- oppstatsagg %>% mutate(normaloppaggavgpoint = pnorm(as.numeric(oppaggavgpoint), mean = mean(oppaggavgpoint), sd = sd(oppaggavgpoint)),
                                      normaloppaggscorediffavg = pnorm(oppaggscorediffavg), mean = mean(oppaggscorediffavg), sd = sd(oppaggscorediffavg),
                                      normalaggavgwinpct =pnorm(as.numeric(aggavgwinpct), mean = mean(aggavgwinpct), sd = sd(aggavgwinpct)))
oppstatsagg <- oppstatsagg[,-c(11,12)]
rm(nalist); rm(cm); rm(oppstats)
teamstats <- sqldf("SELECT * FROM oppstatsagg, teamstats WHERE oppstatsagg.opp = teamstats.team")
teamstats <- teamstats[,-12]
teamstatsopp <- teamstats
names(teamstatsopp) <- c("opp", paste(names(teamstatsopp[,2:23]), "opp", sep = "_"))
compiled <- sqldf("SELECT * FROM allweeks, teamstats, teamstatsopp WHERE allweeks.team = teamstats.opp AND allweeks.opp = teamstatsopp.opp")
compiled <- compiled[,-c(1,10,33)]

# Because most college football games are played at one team's home stadium or another, one team usually enjoys a 
# home field advantage.  There are very few games where this isn't the case.  However, most bowl games are played 
# at neutral sites where neither team has a significant home field advantage.  Since there are simply not enough
# data points for each team to accurately project just how much better they perform at home/away, the purpose 
# of the code below is to normalize each game to adjust for home field advantage.

dup <- compiled
homeadj <- round(rnorm(nrow(dup), 2, 1), 0)
dup <- dup %>% mutate(teamscore = ifelse(home == 1, teamscore - homeadj, teamscore + homeadj),
                      oppscore = ifelse(home == 1, oppscore + homeadj, oppscore - homeadj)) %>%
               mutate(home = 1 - home,
                      win = ifelse(teamscore > oppscore, 1, 0),
                      loss = ifelse(teamscore <= oppscore, 1, 0))
dup$teamscore <- sapply(dup$teamscore, function(x) max(x, 0))
dup$oppscore <- sapply(dup$oppscore, function(x) max(x, 0))
compiled <- rbind(compiled,dup)
rm(dup)

# Selecting necessary features for each major dataset to be used in the prediction algorithms

compiled <- compiled[,c(3:8,1,2,9:52)]
matchup <- compiled[,c(1,3,7:11)]
results <- compiled[,c(1:5)]
stats <- compiled[,c(7:52)]
results <- results %>% mutate(score.diff = teamscore - oppscore)

# Removing fields that are highly correlated

stat_correlation <- cor(stats)
highlycorrelated <- findCorrelation(stat_correlation, cutoff = 0.90)
stats <- stats[,-highlycorrelated]

# Recombining fields into final dataset

newgamedata <- cbind(compiled$team, stats[,grep("_opp$", colnames(stats), invert = TRUE)])
newgamedata <- newgamedata[,-c(2:3)]
names(newgamedata)[1] <- "team"
newgamedata <- unique(newgamedata)

rm(allweeks); rm(oppstatsagg); rm(teamstats);

# Splitting the data into training and testing sets

y_teamscore <- results$teamscore
y_oppscore <- results$oppscore
set.seed(314)
intrain <- createDataPartition(y_teamscore, p = 0.8, list = FALSE)
train_y <- y_teamscore[intrain]
train_x <- stats[intrain,]
test_y <- y_teamscore[-intrain]
test_x <- stats[-intrain,]

# And creating two separate random forest models:  One to model one team's score, another to project the other
# team's score.  Generating predictions for the test set and determining the RMSE.  

intrain <- createDataPartition(y_teamscore, p = 0.8, list = FALSE)
train_y <- y_teamscore[intrain]
train_x <- stats[intrain,]
test_y <- y_teamscore[-intrain]
test_x <- stats[-intrain,]
ts_model <- train(train_x, 
                  train_y, 
                  method = "rf", 
                  preProcess = c("center", "scale"), 
                  ntree = 3500,
                  tuneLength = 10,
                  importance = TRUE,
                  trControl = trainControl(method = "cv"))                               
ts_predict <- predict(ts_model, test_x)
RMSE(ts_predict, test_y)
varImp(ts_model)

intrain <- createDataPartition(y_oppscore, p = 0.8, list = FALSE)
train_y <- y_oppscore[intrain]
train_x <- stats[intrain,]
test_y <- y_oppscore[-intrain]
test_x <- stats[-intrain,]
os_model <- train(train_x, 
                  train_y, 
                  method = "rf", 
                  preProcess = c("center", "scale"), 
                  ntree = 2500,
                  tuneLength = 5,
                  importance = TRUE,
                  trControl = trainControl(method = "cv"))                               
os_predict <- predict(os_model, test_x)
RMSE(os_predict, test_y)
varImp(rf_os)

# Creating a table to display the test set predictions

matchupresults <- cbind(matchup[-intrain,], ts_predict, os_predict)
matchupresults$win <- ifelse(matchupresults$ts_predict > matchupresults$os_predict, 1, 0) 
matchupresults$ts_predict <- round(matchupresults$ts_predict, 0)
matchupresults$os_predict <- round(matchupresults$os_predict, 0)
matchupresults$ts_predict <- sapply(matchupresults$ts_predict , function(x) max(x, 0))
matchupresults$os_predict <- sapply(matchupresults$os_predict , function(x) max(x, 0))

# In football, most scoring is either by way of a touchdown (which typically gives 7 points) or a field goal
# (for 3 points).  There are possibilities for 2, 6, and 8 point scores, but those are not likely to occur in 
# most games.  As such, there are some point totals that are far more likely to occur than others.  While it is
# possible to score 4 points in a single game, the likelihood of that occuring is miniscule as it has only 
# happened a few times in over 100 years.  It is far more probable that a team will score 3 points than 4.  The following 
# code alters the unlikely scores predicted by each model to reflect more common totals.

fg <- c(0:3)
td <- c(0:8)
likelyscores <- expand.grid(t = 3*fg, s = 7*td, stringsAsFactors = FALSE)
likelyscores <- likelyscores %>% mutate(fgs = t/3,
                                        tds = s/7) %>%
                                 mutate(totalscores = fgs + tds) %>%
                                 filter(totalscores < 12) %>%
                                 mutate(finalscores = t + s) %>% 
                                 select(finalscores)
likelyscores <- unique(likelyscores)
score_replace <- function(teamscore) {
        r <- ifelse(teamscore %in% likelyscores, teamscore, likelyscores[which(abs(likelyscores - teamscore) == min(abs(likelyscores - teamscore))), 1])
        return(r)
}

# The models will periodically project a tie between two teams; however, because of overtime, no game will actually
# conclude with a tie.  Noted above, overtime scores tend to skew the scoring statistics in an unpredictable way, 
# so it's difficult to predict that.  However, because the algorithm initially generates an unrounded score 
# (it may generate a score of 20.452313 to 19.892341) that gets rounded to 20 to 20, it does actually pick a winner,
# so the winning score is tweaked to reflect a close game where the winner wins by one point.

matchupresults$ts_predict <- sapply(matchupresults$ts_predict, score_replace)
matchupresults$os_predict <- sapply(matchupresults$os_predict, score_replace)
tie <- which(matchupresults$ts_predict == matchupresults$os_predict)
matchupresults$ts_predict[tie] <- ifelse(matchupresults$win[tie] == 1, matchupresults$ts_predict[tie]+1, matchupresults$ts_predict[tie]-1)


# Below is the code for a boosted tree model.  All of the documentation above can be applied to sections below

y_teamscore <- results$teamscore
y_oppscore <- results$oppscore
set.seed(314)
intrain <- createDataPartition(y_teamscore, p = 0.8, list = FALSE)
train_y <- y_teamscore[intrain]
train_x <- stats[intrain,]
test_y <- y_teamscore[-intrain]
test_x <- stats[-intrain,]
tuneg <- expand.grid(nrounds = 200, 
                    max_depth = 50, 
                    eta = c(0.1, 0.01, 0.001), 
                    gamma = c(3,5), 
                    colsample_bytree = c(0.4, 0.7, 1.0), 
                    min_child_weight = c(0.25, 0.5),
                    subsample = 1)

intrain <- createDataPartition(y_teamscore, p = 0.8, list = FALSE)
train_y <- y_teamscore[intrain]
train_x <- stats[intrain,]
test_y <- y_teamscore[-intrain]
test_x <- stats[-intrain,]
ts_model <- train(train_x, 
                  train_y, 
                  method = "xgbTree", 
                  preProcess = c("center", "scale"), 
                  importance = TRUE,
                  tuneGrid = tuneg,
                  trControl = trainControl(method = "cv"))                            
ts_predict <- predict(ts_model, test_x)
RMSE(ts_predict, test_y)
varImp(ts_model)

intrain <- createDataPartition(y_oppscore, p = 0.8, list = FALSE)
train_y <- y_oppscore[intrain]
train_x <- stats[intrain,]
test_y <- y_oppscore[-intrain]
test_x <- stats[-intrain,]
os_model <- train(train_x, 
                  train_y, 
                  method = "xgbTree", 
                  preProcess = c("center", "scale"), 
                  importance = TRUE,
                  tuneGrid = tuneg,
                  trControl = trainControl(method = "cv"))                              
os_predict <- predict(os_model, test_x)
RMSE(os_predict, test_y)
varImp(os_model)

matchupresults <- cbind(matchup[-intrain,], ts_predict, os_predict)
matchupresults$win <- ifelse(matchupresults$ts_predict > matchupresults$os_predict, 1, 0) 
matchupresults$ts_predict <- round(matchupresults$ts_predict, 0)
matchupresults$os_predict <- round(matchupresults$os_predict, 0)
matchupresults$ts_predict <- sapply(matchupresults$ts_predict , function(x) max(x, 0))
matchupresults$os_predict <- sapply(matchupresults$os_predict , function(x) max(x, 0))

fg <- c(0:3)
td <- c(0:8)
likelyscores <- expand.grid(t = 3*fg, s = 7*td, stringsAsFactors = FALSE)
likelyscores <- likelyscores %>% mutate(fgs = t/3,
                                        tds = s/7) %>%
                                 mutate(totalscores = fgs + tds) %>%
                                 filter(totalscores < 12) %>%
                                 mutate(finalscores = t + s) %>% 
                                 select(finalscores)
likelyscores <- unique(likelyscores)
score_replace <- function(teamscore) {
        r <- ifelse(teamscore %in% likelyscores, teamscore, likelyscores[which(abs(likelyscores - teamscore) == min(abs(likelyscores - teamscore))), 1])
        return(r)
}
matchupresults$ts_predict <- sapply(matchupresults$ts_predict, score_replace)
matchupresults$os_predict <- sapply(matchupresults$os_predict, score_replace)
tie <- which(matchupresults$ts_predict == matchupresults$os_predict)
matchupresults$ts_predict[tie] <- ifelse(matchupresults$win[tie] == 1, matchupresults$ts_predict[tie]+1, matchupresults$ts_predict[tie]-1)

# Another model; this time, it is a feedforward neural network.

y_teamscore <- results$teamscore
y_oppscore <- results$oppscore
set.seed(314)

intrain <- createDataPartition(y_teamscore, p = 0.8, list = FALSE)
train_y <- y_teamscore[intrain]
train_x <- stats[intrain,]
test_y <- y_teamscore[-intrain]
test_x <- stats[-intrain,]
ts_model <- train(train_x, 
                  train_y, 
                  method = "brnn", 
                  preProcess = c("center", "scale"), 
                  trControl = trainControl(method = "cv"))                               
ts_predict <- predict(ts_model, test_x)
RMSE(ts_predict, test_y)


intrain <- createDataPartition(y_oppscore, p = 0.8, list = FALSE)
train_y <- y_oppscore[intrain]
train_x <- stats[intrain,]
test_y <- y_oppscore[-intrain]
test_x <- stats[-intrain,]
os_model <- train(train_x, 
                  train_y, 
                  method = "brnn", 
                  preProcess = c("center", "scale"), 
                  trControl = trainControl(method = "cv"))                               
os_predict <- predict(os_model, test_x)
RMSE(os_predict, test_y)

matchupresults <- cbind(matchup[-intrain,], ts_predict, os_predict)
matchupresults$win <- ifelse(matchupresults$ts_predict > matchupresults$os_predict, 1, 0) 
matchupresults$ts_predict <- round(matchupresults$ts_predict, 0)
matchupresults$os_predict <- round(matchupresults$os_predict, 0)
matchupresults$ts_predict <- sapply(matchupresults$ts_predict , function(x) max(x, 0))
matchupresults$os_predict <- sapply(matchupresults$os_predict , function(x) max(x, 0))

fg <- c(0:3)
td <- c(0:8)
likelyscores <- expand.grid(t = 3*fg, s = 7*td, stringsAsFactors = FALSE)
likelyscores <- likelyscores %>% mutate(fgs = t/3,
                                        tds = s/7) %>%
                                 mutate(totalscores = fgs + tds) %>%
                                 filter(totalscores < 12) %>%
                                 mutate(finalscores = t + s) %>% 
                                 select(finalscores)
likelyscores <- unique(likelyscores)
score_replace <- function(teamscore) {
        r <- ifelse(teamscore %in% likelyscores, teamscore, likelyscores[which(abs(likelyscores - teamscore) == min(abs(likelyscores - teamscore))), 1])
        return(r)
}
matchupresults$ts_predict <- sapply(matchupresults$ts_predict, score_replace)
matchupresults$os_predict <- sapply(matchupresults$os_predict, score_replace)
tie <- which(matchupresults$ts_predict == matchupresults$os_predict)
matchupresults$ts_predict[tie] <- ifelse(matchupresults$win[tie] == 1, matchupresults$ts_predict[tie]+1, matchupresults$ts_predict[tie]-1)

# Generalized linear model

y_teamscore <- results$teamscore
y_oppscore <- results$oppscore
set.seed(314)

intrain <- createDataPartition(y_teamscore, p = 0.8, list = FALSE)
train_y <- y_teamscore[intrain]
train_x <- stats[intrain,]
test_y <- y_teamscore[-intrain]
test_x <- stats[-intrain,]

ts_model <- train(train_x, 
                 train_y, 
                 method = "glm", 
                 preProcess = c("center", "scale"), 
                 trControl = trainControl(method = "cv"))                               
ts_predict <- predict(ts_model, test_x)
RMSE(ts_predict, test_y)

train_y <- y_oppscore[intrain]
train_x <- stats[intrain,]
test_y <- y_oppscore[-intrain]
test_x <- stats[-intrain,]
os_model <- train(train_x, 
                  train_y, 
                  method = "glm", 
                  preProcess = c("center", "scale"), 
                  trControl = trainControl(method = "cv"))                               
os_predict <- predict(os_model, test_x)
RMSE(os_predict, test_y)

matchupresults <- cbind(matchup[-intrain,], ts_predict, os_predict)
matchupresults$ts_predict <- round(matchupresults$ts_predict, 0)
matchupresults$os_predict <- round(matchupresults$os_predict, 0)
matchupresults$ts_predict <- sapply(matchupresults$ts_predict , function(x) max(x, 0))
matchupresults$os_predict <- sapply(matchupresults$os_predict , function(x) max(x, 0))

fg <- c(0:3)
td <- c(0:8)
likelyscores <- expand.grid(t = 3*fg, s = 7*td, stringsAsFactors = FALSE)
likelyscores <- likelyscores %>% mutate(fgs = t/3,
                                        tds = s/7) %>%
                                 mutate(totalscores = fgs + tds) %>%
                                 filter(totalscores < 12) %>%
                                 mutate(finalscores = t + s) %>% 
                                 select(finalscores)
likelyscores <- unique(likelyscores)
score_replace <- function(teamscore) {
      r <- ifelse(teamscore %in% likelyscores, teamscore, likelyscores[which(abs(likelyscores - teamscore) == min(abs(likelyscores - teamscore))), 1])
      return(r)
}
matchupresults$ts_predict <- sapply(matchupresults$ts_predict, score_replace)
matchupresults$os_predict <- sapply(matchupresults$os_predict, score_replace)
tie <- which(matchupresults$ts_predict == matchupresults$os_predict)
matchupresults$ts_predict[tie] <- ifelse(matchupresults$home[tie] == 1, matchupresults$ts_predict[tie]+1, matchupresults$ts_predict[tie]-1)

# The generalized linear model provides a relatively low RMSE and requires significantly less processing time 
# so that seems like a good choice to use for our final model.  

# Time to use the GLM model to predict the bowl games themselves.  Note that the schedule doesn't assume to know the 
# winner of the semi-final playoff games, so all 4 potential final matchups are predicted.

bowlgames <- read.csv("C:\\Users\\Bryan\\Google Drive\\CFB2017\\bowlgames.csv")
upcominggames <- read.csv("C:\\Users\\Bryan\\Google Drive\\CFB2017\\bowlgames.csv")
upcominggames <- sqldf("SELECT * FROM upcominggames, newgamedata WHERE upcominggames.team = newgamedata.team")
upcominggames <- upcominggames[,-5]
upcominggames <- sqldf("SELECT * FROM upcominggames LEFT JOIN newgamedata ON upcominggames.opp = newgamedata.team")
upcominggames <- upcominggames[,-16]
upcominggames <- within(upcominggames, rm(team, opp))
names(upcominggames) <- names(stats)
bowlts_predict <- predict(ts_model, upcominggames)
bowlos_predict <- predict(os_model, upcominggames)
bowl_results <- cbind(bowlgames, bowlts_predict, bowlos_predict)
bowl_results$win <- ifelse(bowl_results$bowlts_predict > bowl_results$bowlos_predict, 1, 0)
bowl_results$bowlts_predict <- round(bowl_results$bowlts_predict, 0)
bowl_results$bowlos_predict <- round(bowl_results$bowlos_predict, 0)
bowl_results$bowlts_predict <- sapply(bowl_results$bowlts_predict , function(x) max(x, 0))
bowl_results$bowlos_predict <- sapply(bowl_results$bowlos_predict , function(x) max(x, 0))
bowl_results$bowlts_predict <- sapply(bowl_results$bowlts_predict, score_replace)
bowl_results$bowlos_predict <- sapply(bowl_results$bowlos_predict, score_replace)
tie <- which(bowl_results$bowlos_predict == bowl_results$bowlts_predict)
bowl_results$bowlts_predict[tie] <- ifelse(bowl_results$win[tie] == 1, bowl_results$bowlts_predict[tie]+1,bowl_results$bowlts_predict[tie]-1)
bowl_prediction <- within(bowl_results, rm(home, n.loc, win))

print(bowl_prediction)

# Now that the bowl games have all been played, we can compare the projected results to the actual results.  In how 
# games did the algorithm correctly project the final score, and how close was it overall?

real_scores <- read.csv("C:\\Users\\Bryan\\Google Drive\\CFB2017\\actual_bowls.txt")
bowl_compare <- sqldf("SELECT * from real_scores, bowl_results WHERE bowl_results.team = real_scores.team AND bowl_results.opp = real_scores.opp")
bowl_compare <- bowl_compare[,-c(5,6)]
bowl_compare <- bowl_compare %>% mutate(winner = ifelse(teamscore > oppscore, team, opp),
                                        proj_winner = ifelse(bowlts_predict > bowlos_predict, team, opp)) %>%
                                 mutate(correct_proj_winner = ifelse(winner == proj_winner, 1, 0))
print(paste("The model correctly projected the winner in", sum(bowl_compare$correct_proj_winner), "games out of", nrow(real_scores), sep = " "))

# This might not seem significantly better than a simple coin flip, but the bowl results from this year included several minor upsets.
# As such, RMSE would likely be a better metric to determine how well the model fits the data rather than accuracy.
# Additionally, most bowl games involve a different psychological mindset from regular season matchups that is 
# difficult to quantify.  Additionally, there are other factors that go into projecting games between two teams
# in which there are few (if any) overlapping results to directly compare against.  
# For comparison, an ESPN college football writer correctly projected the winners of 19 games.
# Link here:  
# http://www.espn.com/college-football/story/_/id/21653211/predicting-41-bowl-games-college-football-playoff-national-championship

error <- RMSE(rbind(bowl_compare$teamscore, bowl_compare$oppscore), rbind(bowl_compare$bowlts_predict, bowl_compare$bowlos_predict))
print(paste("The combined RMSE for this model is ", round(error, 4), sep = " "))