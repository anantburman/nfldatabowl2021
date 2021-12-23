# exploratory data analysis of the dataset
# rudimentary barplot of the different types of special teams plays
dataGames = read.csv("games.csv")
dataPFF = read.csv("PFFScoutingData.csv")
dataPlays = read.csv("plays.csv")
#loading the three used part of the dataset
playType <- as.factor(dataPlays$specialTeamsPlayType)
playType <- unclass(playType)
hist(playType)
# histogram1
dataPlays1 <- dataPlays
dataPlays1$specialTeamsPlayType <- unclass(dataPlays$specialTeamsPlayType)
ggplot(dataPlays1, aes(dataPlays1$specialTeamsPlayType)) + geom_bar()
# barplot of the playtypes
datal <- dataPFF[dataPFF$kickType == "N" | dataPFF$kickType == "R" | dataPFF$kickType == "A", ]
unclass(datal$kickType)
ggplot(datal, aes(kickType)) + geom_bar()
unclass(datal$kickType)
ggplot(datal, aes(kickType)) + geom_bar()
# barplot of different types of punts
ggplot(datal, aes(x=hangTime, y=kickType)) + geom_boxplot()
# boxplot of different types of kicks and hangtime
datal <- dataPFF[dataPFF$kickType != "N" & dataPFF$kickType != "R" & dataPFF$kickType != "A", ]
unclass(datal$kickType)
ggplot(datal, aes(kickType)) + geom_bar()
# barplot of different types of kickoffs

#1
dataGames = read.csv("games.csv")
dataPFF = read.csv("PFFScoutingData.csv")
dataPlays = read.csv("plays.csv")
#data for kickoffs segmented or filtered
# is returning a kcik worth it if travels or looks like its going to reach the endzone
dataKickoffs = dataPlays[dataPlays$specialTeamsPlayType == "Kickoff",]
KReturn <- dataKickoffs[dataKickoffs$playResult!=40,]
KTouchback <- dataKickoffs[dataKickoffs$playResult==40,]
KReturnD <- KReturn[KReturn$kickLength > 65,]

# from the graph of returned kicks that travelled to the end-zone
# the histogram shows that the shape of the plot is left-skewed with significant outliers on the left
hist(KReturnD$playResult)
boxplot(KReturnD$playResult)
mean(KReturnD$playResult, trim=1/5)
hist(KReturnD$playResult)
q3 <- quantile(KReturnD$playResult, 0.75)
q1 <- quantile(KReturnD$playResult, 0.25)
iqr <- IQR(KReturnD$playResult)
KReturnDOutliersR <- KReturnD[(KReturnD$playResult > ((q1) - 1.5*(iqr))) & (KReturnD$playResult < (1.5*(iqr) + (q3))),]
hist(KReturnDOutliersR$playResult)
length(KReturnDOutliersR$playResult)
n = length(KReturnDOutliersR$playResult)
mean1 <- mean(KReturnDOutliersR$playResult)
sd1 <- sd(KReturnDOutliersR$playResult)
testMean <- 40
testStat1 <- (mean1-testMean)/(sd1/sqrt(n))
pt(testStat1, df=n-1, lower.tail = FALSE)
t.test(x=KReturnDOutliersR$playResult, alternative = 'greater', mu=testMean)
t.test(x=KReturnD$playResult, alternative = 'greater', mu=testMean)


#2
# q-2 punts
dataPlays = read.csv("plays.csv")
Punts = dataPlays[dataPlays$specialTeamsPlayType == "Punt",]
puntsReturned <- Punts[, c("playId", "gameId", 'kickLength', 'kickReturnYardage', 'playResult', 'down', 'yardsToGo')]
puntsReturned <- na.omit(puntsReturned)
puntsReturned$combinedId <- puntsReturned$playId + puntsReturned$gameId
# punts returned dataset is configured here
dataGames = read.csv("games.csv")
rest <- merge(puntsReturned, dataGames, by.x="gameId", by.y="gameId")
#merging the two datasets
summary(rest)
boxplot(rest$yardsToGo ~ rest$season)
# boxplot unzoomed
boxplot(rest$yardsToGo ~ rest$season, ylim=c(0,10))
# boxplot zoomed
a1 <- aov(rest$yardsToGo ~ rest$season)
summary(a1)
#running the anova
pairwise.t.test(x=rest$yardsToGo, g=rest$season, p.adj="bonferroni")
# looking inside to see the relations in the anova
qqnorm(a1$residuals)
# drawing the qq norm graph
rest$extar <- as.numeric(rest$yardsToGo < 5)
interaction.plot(response = rest$yardsToGo, x.factor=rest$season, trace.factor=rest$extar)
rs1 <- rest[rest$yardsToGo < 5 & rest$season == '2018', ]
mean(rs1$yardsToGo)
rs1 <- rest[rest$yardsToGo < 5 & rest$season == '2019', ]
mean(rs1$yardsToGo)
rs1 <- rest[rest$yardsToGo < 5 & rest$season == '2020', ]
mean(rs1$yardsToGo)
#means to get an idea

#3
#q3

dataPlayers = read.csv("players.csv")
dataPlays = read.csv("plays.csv")
Punts = dataPlays[dataPlays$specialTeamsPlayType == "Punt",]
puntsReturned <- Punts[, c("playId", "gameId", 'kickLength', 'kickReturnYardage', 'playResult', 'down', 'yardsToGo')]
puntsReturned$combinedId <- puntsReturned$playId + puntsReturned$gameId
puntsReturned <- na.omit(puntsReturned)
#setting up punts returned
dataPFF = read.csv("PFFScoutingData.csv")
dataPFF$combinedId = dataPFF$gameId + dataPFF$playId
dataPFF <- dataPFF[, c("playId", "gameId", "combinedId", "hangTime", "kickType", "operationTime", "kickDirectionIntended", "kickDirectionActual", "snapDetail"),]
dataPFF <- dataPFF[dataPFF$combinedId %in% puntsReturned$combinedId, ]
dataPFF <- dataPFF[dataPFF$gameId %in% puntsReturned$gameId & dataPFF$playId %in% puntsReturned$playId, ]
dataPFF <- na.omit(dataPFF)
#setting up the PFF dataset
dataPFF$kickDirectionChanged <- dataPFF$kickDirectionActual != dataPFF$kickDirectionIntended
res <- merge(puntsReturned, dataPFF, by.x="combinedId", by.y="combinedId")
#merging two datasets
#pairs(res[,c("hangTime", "kickLength", "kickReturnYardage", "playResult")], col=2)
cor(res[,c("hangTime", "kickLength", "kickReturnYardage", "playResult")], use='pairwise.complete.obs')
#correlations
res$kickDirectionChanged <- as.numeric(res$kickDirectionChanged)
res$kickDirectionChanged <- as.numeric(res$kickDirectionChanged)
resstry1 <- res[res$kickDirectionChanged == 0, ]
cor(resstry1[,c("hangTime", "kickLength", "kickReturnYardage", "playResult")], use='pairwise.complete.obs')
arm <- as.factor(resstry1$kickDirectionActual)
unclass(arm)
resstry1$kickDirectionActual <- unclass(arm)
resstry1$kickDirectionActual
boxplot(resstry1$kickLength ~ resstry1$kickDirectionActual)
#getting the boxplot
a2 <- aov(resstry1$kickLength ~ resstry1$kickDirectionActual)
summary(a2)
pairwise.t.test(x=resstry1$kickLength, g=resstry1$kickDirectionActual, p.adj="bonferroni")
# doing the ANOVA and anlyzing how variances vary for different groups
qqnorm(a2$residuals)
#getting the qqplot
rm1 <- resstry1[resstry1$kickDirectionActual == 1,]
length(rm1$kickDirectionActual)
mean(rm1$kickLength)
rm1 <- resstry1[resstry1$kickDirectionActual == 2,]
length(rm1$kickDirectionActual)
mean(rm1$kickLength)
rm1 <- resstry1[resstry1$kickDirectionActual == 3,]
length(rm1$kickDirectionActual)
mean(rm1$kickLength)
#mean to see how it varies in the given dataset