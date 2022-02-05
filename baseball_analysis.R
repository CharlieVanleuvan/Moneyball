#### Final Project ####
#  Baseball Data   #

# MOTIVATION: Baseball has a long history of using analytics to build championship teams (SABRmetrics). Beating other teams
#             in analyzing games will help win games

# PROBLEM: It can be costly to maintain a successful baseball team when analytics are not heing used ("Moneyball" vs. Yankees).

# DATA: data source, data cleaning. Why we chose the data in a certain format

# EDA: look 




# use one drive
setwd("C:/Users/Charlie Vanleuvan/OneDrive - Syracuse University/IST 707 Data Analytics")

#load mlb data
mlb2018gl <- read.csv("mlb_data.csv")

#remove the duplicate column. Not needed
#mlb2018gl <- subset(mlb2018gl,select = -c(LFUmpID))

library(sqldf)
library(factoextra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(flexclust)
library(class)
library(GGally)

# set column names
colnames(mlb2018gl) <- c("Date","GameNumber","DayWeek","Visitor","VisitorLeague","VisitorGameNumber","Home","HomeLeague","HomeGameNumber","VisitorScore","HomeScore","LengthGameOuts","DayNight","CompletionInfor","ForfeitInfo","ProtestInfo","ParkID","Attendance","TimeGameMinutes","VisitorLineScore","HomeLineScore","V_AtBats","V_Hits","V_Doubles","V_Triples","V_HRs","V_RBI","V_SacHits","V_SacFlies","V_HitByPitch","V_Walks","V_IntentionalWalks","V_Strikeouts","V_StolenBases","V_CaughtStealing","V_GroundDoublePlays","V_1stCatchersInterference","V_LeftOnBase","V_PitchersUsed","V_IndEarnedRuns","V_TeamEarnedRuns","V_Wild_Pitches","V_Balks","V_Putouts","V_Assists","V_Errors","V_PassedBalls","V_DoublePlays","V_TriplePlays","H_AtBats","H_Hits","H_Doubles","H_Triples","H_HRs","H_RBI","H_SacHits","H_SacFlies","H_HitByPitch","H_Walks","H_IntentionalWalks","H_Strikeouts","H_StolenBases","H_CaughtStealing","H_GroundDoublePlays","H_1stCatchersInterference","H_LeftOnBase","H_PitchersUsed","H_IndEarnedRuns","H_TeamEarnedRuns","H_Wild_Pitches","H_Balks","H_Putouts","H_Assists","H_Errors","H_PassedBalls","H_DoublePlays","H_TriplePlays","HomePlatUmpID","HomePlateUmpName","FirstBaseUmpID","FirstBaseUmpName","SecondBaseUmpID","SecondBaseUmpName","ThirdBaseUmpID","ThirdBaseUmpName","LFUmpID","LFUmpName","RFUmpID","RFUmpName","V_TeamManagerID","V_TeamManagerName","H_TeamManagerID","H_TeamManagerName","W_PitcherID","W_PitcherName","L_PitcherID","L_PitcherName","S_PitcherID","S_PitcherName","GW_RBIBatterID","GW_RBIBatterName","V_StartingPitcherID","V_StartingPitcherName","H_StartingPitcherID","H_StartingPitcherName","V_Batter1ID","V_Batter1Name","V_Batter1Position","V_Batter2ID","V_Batter2Name","V_Batter2Position","V_Batter3ID","V_Batter3Name","V_Batter3Position","V_Batter4ID","V_Batter4Name","V_Batter4Position","V_Batter5ID","V_Batter5Name","V_Batter5Position","V_Batter6ID","V_Batter6Name","V_Batter6Position","V_Batter7ID","V_Batter7Name","V_Batter7Position","V_Batter8ID","V_Batter8Name","V_Batter8Position","V_Batter9ID","V_Batter9Name","V_Batter9Position","H_Batter1ID","H_Batter1Name","H_Batter1Position","H_Batter2ID","H_Batter2Name","H_Batter2Position","H_Batter3ID","H_Batter3Name","H_Batter3Position","H_Batter4ID","H_Batter4Name","H_Batter4Position","H_Batter5ID","H_Batter5Name","H_Batter5Position","H_Batter6ID","H_Batter6Name","H_Batter6Position","H_Batter7ID","H_Batter7Name","H_Batter7Position","H_Batter8ID","H_Batter8Name","H_Batter8Position","H_Batter9ID","H_Batter9Name","H_Batter9Position","AcquisitionInfo")

# make table for winners and losers of each game
WinnersAndLosers <- sqldf("SELECT Home, Visitor, HomeScore, VisitorScore, CASE WHEN VisitorScore > HomeScore THEN Visitor WHEN HomeScore > VisitorScore THEN Home ELSE 'TIE' END AS Winner, CASE WHEN VisitorScore < HomeScore THEN Visitor WHEN HomeScore < VisitorScore THEN Home ELSE 'TIE' END AS Loser FROM mlb2018gl")

# Table of all teams
Teams <- sqldf("SELECT DISTINCT Home AS Team FROM mlb2018gl")

# Table of team records
TeamRecords <- sqldf("SELECT Team,  (SELECT COUNT(Winner) FROM WinnersAndLosers W WHERE W.Winner = T.Team GROUP BY W.Winner) AS Wins, (SELECT COUNT(Loser) FROM WinnersAndLosers W WHERE W.Loser = T.Team GROUP BY W.Loser) AS Losses FROM Teams T")

# removes the unneeded columns
mlb2018glTEMP <- mlb2018gl[,c(-2,-6,-9,-12,-14,-15,-16,-17,-18,-19,-20,-21,-78,-79,-80,-81,-82,-83,-84,-85,-86,-87,-88,-89,-90,-91,-92,-93,-94,-96,-98,-100,-102,-104,-106,-109,-112,-115,-118,-121,-124,-127,-130,-133,-136,-139,-142,-145,-148,-151,-154,-157,-160)]

#str(mlb2018glTEMP)

# reassign the original DF to the new one without the extra columns
mlb2018gl <- mlb2018glTEMP

# table of all CWS games
ChicagoWhiteSox <- sqldf("SElECT * FROM mlb2018gl WHERE Home = 'CHA' OR Visitor = 'CHA'")

# table of CWS batting stats for 2018
ChicagoWhiteSox2018Batting <- read.csv("CWS2018_BattingStatsForSeason_Clean.csv",header = TRUE, stringsAsFactors = FALSE)

# table of CWS pitching stats for 2018
ChicagoWhiteSox2018Pitching <- read.csv("CWS2018_PitchingStatsForSeason_Clean.csv",header = TRUE, stringsAsFactors = FALSE)

# CWS batting lineups 
CWSBattingLineUps <- sqldf("SELECT Date, Home, Visitor AS Away, CASE WHEN VisitorScore > HomeScore THEN 'WIN' ELSE 'LOSS' END AS Result,  V_Batter1Name AS Batter1, V_Batter2Name AS Batter2, V_Batter3Name AS Batter3, V_Batter4Name AS Batter4, V_Batter5Name AS Batter5, V_Batter6Name AS Batter6, V_Batter7Name AS Batter7, V_Batter8Name AS Batter8, V_Batter9Name AS Batter9 FROM ChicagoWhiteSox WHERE Visitor = 'CHA' UNION SELECT Date, Home, Visitor AS Away, CASE WHEN HomeScore > VisitorScore THEN 'WIN' ELSE 'LOSS' END AS Result,  H_Batter1Name AS Batter1, H_Batter2Name AS Batter2, H_Batter3Name AS Batter3, H_Batter4Name AS Batter4, H_Batter5Name AS Batter5, H_Batter6Name AS Batter6, H_Batter7Name AS Batter7, H_Batter8Name AS Batter8, H_Batter9Name AS Batter9 FROM ChicagoWhiteSox WHERE Home = 'CHA'")

# CWS winning batting lineups
CWSWinningBattingLineups <- sqldf("SELECT Date, Home, Away, Batter1, Batter2, Batter3, Batter4, Batter5, Batter6, Batter7, Batter8, Batter9 FROM CWSBattingLineUps WHERE Result = 'WIN'")

# Counts of CWS winning batting lineup orders
CWSWinningLineupCounts <- sqldf("SELECT DISTINCT Batter1, Batter2, Batter3, Batter4, Batter5, Batter6, Batter7, Batter8, Batter9 FROM CWSWinningBattingLineups")

CWSWinningHomeLineups <- sqldf("SELECT DISTINCT Date, Batter1, Batter2, Batter3, Batter4, Batter5, Batter6, Batter7, Batter8, Batter9 FROM CWSWinningBattingLineups WHERE Home = 'CHA'")
CWSWinningAwayLineups <- sqldf("SELECT DISTINCT Date, Batter1, Batter2, Batter3, Batter4, Batter5, Batter6, Batter7, Batter8, Batter9 FROM CWSWinningBattingLineups WHERE Away = 'CHA'")


# Table of all WAS games
WashingtonNationals <- sqldf("SELECT * FROM mlb2018gl WHERE Home = 'WAS' OR Visitor = 'WAS'")

# Table of all WAS Batting stats for 2018
WashingtonNationals2018Batting <- read.csv("WAS2018_BattingStatsForSeason_Clean.csv",header = TRUE, stringsAsFactors = FALSE)

# Table of all WAS Pitching stats for 2018
WashingtonNationals2018Pitching <- read.csv("WAS2018_PitchingStatsForSeason_Clean.csv",header = TRUE, stringsAsFactors = FALSE)

# Table of all WAS batting lineups
WASBattingLineUps <- sqldf("SELECT Date, Home, Visitor AS Away, CASE WHEN VisitorScore > HomeScore THEN 'WIN' ELSE 'LOSS' END AS Result,  V_Batter1Name AS Batter1, V_Batter2Name AS Batter2, V_Batter3Name AS Batter3, V_Batter4Name AS Batter4, V_Batter5Name AS Batter5, V_Batter6Name AS Batter6, V_Batter7Name AS Batter7, V_Batter8Name AS Batter8, V_Batter9Name AS Batter9 FROM WashingtonNationals WHERE Visitor = 'WAS' UNION SELECT Date, Home, Visitor AS Away, CASE WHEN HomeScore > VisitorScore THEN 'WIN' ELSE 'LOSS' END AS Result,  H_Batter1Name AS Batter1, H_Batter2Name AS Batter2, H_Batter3Name AS Batter3, H_Batter4Name AS Batter4, H_Batter5Name AS Batter5, H_Batter6Name AS Batter6, H_Batter7Name AS Batter7, H_Batter8Name AS Batter8, H_Batter9Name AS Batter9 FROM WashingtonNationals WHERE Home = 'WAS'")

# Table of all Winning WAS batting lineups
WASWinningBattingLineups <- sqldf("SELECT Date, Home, Away, Batter1, Batter2, Batter3, Batter4, Batter5, Batter6, Batter7, Batter8, Batter9 FROM WASBattingLineUps WHERE Result = 'WIN'")

# Table of Winning WAS Home lineups
WASWinningHomeLineups <- sqldf("SELECT DISTINCT Date, Batter1, Batter2, Batter3, Batter4, Batter5, Batter6, Batter7, Batter8, Batter9 FROM WASWinningBattingLineups WHERE Home = 'WAS'")

# Table of winning WAS Away lineups
WASWinningAwayLineups <- sqldf("SELECT DISTINCT Date, Batter1, Batter2, Batter3, Batter4, Batter5, Batter6, Batter7, Batter8, Batter9 FROM WASWinningBattingLineups WHERE Away = 'WAS'")



# CWS player salaries
CWSSalaries18 <- read.csv("MLB Chicago AL 18 OD.csv",header = FALSE,stringsAsFactors = TRUE) 
colnames(CWSSalaries18) <- c("Player2","Position","MLSRV","Agent","Length/TotalValue","sal2018")
CWSSalaries18 <- CWSSalaries18[3:44,1:6]
CWSSalaries18 <- CWSSalaries18[!CWSSalaries18$Position =="",]

# WAS player salaries
WASSalaries18 <- read.csv("MLB Washington 18 OD.csv",header = FALSE,stringsAsFactors = TRUE) 
colnames(WASSalaries18) <- c("Player2","Position","MLSRV","Agent","Length/TotalValue","sal2018")
WASSalaries18 <- WASSalaries18[3:44,1:6]

# set column names to be Player
colnames(ChicagoWhiteSox2018Batting)[3] <- "Player"
colnames(ChicagoWhiteSox2018Pitching)[3] <- "Player"
colnames(WashingtonNationals2018Batting)[3] <- "Player"
colnames(WashingtonNationals2018Pitching)[3] <- "Player"


# table of CWS players
CWSPlayer <- data.frame(do.call('rbind', strsplit(as.character(CWSSalaries18$Player),', ',fixed=TRUE)))
CWSPlayer <- CWSPlayer %>% unite("Player", X2:X1, sep = " ",remove = FALSE)  
CWSPlayer <- CWSPlayer %>% unite("Player2",X1:X2, sep = ", ",remove = FALSE)

# table of CWS player Salaries
CWSSalaries18 <- merge.data.frame(CWSPlayer,CWSSalaries18,by = "Player2")
CWSSalaries18$sal2018 <- gsub("\\$","",CWSSalaries18$sal2018)
CWSSalaries18$sal2018 <- as.numeric(gsub(",","",CWSSalaries18$sal2018))
CWSSalaries18 <- na.omit(CWSSalaries18)

# table of WAS players
WASPlayer <- data.frame(do.call('rbind', strsplit(as.character(WASSalaries18$Player),', ',fixed=TRUE)))
WASPlayer <- WASPlayer %>% unite("Player", X2:X1, sep = " ",remove = FALSE)  
WASPlayer <- WASPlayer %>% unite("Player2",X1:X2, sep = ", ",remove = FALSE)

# tableof WAS player salaries
WASSalaries18 <- merge.data.frame(WASPlayer,WASSalaries18,by = "Player2")
WASSalaries18$sal2018 <- gsub("\\$","",WASSalaries18$sal2018)
WASSalaries18$sal2018 <- as.numeric(gsub(",","",WASSalaries18$sal2018))
WASSalaries18 <- na.omit(WASSalaries18)


############
Begin EDA
###########

#merge CWS player stats and salaries
CWSsalbatstats18 <- merge(CWSSalaries18,ChicagoWhiteSox2018Batting,by = "Player")
CWSsalbatstats18 <- CWSsalbatstats18 %>%
  mutate_at(vars(sal2018), funs(./1000000)) 

#correlation matrix for all hitting stats
ggpairs(CWSsalbatstats18[,c(9,12,16,17,21,27,29)])

#white sox salary vs Age
ggplot(CWSsalbatstats18,aes(x=Age,y=sal2018, label=Player)) + geom_text_repel() +geom_point(aes(size=G)) + ylab("Salaries 2018, in Millions") + xlab("Age") + ggtitle("White Sox Player Age by Salary") + labs(size = "Games Played")

#white sox Salary vs OBP
ggplot(CWSsalbatstats18,aes(x=sal2018,y=OBP, label=Player)) + geom_text_repel() +geom_point(aes(size=G)) + ylab("On-Base Percentage") + xlab("Salary, in Millions") + ggtitle("White Sox Player OBP vs. Salary") + labs(size = "Games Played")

#white sox OBP vs Runs
ggplot(CWSsalbatstats18,aes(x=OBP,y=R, label=Player)) + geom_text_repel() +geom_point(aes(size=sal2018)) + ylab("Runs") + xlab("On-Base Percentage") + ggtitle("White Sox Player OBP vs. Runs") + labs(size = "2018 Salary")

#white sox Runs vs RBI
ggplot(CWSsalbatstats18,aes(x=RBI,y=R, label=Player)) + geom_text_repel() +geom_point(aes(size=sal2018)) + ylab("R") + xlab("RBI") + ggtitle("White Sox Player Runs vs. RBI") + labs(size = "2018 Salary")


#white sox OBP vs salary
ggplot(CWSsalbatstats18,aes(x=OBP,y=sal2018,color=RBI, label=Player)) + geom_text_repel() +geom_point(aes(size=G)) + ylab("Salaries 2018, in Millions") + xlab("On Base Percentage") + ggtitle("White Sox On Base Percentage by Salary") + labs(size = "Games Played")

#merge CWS pitcher stats and salaries
CWSsalpitchstats18 <- merge(CWSSalaries18,ChicagoWhiteSox2018Pitching,by = "Player")
CWSsalpitchstats18 <- CWSsalpitchstats18 %>% mutate_at(vars(sal2018),funs(./1000000))
#white sox ERA vs salary
ggplot(CWSsalpitchstats18,aes(x=ERA,y=sal2018,color=W.L., label=Player)) + geom_text_repel()+ geom_point(aes(size=SV)) + ylab("Salaries 2018, in Millions") + xlab("Earned Run Average") + ggtitle("White Sox Earned Run Average by Salary") + labs(size="Saved Games", color= "Win/Loss Percentage")


##### WAS Player EDA #######
#merge WAS player stats and salaries
WASsalbatstats18 <- merge(WASSalaries18,WashingtonNationals2018Batting,by = "Player")
WASsalbatstats18 <- WASsalbatstats18 %>%
  mutate_at(vars(sal2018), funs(./1000000)) 
ggplot(WASsalbatstats18,aes(x=Age,y=sal2018, label = Player))+ geom_text_repel() + geom_point(aes(size=G)) + ylab("Salaries 2018, in Millions") + xlab("Age") + ggtitle("Nationals Age by Salary") + labs(size = "Games Played")

WASsalpitchstats18 <- merge(WASSalaries18,WashingtonNationals2018Pitching,by = "Player")
WASsalpitchstats18 <- WASsalpitchstats18 %>% mutate_at(vars(sal2018),funs(./1000000))
ggplot(WASsalpitchstats18,aes(x=ERA,y=sal2018,label = Player))+ geom_text_repel() + geom_point(aes(size=SV)) + ylab("Salaries 2018, in Millions") + xlab("Earned Run Average") + ggtitle("Nationals Earned Run Average by Salary") + labs(size="Saved Games", color= "Win/Loss Average")


#correlation matrix for all hitting stats
ggpairs(WASsalbatstats18[,c(9,12,16,17,21,27,29)])

#WAS Salary vs OBP
ggplot(WASsalbatstats18,aes(x=sal2018,y=OBP)) + geom_point(aes(size=G)) + ylab("On-Base Percentage") + xlab("2018 Salary, in Millions") + ggtitle("Nationals On Base Percentage by Salary") + labs(size = "Games Played")

#WAS Salary vs RBI
ggplot(WASsalbatstats18,aes(x=sal2018,y=RBI)) + geom_point(aes(size=G)) + ylab("RBI") + xlab("2018 Salary, in Millions") + ggtitle("Nationals RBIs by Salary") + labs(size = "Games Played")

#WAS Salary vs Runs
ggplot(WASsalbatstats18,aes(x=RBI,y=R)) + geom_point(aes(size=G)) + ylab("Runs") + xlab("RBIs") + ggtitle("Nationals RBIs vs. Runs Scored ") + labs(size = "Games Played")


#CWS game winning RBI plot
CWSWins <- sqldf("select * from ChicagoWhiteSox where ((Visitor = 'CHA' AND VisitorScore > HomeScore) OR (Home = 'CHA' AND HomeScore > VisitorScore))")
CWSGameWinningRBIs <-  sqldf("select GW_RBIBatterName, count(GW_RBIBatterName) from CWSWins group by GW_RBIBatterName")
colnames(CWSGameWinningRBIs) <- c("Player","GWRBIs")
CWSGameWinningRBIs <- CWSGameWinningRBIs[order(-CWSGameWinningRBIs$GWRBIs),]
ggplot(CWSGameWinningRBIs[1:12,], aes(x=reorder(Player,-GWRBIs), y=GWRBIs))+ geom_text(aes(label=GWRBIs), vjust = -0.5)+ ggtitle("Chicago White Sox Game Winning RBI Leaders for 2018") +xlab("Player") + ylab("# of GWRBIs") +geom_col(color = '#27251F', fill='#C4CED4')+theme(axis.text.x = element_text(angle = 45,hjust=1))


#CWS pitcher run support plot
CWSHomeGames <- sqldf("select HomeScore,H_StartingPitcherName from ChicagoWhiteSox where (Home = 'CHA')")
colnames(CWSHomeGames) <- c("Score","Pitcher")
CWSAwayGames <- sqldf("select VisitorScore, V_StartingPitcherName from ChicagoWhiteSox where (Visitor = 'CHA')")
colnames(CWSAwayGames) <- c("Score","Pitcher")
CWSPitcherRunSupport <- rbind.data.frame(CWSHomeGames,CWSAwayGames)
CWSPitcherRunSupport <- aggregate(CWSPitcherRunSupport$Score, list(CWSPitcherRunSupport$Pitcher), mean)
colnames(CWSPitcherRunSupport) <- c("Pitcher","AverageRunSupport")
CWSPitcherRunSupport$AverageRunSupport <- round(CWSPitcherRunSupport$AverageRunSupport,2)
ggplot(CWSPitcherRunSupport, aes(x=reorder(Pitcher,-AverageRunSupport),y=AverageRunSupport))+geom_text(aes(label=AverageRunSupport),vjust=-0.5) + ggtitle("Average Run Support for Chicago White Sox Starting Rotation") + xlab("Player")+ylab("Run Support per Start") + geom_col(color='#27251F',fill='#C4CED4')+ theme(axis.text.x = element_text(angle = 45,hjust=1))


#WAS Game Winning RBI plot
WASWins <- sqldf("select * from WashingtonNationals where ((Visitor = 'WAS' AND VisitorScore > HomeScore) OR (Home = 'WAS' AND HomeScore > VisitorScore))")
WASGameWinningRBIs <-  sqldf("select GW_RBIBatterName, count(GW_RBIBatterName) from WASWins group by GW_RBIBatterName")
colnames(WASGameWinningRBIs) <- c("Player","GWRBIs")
WASGameWinningRBIs <- WASGameWinningRBIs[order(-WASGameWinningRBIs$GWRBIs),]
ggplot(WASGameWinningRBIs, aes(x=reorder(Player,-GWRBIs), y=GWRBIs))+ geom_text(aes(label=GWRBIs), vjust = -0.5)+ ggtitle("Washington Nationals Game Winning RBI Leaders for 2018") +xlab("Player") + ylab("# of GWRBIs") +geom_col(color = '#AB0003', fill='#14225A')+theme(axis.text.x = element_text(angle = 45,hjust=1))


#WAS pitcher run support plot
WASHomeGames <- sqldf("select HomeScore,H_StartingPitcherName from WashingtonNationals where (Home = 'WAS')")
colnames(WASHomeGames) <- c("Score","Pitcher")
WASAwayGames <- sqldf("select VisitorScore, V_StartingPitcherName from WashingtonNationals where (Visitor = 'WAS')")
colnames(WASAwayGames) <- c("Score","Pitcher")
WASPitcherRunSupport <- rbind.data.frame(WASHomeGames,WASAwayGames)
WASPitcherRunSupport <- aggregate(WASPitcherRunSupport$Score, list(WASPitcherRunSupport$Pitcher), mean)
colnames(WASPitcherRunSupport) <- c("Pitcher","AverageRunSupport")
WASPitcherRunSupport$AverageRunSupport <- round(WASPitcherRunSupport$AverageRunSupport,2)
ggplot(WASPitcherRunSupport, aes(x=reorder(Pitcher,-AverageRunSupport),y=AverageRunSupport))+geom_text(aes(label=AverageRunSupport),vjust=-0.5) + ggtitle("Average Run Support for Washington Nationals Starting Rotation") + xlab("Player")+ylab("Run Support per Start") + geom_col(color='#AB0003',fill='#14225A')+ theme(axis.text.x = element_text(angle = 45,hjust=1))





###################
CLUSTERING ANALYSES
###################

#clustering players
WashingtonNationals2018Batting # WAS batting stats for 2018
ChicagoWhiteSox2018Batting # CWS batting stats for 2018
set.seed(20)

#create batting data frame for clustering
WASclusteringDF <- filter(WASsalbatstats18,!(Position %in% c('rhp-s','lhp-s','rhp','lhp')))

k_vs_squares <- data.frame(K = c(1:10),Sum_Of_Squares = 0)
k_vs_squares$K <- as.integer(k_vs_squares$K)

#Loop through k's and plot sum of squares vs k
for (k in c(1:10)) {
  clusters <- kmeans(WASclusteringDF[,-c(1:8,10:12,37)],k, iter.max = 100)
  
  k_vs_squares$Sum_Of_Squares[k_vs_squares$K == k] <- clusters$tot.withinss
}

#plot k vs within cluster sum of squares
ggplot(k_vs_squares,aes(x = K, y = Sum_Of_Squares))+ geom_line() +scale_x_continuous(breaks=seq(0,11,1))  + xlab("k Value") + ylab("Within Cluster Sum of Squares") + ggtitle("Resulting Within Cluster Squared Errors for 10 k Values")

# Kmeans with optimal K = 3
three_clusters <- kmeans(WASclusteringDF[,-c(1:8,10:12,37)],3, iter.max = 100)
WAS_Batting_3_Clusters <- data.frame(WASclusteringDF,three_clusters$cluster)
ggplot(WAS_Batting_3_Clusters, aes(x=R,y=RBI))+geom_point(aes(size=Age,color=factor(three_clusters.cluster))) + geom_text_repel(aes(label=paste(Player,",",Pos))) + scale_color_manual(name = "Cluster",values=c("red","navy blue","lightskyblue","snow3")) + xlab("Runs") + ylab("RBIs") +ggtitle("Washington Nationals Player Runs vs. RBI, Clustered by K-Means") + labs(size="Age")
three_clusters$tot.withinss
three_clusters$centers


# K= 4
four_clusters <- kmeans(WASclusteringDF[,-c(1:8,10:12,37)],4, iter.max = 100)
WAS_Batting_4_Clusters <- data.frame(WASclusteringDF,four_clusters$cluster)
ggplot(WAS_Batting_4_Clusters, aes(x=R,y=RBI))+geom_point(aes(size=Age,color=factor(four_clusters.cluster))) + geom_text_repel(aes(label=paste(Player,",",Pos))) + scale_color_manual(name = "Cluster",values=c("red","navy blue","lightskyblue","snow3")) + xlab("R") + ylab("RBIs") +ggtitle("Washington Nationals Player Runs vs. RBI, Clustered by K-Means") + labs(size="Age")
four_clusters$tot.withinss
four_clusters$centers

# K=5 --> too much separation, overfitting 
five_clusters <- kmeans(WASclusteringDF[,-c(1:8,10:12,37)],5, iter.max = 100 )
WAS_Batting_5_Clusters <- data.frame(WASclusteringDF,five_clusters$cluster)
ggplot(WAS_Batting_5_Clusters, aes(x=R,y=RBI))+geom_point(aes(size=Age,color=factor(five_clusters.cluster))) + geom_text_repel(aes(label=paste(Player,",",Pos))) + scale_color_manual(name = "Cluster",values=c("red","navy blue", "lightskyblue","snow3","black")) + ylab("RBI")+xlab("Runs")+ ggtitle("Washington Nationals Runs vs. RBI, Clustered by K-Means") + labs(size="Age")
five_clusters$tot.withinss


#summarize the clusters for k = 3
cluster_summary <- WAS_Batting_3_Clusters[,c("sal2018",
                          "Age",
                          "BA",
                          "R",
                          "H",
                          "HR",
                          "RBI",
                          "OPS",
                          "three_clusters.cluster")] %>% group_by(three_clusters.cluster) %>% summarize_all(funs(mean),na.rm = TRUE)
cluster_summary_2 <- gather(cluster_summary,-three_clusters.cluster, key = HittingCategory, value = Value)

WAS_overall_means <- WASclusteringDF[,c("sal2018",
                                        "Age",
                                        "BA",
                                        "R",
                                        "H",
                                        "HR",
                                        "RBI",
                                        "OPS")] %>% summarize_all(funs(mean))
WAS_overall_means <- gather(WAS_overall_means,key = HittingCategory,value=Overall_Mean)

cluster_comparison <- left_join(cluster_summary_2,WAS_overall_means,by="HittingCategory")

cluster_comparison <- cluster_comparison %>% group_by(HittingCategory) %>% mutate(varmeans=(Value/Overall_Mean))


ggplot(cluster_comparison,aes(y=Value,x=HittingCategory))+facet_wrap(~three_clusters.cluster) + geom_bar(aes(fill=Value),stat="identity")+scale_fill_gradient(low="#14225A",high = "#AB0003")+geom_point(aes(y=Overall_Mean,x=HittingCategory))+coord_flip()+ggtitle("Cluster Comparison for the Washington Nationals")+ xlab("Offensive Stats") + theme(legend.position = "none")
WAS_Batting_3_Clusters[order(-WAS_Batting_3_Clusters$sal2018),]




#Clustering for CWS
#create batting data frame for clustering
CWSclusteringDF <- filter(CWSsalbatstats18,!(Position %in% c('rhp-s','lhp-s','rhp','lhp')))

stepFlexclust(CWSclusteringDF[,-c(1:8,10:12,37)],k=2:11,nrep=20)

k_vs_squares <- data.frame(K = c(1:10),Sum_Of_Squares = 0)
k_vs_squares$K <- as.integer(k_vs_squares$K)

#Loop through k's and plot sum of squares vs k
for (k in c(1:10)) {
  clusters <- kmeans(CWSclusteringDF[,-c(1:8,10:12,37)],k, iter.max = 100)
  
  k_vs_squares$Sum_Of_Squares[k_vs_squares$K == k] <- clusters$tot.withinss
}

#plot k vs within cluster sum of squares
ggplot(k_vs_squares,aes(x = K, y = Sum_Of_Squares))+ geom_line() +scale_x_continuous(breaks=seq(0,11,1))  + xlab("k Value") + ylab("Within Cluster Sum of Squares") + ggtitle("Resulting Within Cluster Squared Errors for 10 k Values")


# K= 2
cws_two_clusters <- kmeans(CWSclusteringDF[,-c(1:8,10:12,37)],2, iter.max = 100)
CWS_Batting_2_Clusters <- data.frame(CWSclusteringDF,cws_two_clusters$cluster)
ggplot(CWS_Batting_2_Clusters, aes(x=R,y=RBI))+geom_point(aes(size=Age,color=factor(cws_two_clusters.cluster))) + geom_text_repel(aes(label=paste(Player,",",Pos))) + scale_color_manual(name = "Cluster",values=c("black","gray53","navy blue","red")) + xlab("R") + ylab("RBI") +ggtitle("RBI vs. Chicago White Sox Player Runs, Clustered by K-Means") + labs(size="Age")





# K= 4
cws_four_clusters <- kmeans(CWSclusteringDF[,-c(1:8,10:12,37)],4, iter.max = 100)
CWS_Batting_4_Clusters <- data.frame(CWSclusteringDF,cws_four_clusters$cluster)
ggplot(CWS_Batting_4_Clusters, aes(x=R,y=RBI))+geom_point(aes(size=Age,color=factor(cws_four_clusters.cluster))) + geom_text_repel(aes(label=paste(Player,",",Pos))) + scale_color_manual(name = "Cluster",values=c("black","gray53","navy blue","red")) + xlab("Runs") + ylab("RBI") +ggtitle("Runs vs. Chicago White Sox Player RBIs, Clustered by K-Means") + labs(size="Age")
ggplot(CWS_Batting_4_Clusters, aes(x=sal2018,y=H))+geom_point(aes(size=Age,color=factor(cws_four_clusters.cluster))) + geom_text_repel(aes(label=paste(Player,",",Pos))) + scale_color_manual(name = "Cluster",values=c("black","gray53","navy blue","red")) + xlab("2018 Player Salary, in $Mil") + ylab("Hits (H)") +ggtitle("Hits vs. Chicago White Sox Player Salary, Clustered by K-Means") + labs(size="Age")


# K= 6
cws_six_clusters <- kmeans(CWSclusteringDF[,-c(1:8,10:12,37)],6, iter.max = 100)
CWS_Batting_6_Clusters <- data.frame(CWSclusteringDF,cws_six_clusters$cluster)
ggplot(CWS_Batting_6_Clusters, aes(x=R,y=RBI))+geom_point(aes(size=Age,color=factor(cws_six_clusters.cluster))) + geom_text_repel(aes(label=paste(Player,",",Pos))) + scale_color_manual(name = "Cluster",values=c("black","gray53","navy blue","red","green","orange")) + xlab("Runs") + ylab("RBI") +ggtitle("RBI vs. Chicago White Sox Player Runs, Clustered by k-Means") + labs(size="Age")


#summarize the clusters
CWS_cluster_summary <- CWS_Batting_4_Clusters[,c("sal2018",
                                             "Age",
                                             "BA",
                                             "R",
                                             "H",
                                             "HR",
                                             "RBI",
                                             "OPS",
                                             "cws_four_clusters.cluster")] %>% group_by(cws_four_clusters.cluster) %>% summarize_all(funs(mean),na.rm = TRUE)
CWS_cluster_summary_2 <- gather(CWS_cluster_summary,-cws_four_clusters.cluster, key = HittingCategory, value = Value)

CWS_overall_means <- CWSclusteringDF[,c("sal2018",
                                        "Age",
                                        "BA",
                                        "R",
                                        "H",
                                        "HR",
                                        "RBI",
                                        "OPS")] %>% summarize_all(funs(mean))
CWS_overall_means <- gather(CWS_overall_means,key = HittingCategory,value=Overall_Mean)

CWS_cluster_comparison <- left_join(CWS_cluster_summary_2,CWS_overall_means,by="HittingCategory")

CWS_cluster_comparison <- CWS_cluster_comparison %>% group_by(HittingCategory) %>% mutate(varmeans=(Value/Overall_Mean))

ggplot(CWS_cluster_comparison,aes(y=Value,x=HittingCategory))+facet_wrap(~cws_four_clusters.cluster) + geom_bar(aes(fill=Value),stat="identity")+scale_fill_gradient(low="#27251F",high = "#C4CED4")+geom_point(aes(y=Overall_Mean,x=HittingCategory))+coord_flip()+ggtitle("Cluster Comparison for the Chicago White Sox")+ xlab("Offensive Stats") + theme(legend.position = "none")

CWS_Batting_4_Clusters[order(-CWS_Batting_4_Clusters$sal2018),]



############
bucket player salaries CWS
################

CWSsalbatstats18$salCategory <- ""
CWSsalpitchstats18$salCategory <- ""
CWSsalbatstats18$salCategory[CWSsalbatstats18$sal2018 <= 1.999] <- "low"
CWSsalbatstats18$salCategory[CWSsalbatstats18$sal2018 >= 7.000] <- "high"
CWSsalbatstats18$salCategory[CWSsalbatstats18$salCategory == ""] <- "mid"
CWSsalpitchstats18$salCategory[CWSsalpitchstats18$sal2018 <= 1.999] <- "low"
CWSsalpitchstats18$salCategory[CWSsalpitchstats18$sal2018 >= 7.000] <- "high"
CWSsalpitchstats18$salCategory[CWSsalpitchstats18$salCategory == ""] <- "mid"
CWSsalbatstats18reduced <- CWSsalbatstats18[,c(38,2,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,31,32,36)]
CWSsalpitchstats18reduced <- CWSsalpitchstats18[,c(44,2,10,5,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,36)]
CWSsalbatstats18reduced$salCategory <- as.factor(CWSsalbatstats18reduced$salCategory)
CWSsalbatstats18reduced$Pos <- as.factor(CWSsalbatstats18reduced$Pos)
CWSsalpitchstats18reduced$salCategory <- as.factor(CWSsalpitchstats18reduced$salCategory)
CWSsalpitchstats18reduced$Pos <- as.factor(CWSsalpitchstats18reduced$Pos)
CWSsalpitchstats18reduced$W.L.[is.na(CWSsalpitchstats18reduced$W.L.)] <- 0

##################
bucket player salaries WAS
####################

WASsalbatstats18$salCategory <- ""
WASsalpitchstats18$salCategory <- ""
WASsalbatstats18$salCategory[WASsalbatstats18$sal2018 <= 3.000] <- "low"
WASsalbatstats18$salCategory[WASsalbatstats18$sal2018 >= 11.000] <- "high"
WASsalbatstats18$salCategory[WASsalbatstats18$salCategory == ""] <- "mid"
WASsalpitchstats18$salCategory[WASsalpitchstats18$sal2018 <= 1.000] <- "low"
WASsalpitchstats18$salCategory[WASsalpitchstats18$sal2018 >= 7.000] <- "high"
WASsalpitchstats18$salCategory[WASsalpitchstats18$salCategory == ""] <- "mid"
WASsalbatstats18reduced <- WASsalbatstats18[,c(38,2,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,31,32,36)]
WASsalpitchstats18reduced <- WASsalpitchstats18[,c(44,2,10,5,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,36)]
WASsalbatstats18reduced$salCategory <- as.factor(WASsalbatstats18reduced$salCategory)
WASsalbatstats18reduced$Pos <- as.factor(WASsalbatstats18reduced$Pos)
WASsalpitchstats18reduced$salCategory <- as.factor(WASsalpitchstats18reduced$salCategory)
WASsalpitchstats18reduced$Pos <- as.factor(WASsalpitchstats18reduced$Pos)
WASsalpitchstats18reduced$W.L.[is.na(WASsalpitchstats18reduced$W.L.)] <- 0


###############
knn - Washington
###############

#Batters
# Use WASsalbatstats18reduced

#Number of samples = 19
nrow(WASsalbatstats18reduced)


#convert all columns to numeric
WASsalbatstats18reduced$Rk <- as.numeric(WASsalbatstats18reduced$Rk)
WASsalbatstats18reduced$G <- as.numeric(WASsalbatstats18reduced$G)
WASsalbatstats18reduced$PA<- as.numeric(WASsalbatstats18reduced$PA)
WASsalbatstats18reduced$AB <- as.numeric(WASsalbatstats18reduced$AB)
WASsalbatstats18reduced$R <- as.numeric(WASsalbatstats18reduced$R)
WASsalbatstats18reduced$H <- as.numeric(WASsalbatstats18reduced$H)
WASsalbatstats18reduced$X2B <- as.numeric(WASsalbatstats18reduced$X2B)
WASsalbatstats18reduced$X3B <- as.numeric(WASsalbatstats18reduced$X3B)
WASsalbatstats18reduced$HR <- as.numeric(WASsalbatstats18reduced$HR)
WASsalbatstats18reduced$RBI <- as.numeric(WASsalbatstats18reduced$RBI)
WASsalbatstats18reduced$SB <- as.numeric(WASsalbatstats18reduced$SB)
WASsalbatstats18reduced$CS <- as.numeric(WASsalbatstats18reduced$CS)
WASsalbatstats18reduced$BB <- as.numeric(WASsalbatstats18reduced$BB)
WASsalbatstats18reduced$SO <- as.numeric(WASsalbatstats18reduced$SO)
WASsalbatstats18reduced$TB <- as.numeric(WASsalbatstats18reduced$TB)
WASsalbatstats18reduced$GDP <- as.numeric(WASsalbatstats18reduced$GDP)
WASsalbatstats18reduced$IBB <- as.numeric(WASsalbatstats18reduced$IBB)


#remove non stat columns

WASsalbatstats18reduced <- WASsalbatstats18reduced[,c(1,5:24)]

#Test accuracy for single fold validation. Using a single train/test split to find optimal k Value
# if no clear best choice, then use sqrt(nrow(dataset))

k_folds <- 3
holdout <- split(sample(1:nrow(WASsalbatstats18reduced)),1:k_folds)

accuracy_values <- c()

#loop through kfolds in 3 fold cross validation. Manually change k in knn function from 1 to 5
for (k in 1:k_folds) {
  test <- sample_n(WASsalbatstats18reduced[holdout[[k]],],1000, replace = TRUE)
  test_labels <- test[,1, drop = TRUE]
  #test <- sample_n(WASsalbatstats18reduced[holdout[[k]],-c(1)],4)
  test <- test[,-c(1)]
  test <- na.omit(test)
  
  train <- sample_n(WASsalbatstats18reduced[-holdout[[k]],],5000, replace = TRUE)
  train <- na.omit(train)
  classification_labels <- train$salCategory
  train <- train[,-c(1)]
  
  kNNmodel <- knn(train = train, test = test, cl = classification_labels, k = 5)
  
  #output <- data.frame(Predictions = kNNmodel, Actual = test_labels)
  #print(output)
  #create predictions
  accuracy_table <- table(Predicted = kNNmodel, Actual = test_labels)
  
  #generate accuracy score
  accuracy <- sum(diag(accuracy_table)) / sum(accuracy_table)
  
  #append to accuracy_values
  accuracy_values <- append(accuracy_values,accuracy)
  
  #print out accuracy score
  print(paste("K-Fold Iteration: ", k, "    Accuracy: ", accuracy))
  
}

#average accuracy values for above iteration
print(paste("Average Accuracy: ", mean(accuracy_values)))

#plot acuracies for each  kfold cross validation
ggplot(as.data.frame(accuracy_values), aes(x = row_number(as.data.frame(accuracy_values)), y = accuracy_values)) + geom_line() + geom_point() + xlab("k-fold Cross Validation Iteration") + ylab("Accuracy Score") + ggtitle("Cross Validation Accuracy Scores -- Nationals Batters")





















#Pitchers
# Use WASsalpitchstats18reduced

#Number of samples = 19
nrow(WASsalpitchstats18reduced)


#convert all columns to numeric


#remove non stat columns

WASsalpitchstats18reduced <- WASsalpitchstats18reduced[,c(1,5:23)]

#Test accuracy for single fold validation. Using a single train/test split to find optimal k Value
# if no clear best choice, then use sqrt(nrow(dataset))

k_folds <- 3
holdout <- split(sample(1:nrow(WASsalpitchstats18reduced)),1:k_folds)

accuracy_values <- c()

#loop through kfolds in 3 fold cross validation. Manually change k in knn function from 1 to 5
for (k in 1:k_folds) {
  test <- sample_n(WASsalpitchstats18reduced[holdout[[k]],],1000, replace = TRUE)
  test_labels <- test[,1, drop = TRUE]
  #test <- sample_n(WASsalbatstats18reduced[holdout[[k]],-c(1)],4)
  test <- test[,-c(1)]
  test <- na.omit(test)
  
  train <- sample_n(WASsalpitchstats18reduced[-holdout[[k]],],5000, replace = TRUE)
  train <- na.omit(train)
  classification_labels <- train$salCategory
  train <- train[,-c(1)]
  
  kNNmodel <- knn(train = train, test = test, cl = classification_labels, k = 3)
  
  #output <- data.frame(Predictions = kNNmodel, Actual = test_labels)
  #print(output)
  #create predictions
  accuracy_table <- table(Predicted = kNNmodel, Actual = test_labels)
  
  #generate accuracy score
  accuracy <- sum(diag(accuracy_table)) / sum(accuracy_table)
  
  #append to accuracy_values
  accuracy_values <- append(accuracy_values,accuracy)
  
  #print out accuracy score
  print(paste("K-Fold Iteration: ", k, "    Accuracy: ", accuracy))
  
}

#average accuracy values for above iteration
print(paste("Average Accuracy: ", mean(accuracy_values)))

#plot acuracies for each  kfold cross validation
ggplot(as.data.frame(accuracy_values), aes(x = row_number(as.data.frame(accuracy_values)), y = accuracy_values)) + geom_line() + geom_point() + xlab("k-fold Cross Validation Iteration") + ylab("Accuracy Score") + ggtitle("Cross Validation Accuracy Scores -- Nationals Pitchers")
















###############
knn - White Sox
##############
#Batters
# Use CWSsalbatstats18reduced


#Number of samples = 19
nrow(CWSsalbatstats18reduced)


#convert all columns to numeric


#remove non stat columns

CWSsalbatstats18reduced <- CWSsalbatstats18reduced[,c(1,5:24)]

#Test accuracy for single fold validation. Using a single train/test split to find optimal k Value
# if no clear best choice, then use sqrt(nrow(dataset))

k_folds <- 3
holdout <- split(sample(1:nrow(CWSsalbatstats18reduced)),1:k_folds)

accuracy_values <- c()

#loop through kfolds in 3 fold cross validation. Manually change k in knn function from 1 to 5
for (k in 1:k_folds) {
  test <- sample_n(CWSsalbatstats18reduced[holdout[[k]],],1000, replace = TRUE)
  test_labels <- test[,1, drop = TRUE]
  #test <- sample_n(CWSsalbatstats18reduced[holdout[[k]],-c(1)],4)
  test <- test[,-c(1)]
  test <- na.omit(test)
  
  train <- sample_n(CWSsalbatstats18reduced[-holdout[[k]],],5000, replace = TRUE)
  train <- na.omit(train)
  classification_labels <- train$salCategory
  train <- train[,-c(1)]
  
  kNNmodel <- knn(train = train, test = test, cl = classification_labels, k = 5)
  
  #output <- data.frame(Predictions = kNNmodel, Actual = test_labels)
  #print(output)
  #create predictions
  accuracy_table <- table(Predicted = kNNmodel, Actual = test_labels)
  
  #generate accuracy score
  accuracy <- sum(diag(accuracy_table)) / sum(accuracy_table)
  
  #append to accuracy_values
  accuracy_values <- append(accuracy_values,accuracy)
  
  #print out accuracy score
  print(paste("K-Fold Iteration: ", k, "    Accuracy: ", accuracy))
  
}

#average accuracy values for above iteration
print(paste("Average Accuracy: ", mean(accuracy_values)))

#plot acuracies for each  kfold cross validation
ggplot(as.data.frame(accuracy_values), aes(x = row_number(as.data.frame(accuracy_values)), y = accuracy_values)) + geom_line() + geom_point() + xlab("k-fold Cross Validation Iteration") + ylab("Accuracy Score") + ggtitle("Cross Validation Accuracy Scores -- White Sox Batters")













#Pitchers
# Use CWSsalpitchstats18reduced

#Number of samples = 19
nrow(CWSsalpitchstats18reduced)


#convert all columns to numeric


#remove non stat columns

CWSsalpitchstats18reduced <- CWSsalpitchstats18reduced[,c(1,5:23)]

#Test accuracy for single fold validation. Using a single train/test split to find optimal k Value
# if no clear best choice, then use sqrt(nrow(dataset))

k_folds <- 3
holdout <- split(sample(1:nrow(CWSsalpitchstats18reduced)),1:k_folds)

accuracy_values <- c()

#loop through kfolds in 3 fold cross validation. Manually change k in knn function from 1 to 5
for (k in 1:k_folds) {
  test <- sample_n(CWSsalpitchstats18reduced[holdout[[k]],],1000, replace = TRUE)
  test_labels <- test[,1, drop = TRUE]
  #test <- sample_n(CWSsalpitchstats18reduced[holdout[[k]],-c(1)],4)
  test <- test[,-c(1)]
  test <- na.omit(test)
  
  train <- sample_n(CWSsalpitchstats18reduced[-holdout[[k]],],5000, replace = TRUE)
  train <- na.omit(train)
  classification_labels <- train$salCategory
  train <- train[,-c(1)]
  
  kNNmodel <- knn(train = train, test = test, cl = classification_labels, k = 3)
  
  #output <- data.frame(Predictions = kNNmodel, Actual = test_labels)
  #print(output)
  #create predictions
  accuracy_table <- table(Predicted = kNNmodel, Actual = test_labels)
  
  #generate accuracy score
  accuracy <- sum(diag(accuracy_table)) / sum(accuracy_table)
  
  #append to accuracy_values
  accuracy_values <- append(accuracy_values,accuracy)
  
  #print out accuracy score
  print(paste("K-Fold Iteration: ", k, "    Accuracy: ", accuracy))
  
}

#average accuracy values for above iteration
print(paste("Average Accuracy: ", mean(accuracy_values)))

#plot acuracies for each  kfold cross validation
ggplot(as.data.frame(accuracy_values), aes(x = row_number(as.data.frame(accuracy_values)), y = accuracy_values)) + geom_line() + geom_point() + xlab("k-fold Cross Validation Iteration") + ylab("Accuracy Score") + ggtitle("Cross Validation Accuracy Scores -- White Sox Pitchers")
