setwd("C:/Users/gwandro/Documents/Analytics Program/Reading Datasets")
load("pls2016.RData")
#set location to pull pls2016 data


players2016 =  data.frame()
playing_positions = data.frame()
teams = data.frame()
leagues = data.frame()
#creating the dataframes

for ( i in 1:length(pls2016)) {
  players2016 = rbind(players2016, pls2016[[i]]$players)
  playing_positions = rbind(playing_positions, pls2016[[i]]$playing_position)
  teams = rbind(teams, pls2016[[i]]$teams)
  leagues = rbind(leagues, pls2016[[i]]$leagues)
}
#converting the data from pls2016 into the dataframes, using row bind

dim(leagues)
dim(players2016)
dim(playing_positions)
dim(teams)

ls(teams)
ls(players2016)

load("gls2016.RData")
head(gls2016[[1]])
names(gls2016[[1]])
names(pls2016[[1]])
#loading game logs data


game_logs = data.frame()
players = data.frame()
winning_teams = data.frame()
#creating game log data frames


for ( i in 1:length(gls2016)) {
  game_logs = rbind(game_logs, gls2016[[i]]$game_logs)
  players = rbind(players, gls2016[[i]]$players)
  winning_teams = rbind(winning_teams, gls2016[[i]]$winning_teams)
}
#creating data frames for gls2016

dim(game_logs)

colnames(players2016)[1]  =  "player_id"
names(players2016)
#rename id column in players2016

player_logs = merge(players2016, game_logs, by.x = "player_id", by.y ="player_id")
names(player_logs)
#merging game_logs and players2016 by player id column

#Creating a dataframe that only incorporates starting pitchers
pitchers = player_logs[player_logs$position_name == "Starter",]
save(pitchers, file = "pitchers.RData")
#Saving pitchers data frame

pitchers$pitch_metric = pitchers$strikeouts / pitchers$pitches_thrown
pitchers[, c("strikeouts", "pitches_thrown")]

#pitchers_2 is a subset of data with no NAs for strikeouts
pitchers_2 = pitchers[!is.na(pitchers$strikeouts),]
pitchers_2 = pitchers_2[pitchers_2$strikeouts > 0,]

sum(is.na(pitchers$strikeouts))/nrow(pitchers)
sum(is.na(pitchers_2$strikeouts))/nrow(pitchers_2)
#checking to see if there are any NAs in pitchers_2$strikeouts

pitchers_2$pitch_metric = pitchers_2$strikeouts / pitchers_2$pitches_thrown

#creating a dataframe to see season total per player
pitchers_3 = data.frame(aggregate(pitchers_2$pitch_metric, by=list(pitchers_2$player_id), FUN = sum))
pitchers_3 = merge(pitchers_3, pitchers_2, by.x = "Group.1", by.y = "player_id")


eff_pitcher = pitchers_3[order(unique(-pitchers_3$x)),][1:10,]
eff_pitcher

#Pulling top 5 most effective unique pitchers season total
  #7   0669e315-e6e9-49af-8016-8f603b9c183a 0.25092908
    #team_id.y: f53d7ed3-1ebe-4e02-979d-6e51c0f5fb18

  #88  808eb897-4041-45e4-b2cb-fc9fa5b8cb5f 0.25505043
    #team_id.y: 9ebc3cf3-9559-4b74-b8ad-f15f2c5eae71
  
#110 a2e41146-34fd-4ef9-a8c0-d8de0faba4ac 0.14117956
    #team_id.y:0220ab72-3a17-4a65-8872-72454aeb828d

  #28  2d4acf7d-0527-41b6-a7a5-a4bd824b5334 0.14072871
    #team_id.y:6c498284-84e6-4e74-9f56-b934be3384d5

  #64  5f24d270-afa2-4904-95bb-b3bfa985d495 0.03400563
    #team_id.y:8a72ea07-afce-4907-9e6a-13b3f4e9d378

#Finding the players team ID, to see if their team performed well during season
#subset(pitchers_2, player_id == "0669e315-e6e9-49af-8016-8f603b9c183a")
#subset(pitchers_2, player_id == "808eb897-4041-45e4-b2cb-fc9fa5b8cb5f")
#subset(pitchers_2, player_id == "a2e41146-34fd-4ef9-a8c0-d8de0faba4ac")
#subset(pitchers_2, player_id == "2d4acf7d-0527-41b6-a7a5-a4bd824b5334")
#subset(pitchers_2, player_id == "5f24d270-afa2-4904-95bb-b3bfa985d495")

team_pitch = aggregate(pitchers_2$pitch_metric, by=list(pitchers_2$team_id.y), FUN = sum)
team_rank_pitch = team_pitch[order(-team_pitch$x),][1:20,]
team_rank_pitch

#Top 5 teams with highest pitch_metric by team.id.x
#11 4c2ad3df-a7fd-458d-a412-6e31574a0b7b 0.17814773
#23 c5fefa38-a256-43fe-8147-008c0f8c2f15 0.14195937
#22 a7768b3c-3700-41a8-b97d-4670e044d036 0.11831465
#14 8256c5cd-476c-4af3-8c26-059b0fd8ddcf 0.10448544
#20 9ccdfc26-a7e7-4bcb-8879-3e77d391d58f 0.09869243

#Converting team_outcome from wins to 1
player_logs["win_indicator"] =  ifelse(player_logs$team_outcome == "win",1, 0) 

team_4c2ad = player_logs[player_logs$team_id.y == "4c2ad3df-a7fd-458d-a412-6e31574a0b7b",]
team_c5fef = player_logs[pitchers_2$team_id.y == "c5fefa38-a256-43fe-8147-008c0f8c2f15",]
team_a7768 = player_logs[pitchers_2$team_id.y == "a7768b3c-3700-41a8-b97d-4670e044d036",]
team_8256c = player_logs[pitchers_2$team_id.y == "8256c5cd-476c-4af3-8c26-059b0fd8ddcf",]
team_9ccdf = player_logs[pitchers_2$team_id.y == "9ccdfc26-a7e7-4bcb-8879-3e77d391d58f",]

#calculating total wins per team in the season
team_4c2ad = unique(team_4c2ad[c("game_id", "win_indicator")])
team_4c2ad["total_wins"] = sum(team_4c2ad$win_indicator)
team_4c2ad$total_wins
#107 wins 

team_c5fef = unique(team_c5fef[c("game_id", "win_indicator")])
team_c5fef["total_wins"] = sum(team_c5fef$win_indicator)
team_c5fef$total_wins
#150 wins

team_a7768 = unique(team_a7768[c("game_id", "win_indicator")])
team_a7768["total_wins"] = sum(team_a7768$win_indicator)
team_a7768$total_wins
#86 wins

team_8256c = unique(team_8256c[c("game_id", "win_indicator")])
team_8256c["total_wins"] = sum(team_8256c$win_indicator)
team_8256c$total_wins
#114 wins

team_9ccdf = unique(team_9ccdf[c("game_id", "win_indicator")])
team_9ccdf["total_wins"] = sum(team_9ccdf$win_indicator)
team_9ccdf$total_wins
#134 wins

pitch_team_id = c("Team 1"
                ,"Team 2"
                ,"Team 3"
                ,"Team 4"
                ,"Team 5"
)
#4c2ad3df-a7fd-458d-a412-6e31574a0b7b = Team 1
#c5fefa38-a256-43fe-8147-008c0f8c2f15 = Team 2
#a7768b3c-3700-41a8-b97d-4670e044d036 = Team 3
#8256c5cd-476c-4af3-8c26-059b0fd8ddcf = Team 4
#9ccdfc26-a7e7-4bcb-8879-3e77d391d58f = Team 5


pitching_met = c(0.17814773
                 ,0.14195937
                 ,0.11831465
                 ,0.10448544
                 ,0.09869243
)
pitch_graph = data.frame(pitch_team_id,pitching_met)
plot(pitch_graph
     ,xlab = "Team Names"
     ,ylab = "Pitching Metric"
     ,main = "Top Pitching Teams")

#Findingthe batting metric

#removing NAs from runs_batted_in, and values > 0
rbi = player_logs[!is.na(player_logs$runs_batted_in),]
rbi = rbi[rbi$runs_batted_in > 0,]

rbi$rbi_metric = rbi$runs_batted_in / rbi$at_bats
eff_rbi = rbi[order(unique(-rbi$rbi_metric)),][1:20,]
eff_rbi

#Top player, with top RBI per game
#2c755f68-04a7-464f-9f7a-302b40241736

player_total_rbi = aggregate(rbi$rbi_metric, by=list(rbi$player_id), FUN = sum)
player_total_rbi 
max_player_total = player_total_rbi[order(unique(-player_total_rbi$x)),][1:20,]
max_player_total
#


#calculating total wins per team in the season
team_d6c51 = subset(pitchers_2, team_id.y == "d6c517b0-78de-4834-8d36-b6e63b3224ee")
team_d6c51["win_indicator"] =  ifelse(team_d6c51$team_outcome == "win",1, 0)
team_d6c51["total_wins"] = sum(team_d6c51$win_indicator)
team_d6c51$total_wins
#76 wins

top_rbi_plr = subset(player_logs, player_id == "2c755f68-04a7-464f-9f7a-302b40241736",)
hist(top_rbi_plr$runs_batted_in
     ,xlab = "Runs Batted In"
     ,ylab = "Frequency"
     ,main = "RBIs for the Season"
     ,col = "blue")

at_bats = sum(top_rbi_plr$at_bats)
at_bats

rbi = sum(top_rbi_plr$runs_batted_in)
rbi
top_rbi_metric = rbi / at_bats
top_rbi_metric
