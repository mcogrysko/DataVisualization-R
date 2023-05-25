#Mike Ogrysko
#DS736
#Module 1 Deliverable

#libraries
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(DescTools)
library(stringr)
library(plotly)
library(lubridate)

#dataset
#https://www.kaggle.com/datasets/die9origephit/fifa-world-cup-2022-complete-dataset
setwd("/Users/mogrysko/Documents/Coursework/DataScience/DVDM/Mod1/assignment")
filename <- "Fifa_world_cup_matches.csv" 

#load data
df <- fread(filename, na.strings=c(NA, ""))

ncol(df)

df$team1 <- str_to_title(df$team1)
df$team2 <- str_to_title(df$team2)

#round of 16 teams
r_16_teams <- c("Netherlands", "United States", "Argentina", "Australia", "France", "Poland", "England", "Senegal", "Japan", "Croatia", "Brazil", "Korea Republic", "Morocco", "Spain", "Portugal", "Switzerland")

#quarterfinals teams
q_final_teams <- c("Croatia", "Brazil", "Netherlands", "Argentina", "Morocco", "Portugal", "England", "France")

#final 4 teams
final_4_teams <- c("Croatia", "Argentina", "Morocco", "France")



### Plot 1 ###
### Stacked plot showing the 1st round attempts by teams that made the quarterfinals

#get df on only first round rows
first_round_rows <- which(df$category %like any% c("%Group%"))
first_round_df <- df[first_round_rows,]

#get dfs of teams and their attempts
#team 1
team1_df <- first_round_df %>% 
  select(team1, 'on target attempts team1', 'off target attempts team1', 'attempts inside the penalty area team1', 'attempts outside the penalty area  team1') %>%
  filter(team1 %in% final_4_teams) %>%
  data.frame()

#team 2
team2_df <- first_round_df %>% 
  select(team2, 'on target attempts team2', 'off target attempts team2', 'attempts inside the penalty area  team2', 'attempts outside the penalty area  team2') %>%
  filter(team2 %in% q_final_teams) %>%
  data.frame()

#rename headers in team 1 df
team1_df <- team1_df %>% 
  rename(
    team = team1,
    on_target = on.target.attempts.team1,
    off_target = off.target.attempts.team1,
    inside_penalty = attempts.inside.the.penalty.area.team1,
    outside_penalty = attempts.outside.the.penalty.area..team1
  )

#rename headers in team 2 df
team2_df <- team2_df %>% 
  rename(
    team = team2,
    on_target = on.target.attempts.team2,
    off_target = off.target.attempts.team2,
    inside_penalty = attempts.inside.the.penalty.area..team2,
    outside_penalty = attempts.outside.the.penalty.area..team2
  )

#combine team dfs - team and attempts -  on_target, off_target, inside_penalty, outside_penalty 
attempts_df <- rbind(team1_df, team2_df)

#create sums of all columns
on_target_sum <- sum(attempts_df$on_target)
off_target_sum <- sum(attempts_df$off_target)
inside_penalty_sum <- sum(attempts_df$inside_penalty)
outside_penalty_sum <- sum(attempts_df$outside_penalty)

#create Description for new df
Description <- c("On Target", "Off Target", "Inside Penalty", "Outside Penalty")

#create total column for new df using sums of columns
total <- c(on_target_sum, off_target_sum, inside_penalty_sum, outside_penalty_sum)

#create new df description of attempt and the totals for each
attempts_sum_df <- data.frame(Description, total)

#create df of on_target
on_target_df <- attempts_df %>% 
  select(team, on_target) %>%
  group_by(team) %>%
  summarise(n = sum(on_target), .groups='keep') %>%
  data.frame()

on_target_df$Description = "On Target"

#create df of off_target  
off_target_df <- attempts_df %>% 
  select(team, off_target) %>%
  group_by(team) %>%
  summarise(n = sum(off_target), .groups='keep') %>%
  data.frame()

off_target_df$Description = "Off Target"

#create df of inside_penalty
inside_penalty_df <- attempts_df %>% 
  select(team, inside_penalty) %>%
  group_by(team) %>%
  summarise(n = sum(inside_penalty), .groups='keep') %>%
  data.frame()

inside_penalty_df$Description = "Inside Penalty"

#create df of outside_penalty
outside_penalty_df <- attempts_df %>% 
  select(team, outside_penalty) %>%
  group_by(team) %>%
  summarise(n = sum(outside_penalty), .groups='keep') %>%
  data.frame()

outside_penalty_df$Description = "Outside Penalty"

#create df combining on_target, off_target, inside_penalty, outside_penalty
team_attempts_df <- rbind(on_target_df, off_target_df, inside_penalty_df, outside_penalty_df)

#stacked plot using team_attempts_df and attempts_sum_df - shows the 1st round attempts by teams that made the quarterfinals
max_y <- round_any(max(attempts_sum_df$total), 200, ceiling)
p1m1 <- ggplot(team_attempts_df, aes(x = reorder(Description, n, sum), y = n, fill = team)) +
  geom_bar(stat="identity", position = position_stack(reverse=TRUE)) +
  coord_flip() + 
  labs(title = "1st Round Attempts Count by Quarterfinals Teams", x="", y="Attempt Count", fill = "Team") +
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_brewer(palette="Paired", guide = guide_legend(reverse=FALSE)) + 
  geom_text(data = attempts_sum_df, aes(x=Description, y = total, label = scales::comma(total), fill = NULL), hjust = -0.1, size = 4) + 
  scale_y_continuous(breaks = seq(0, max_y, by = 25),
                     limits=c(0, max_y))
p1m1




### Plot 2 ###
### Multiple line plots showing the fouls for the final 4 teams in all games ###

#create list of teams and their fouls
#team 1
team1_fouls_df <- df %>% 
  select(team1, 'fouls against team1', date) %>%
  filter(team1 %in% final_4_teams) %>%
  data.frame()

#team 2
team2_fouls_df <- df %>% 
  select(team2, 'fouls against team2', date) %>%
  filter(team2 %in% final_4_teams) %>%
  data.frame()

#rename team and fouls_against
team1_fouls_df <- team1_fouls_df %>% 
  rename(team = team1,fouls_against = fouls.against.team1)
team2_fouls_df <- team2_fouls_df %>% 
  rename(team = team2,fouls_against = fouls.against.team2)

#combine datasets
fouls_df <- rbind(team1_fouls_df, team2_fouls_df)

#arrange by date
fouls_df <- fouls_df %>% arrange(dmy(fouls_df$date))

#create game column
Game <- c("1", "2", "3", "4", "5", "6", "7")

#create Argentina df
arg_fouls_df <- fouls_df %>%
  filter(team %in% "Argentina") %>%
  data.frame()

arg_fouls_df <- data.frame(arg_fouls_df$fouls_against, arg_fouls_df$team, Game)
arg_fouls_df <- arg_fouls_df %>% 
  rename(n = arg_fouls_df.fouls_against, team = arg_fouls_df.team)

#create French df
fr_fouls_df <- fouls_df %>%
  filter(team %in% "France") %>%
  data.frame()

fr_fouls_df <- data.frame(fr_fouls_df$fouls_against, fr_fouls_df$team, Game)
fr_fouls_df <- fr_fouls_df %>% 
  rename(n = fr_fouls_df.fouls_against, team = fr_fouls_df.team)

#create Morocco df
mor_fouls_df <- fouls_df %>%
  filter(team %in% "Morocco") %>%
  data.frame()

mor_fouls_df <- data.frame(mor_fouls_df$fouls_against, mor_fouls_df$team, Game)
mor_fouls_df <- mor_fouls_df %>% 
  rename(n = mor_fouls_df.fouls_against, team = mor_fouls_df.team)

#create Croatia df
cr_fouls_df <- fouls_df %>%
  filter(team %in% "Croatia") %>%
  data.frame()

cr_fouls_df <- data.frame(cr_fouls_df$fouls_against, cr_fouls_df$team, Game)
cr_fouls_df <- cr_fouls_df %>% 
  rename(n = cr_fouls_df.fouls_against, team = cr_fouls_df.team)

#combine team dfs - team, game #, and n # of fouls
all_fouls <- rbind(arg_fouls_df, fr_fouls_df, mor_fouls_df, cr_fouls_df)

str(all_fouls)
#make game a factor and set order
all_fouls$Game <- as.factor(all_fouls$Game)
game_order <- factor(all_fouls$Game, level = c('1', '2', '3','4','5','6','7'))

p2mod1 <- ggplot(all_fouls, aes(x=game_order, y=n, group=team)) + 
  geom_line(aes(color=team), size=3) + 
  labs(title = "Final 4 Team Fouls by Game", x = "Game", y = "Foul Count") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(shape = 21, size = 5, color = "black", fill="white") + 
  scale_y_continuous(labels=comma) +
  scale_color_brewer(palette = "Paired", name = "Team", guide = guide_legend(reverse = TRUE))

p2mod1









###Plot 3 ###
### Multiple bar charts - forced turnovers by teams that made the quarterfinals

#create list of teams and their forced turnovers
#team 1
team1_forced_df <- df %>% 
  select(team1, 'forced turnovers team1', date) %>%
  filter(team1 %in% final_4_teams) %>%
  data.frame()

#team 2
team2_forced_df <- df %>% 
  select(team2, 'forced turnovers team2', date) %>%
  filter(team2 %in% final_4_teams) %>%
  data.frame()

#rename team and fouls_against
team1_forced_df <- team1_forced_df %>% 
  rename(team = team1,forced_turnovers = forced.turnovers.team1)
team2_forced_df <- team2_forced_df %>% 
  rename(team = team2,forced_turnovers = forced.turnovers.team2)

#combine datasets
forced_df <- rbind(team1_forced_df, team2_forced_df)

#arrange by date
forced_df <- forced_df %>% arrange(dmy(forced_df$date))

#create Argentina df
arg_forced_df <- forced_df %>%
  filter(team %in% "Argentina") %>%
  data.frame()

arg_forced_df <- data.frame(arg_forced_df$forced_turnovers, arg_forced_df$team, Game)
arg_forced_df <- arg_forced_df %>% 
  rename(n = arg_forced_df.forced_turnovers, team = arg_forced_df.team)

#create French df
fr_forced_df <- forced_df %>%
  filter(team %in% "France") %>%
  data.frame()

fr_forced_df <- data.frame(fr_forced_df$forced_turnovers, fr_forced_df$team, Game)
fr_forced_df <- fr_forced_df %>% 
  rename(n = fr_forced_df.forced_turnovers, team = fr_forced_df.team)

#create Morocco df
mor_forced_df <- forced_df %>%
  filter(team %in% "Morocco") %>%
  data.frame()

mor_forced_df <- data.frame(mor_forced_df$forced_turnovers, mor_forced_df$team, Game)
mor_forced_df <- mor_forced_df %>% 
  rename(n = mor_forced_df.forced_turnovers, team = mor_forced_df.team)

#create Croatia df
cr_forced_df <- forced_df %>%
  filter(team %in% "Croatia") %>%
  data.frame()

cr_forced_df <- data.frame(cr_forced_df$forced_turnovers, cr_forced_df$team, Game)
cr_forced_df <- cr_forced_df %>% 
  rename(n = cr_forced_df.forced_turnovers, team = cr_forced_df.team)

#combine team dfs - team, game #, and n # of fouls
all_forced <- rbind(arg_forced_df, fr_forced_df, mor_forced_df, cr_forced_df)

str(all_forced)
#make game a factor and set order
all_forced$Game <- as.factor(all_forced$Game)
game_order <- factor(all_forced$Game, level = c('1', '2', '3','4','5','6','7'))

x = min(as.numeric(levels(all_forced$Game)))
y = max(as.numeric(levels(all_forced$Game)))

all_forced$Game <- factor(all_forced$Game, levels=seq(y, x, by=-1))


#team # and total number of forced turnovers per game
team_forced <- all_forced %>%
  select(team, n) %>%
  group_by(team) %>%
  summarise(tot = sum(n), .groups='keep') %>%
  data.frame()

label_names <- c('Argentina' = 'Argentina (494)','Croatia' = 'Croatia (548)','France' = 'France (524)','Morocco' = 'Morocco (544)')


p3mod1 <- ggplot(all_forced, aes(x = game_order, y = n, fill = team)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = comma) +
  labs(title = "Multiple Bar Charts - Forced Turnovers by Game by Team - Final 4 Teams",
       x="Game",
       y="Forced Turnovers Count",
       fill = "Team") + 
  scale_fill_brewer(palette = "Set2") + 
  facet_wrap(~team, ncol=2, nrow=2, labeller = as_labeller(label_names)) + #breaks into teams
  geom_text(aes(Game, n, label = n, fill = NULL), data = all_forced, vjust=-0.5, size = 2)

p3mod1



###Plot 4 ###
### Heatmap - # passes final 4 teams

#create list of teams and their passes completed
#team 1
team1_pass_df <- df %>% 
  select(team1, 'passes completed team1', date) %>%
  filter(team1 %in% final_4_teams) %>%
  data.frame()

#team 2
team2_pass_df <- df %>% 
  select(team2, 'passes completed team2', date) %>%
  filter(team2 %in% final_4_teams) %>%
  data.frame()

#rename team and passes_comp
team1_pass_df <- team1_pass_df %>% 
  rename(team = team1,
         passes_comp = passes.completed.team1)
team2_pass_df <- team2_pass_df %>% 
  rename(team = team2,
         passes_comp = passes.completed.team2)

#combine datasets
pass_df <- rbind(team1_pass_df, team2_pass_df)

#arrange by date
pass_df <- pass_df %>% arrange(dmy(pass_df$date))

#create Argentina df
arg_pass_df <- pass_df %>%
  filter(team %in% "Argentina") %>%
  data.frame()

arg_pass_df <- data.frame(arg_pass_df$passes_comp, arg_pass_df$team, Game)
arg_pass_df <- arg_pass_df %>% 
  rename(team = arg_pass_df.team, passes_comp = arg_pass_df.passes_comp)

#create French df
fr_pass_df <- pass_df %>%
  filter(team %in% "France") %>%
  data.frame()

fr_pass_df <- data.frame(fr_pass_df$passes_comp, fr_pass_df$team, Game)
fr_pass_df <- fr_pass_df %>% 
  rename(team = fr_pass_df.team, passes_comp = fr_pass_df.passes_comp)

#create Morocco df
mor_pass_df <- pass_df %>%
  filter(team %in% "Morocco") %>%
  data.frame()

mor_pass_df <- data.frame(mor_pass_df$passes_comp, mor_pass_df$team, Game)
mor_pass_df <- mor_pass_df %>% 
  rename(team = mor_pass_df.team, passes_comp = mor_pass_df.passes_comp)

#create Croatia df
cr_pass_df <- pass_df %>%
  filter(team %in% "Croatia") %>%
  data.frame()

cr_pass_df <- data.frame(cr_pass_df$passes_comp, cr_pass_df$team, Game)
cr_pass_df <- cr_pass_df %>% 
  rename(team = cr_pass_df.team, passes_comp = cr_pass_df.passes_comp)

#combine team dfs - team, game #, and n # of fouls
all_pass <- rbind(arg_pass_df, fr_pass_df, mor_pass_df, cr_pass_df)


mylevels <- c('1','2','3','4','5','6','7')
all_pass$Game <- factor(all_pass$Game, levels = mylevels)

breaks <- c(seq(200, 800, by=200))

p4mod1 <- ggplot(all_pass, aes(x = team, y = Game, fill=passes_comp)) + 
  geom_tile(color="black") + 
  geom_text(aes(label=passes_comp)) + 
  coord_equal(ratio=1) + 
  labs(title="Heatmap: Passes Completed by Team by Game",
       x="Team",
       y="Game",
       fill = "Passes Completed") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_discrete(limits = rev(levels(all_pass$Game))) +
  scale_fill_continuous(low="white", high="red", breaks=breaks) + 
  guides(fill = guide_legend(reverse=TRUE, override.aes=list(colour="black")))

p4mod1



###Plot 5 ###
### trellis pies - time of possession Argentina's games

#Argentina
team_arg_poss_df <- df %>% 
  select(team1, team2, 'possession team1', 'possession team2', 'possession in contest', date) %>%
  filter(team1 %in% 'Argentina' | team2 %in% 'Argentina') %>%
  data.frame()

#arrange by date
team_arg_poss_df <- team_arg_poss_df %>% arrange(dmy(team_arg_poss_df$date))

#create game column
Game <- c("1", "2", "3", "4", "5", "6", "7")

team_arg_poss_df$Game <- Game
team_arg_poss_df$contest <- "Contested"

#game # and total number of fouls per game
team1_arg <- team_arg_poss_df %>%
  select(Game, team1, possession.team1) %>%
  data.frame()

team1_arg <- team1_arg %>% 
  rename(
    team = team1,
    possession = possession.team1
  )

team2_arg <- team_arg_poss_df %>%
  select(Game, team2, possession.team2) %>%
  data.frame()

team2_arg <- team2_arg %>% 
  rename(
    team = team2,
    possession = possession.team2
  )

contest_arg <- team_arg_poss_df %>%
  select(Game, contest, possession.in.contest) %>%
  data.frame()

contest_arg <- contest_arg %>% 
  rename(
    team = contest,
    possession = possession.in.contest
  )

#combine arg dfs
all_arg <- rbind(team1_arg, team2_arg, contest_arg)



str(all_arg)
#add possession time
#all_arg$possession_time <- all_arg$possession

#remove % from possession time
all_arg$possession <- gsub('%','',all_arg$possession)

#convert to numeric
all_arg$possession <- as.numeric(all_arg$possession)

team_order <- factor(all_arg$team, level = c('Argentina', 'Contest', 'Saudi Arabia','Mexico','Poland','Australia','Netherlands','Croatia','France'))


p5mod1 <- plot_ly(textposition="inside", labels = ~team, values = ~possession) %>%
  add_pie(data=all_arg[all_arg$Game == 1,], 
          name="1", title="Game 1", domain=list(row=0, column=0), hovertemplate = paste('Team: %{label}<br>Possession: %{value}%<br>')) %>%
  add_pie(data=all_arg[all_arg$Game == 2,], 
          name="2", title="Game 2", domain=list(row=0, column=1)) %>%
  add_pie(data=all_arg[all_arg$Game == 3,], 
          name="3", title="Game 3", domain=list(row=0, column=2)) %>%
  add_pie(data=all_arg[all_arg$Game == 4,], 
          name="4", title="Game 4", domain=list(row=1, column=0)) %>%
  add_pie(data=all_arg[all_arg$Game == 5,], 
          name="5", title="Game 5", domain=list(row=1, column=1)) %>%
  add_pie(data=all_arg[all_arg$Game == 6,], 
          name="6", title="Game 6", domain=list(row=1, column=2)) %>%
  add_pie(data=all_arg[all_arg$Game == 7,], 
          name="7", title="Game 7", domain=list(row=2, column=1)) %>%
  layout(title="Trellis Chart: Time of Possession in Argentina's Games", showlegend = TRUE,
         grid=list(rows=3, columns=3))

p5mod1

#, 
#values = ~all_arg[all_arg$Game=="1","possession"],
#hovertext="Possession %: %{value}<extra></extra>"






