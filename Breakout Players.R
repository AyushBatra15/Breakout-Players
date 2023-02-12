rm(list=ls())
library(tidyverse)
library(modelr)
library(nbastatR)
library(ggimage)
library(hoopR)
library(cowplot)
library(ggtext)
setwd("~/Desktop")
Sys.setenv("VROOM_CONNECTION_SIZE" = 2*131072)

#SEE ARTICLE HERE: https://bestballstats.com/2022/11/13/predicting-nba-first-time-all-stars-in-2022-23/

theme_bbs <- function() {
  font = 'Arial Unicode MS'
  bg = "#E4DBD7"
  light_ln = "#A0A0A0"
  dark_ln = "#404040"
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = bg, color = NA),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.background = element_blank(),
          panel.grid = element_line(color = light_ln),
          panel.grid.minor = element_blank(),
          plot.title = element_text(family = font, size = 18, face = 'bold', hjust = 0.5, vjust = 2),
          plot.subtitle = element_text(family = font, size = 12, hjust = 0.5, vjust = 1),
          plot.caption = element_text(family = font, size = 9, hjust = 1, vjust = -5, color = dark_ln),
          axis.title.x = element_text(family = font, size = 15, vjust = -2, color = dark_ln),
          axis.title.y = element_text(family = font, size = 15,  angle = 90, vjust = 3, color = dark_ln),
          axis.text = element_text(family = font, size = 13, color = dark_ln),
          legend.title = element_text(family = font, size = 13, color = dark_ln, face = 'bold', hjust = 0.5),
          legend.text = element_text(family = font, size = 12, color = dark_ln),
          legend.box.background = element_blank(),
          axis.ticks = element_line(color = light_ln),
          plot.margin = unit(c(1,1,1,1),"cm"))
}

assign_nba_players()
assign_nba_teams()


pct <- function(stat) {
  pct = round(100*rank(stat, ties.method = 'max')/max(rank(stat, ties.method = 'max')))
  return(pct)
}



#List of Players that were All-Stars before 2017-18 Season
stars <- c("Giannis Antetokounmpo","Kemba Walker","DeAndre Jordan","Gordon Hayward",
           "Andre Drummond","Isaiah Thomas","Kawhi Leonard","Draymond Green",
           "Kyle Lowry","Jimmy Butler","Jeff Teague","Kyle Korver","Klay Thompson","DeMarcus Cousins",
           "Paul Millsap","John Wall","DeMar DeRozan","Stephen Curry","Damian Lillard","Anthony Davis",
           "Tyson Chandler","Paul George","Jrue Holiday","Kyrie Irving","Joakim Noah","Brook Lopez","James Harden",
           "Andrew Bynum","LaMarcus Aldridge","Marc Gasol","Luol Deng","Roy Hibbert","Andre Iguodala",
           "Blake Griffin","Kevin Love","Russell Westbrook")

#Create a tibble of first-time All-Stars and the year of their first selection
breakout <- tibble(
  Player = c("LaMelo Ball","Darius Garland","Fred VanVleet","Jarrett Allen","Ja Morant","Andrew Wiggins","Dejounte Murray",
             "Jaylen Brown","Zach LaVine","Julius Randle","Mike Conley","Zion Williamson",
             "Trae Young","Pascal Siakam","Bam Adebayo","Jayson Tatum","Domantas Sabonis","Luka Doncic","Donovan Mitchell","Brandon Ingram","Rudy Gobert","Devin Booker",
             "Khris Middleton","Ben Simmons","Nikola Vucevic","D'Angelo Russell","Nikola Jokic",
             "Joel Embiid","Bradley Beal","Goran Dragic","Victor Oladipo","Kristaps Porzingis","Karl-Anthony Towns"),
  Season = c(rep(2022,7), rep(2021,5), rep(2020,10), rep(2019,5), rep(2018,6))
)



#Gather standard and tracking statistics for a given season, combine into 1 dataframe
gather_stats <- function(season) {
  
  S1 <- season - 2000
  S2 <- season - 1
  String <- paste(as.character(S2),"-",as.character(S1), sep = "")
  
  ages <- nba_leaguedashplayerstats(season = String)
  ages <- ages[["LeagueDashPlayerStats"]]
  
  ages <- ages %>%
    select(PLAYER_ID, PLAYER_NAME, TEAM = TEAM_ABBREVIATION, AGE, MIN, PTS, AST, OREB, DREB, TOV, STL, BLK) %>%
    mutate_at(4:12, as.numeric) %>%
    mutate(MIN = round(MIN)) %>%
    filter(AGE <= 26, MIN >= 600)
  
  adv <- nba_leaguedashplayerstats(season = String, measure_type = "Advanced")
  adv <- adv[["LeagueDashPlayerStats"]]
  
  adv <- adv %>%
    select(PLAYER_ID, PLAYER_NAME, POSS, AST_PCT, TS_PCT, USG_PCT) %>%
    mutate_at(3:6, as.numeric)
  
  stats <- ages %>%
    left_join(adv, by = c("PLAYER_ID","PLAYER_NAME"))
  
  scoring <- nba_leaguedashplayerstats(season = String, measure_type = "Scoring")
  scoring <- scoring[["LeagueDashPlayerStats"]]
  
  scoring <- scoring %>%
    select(PLAYER_ID, PLAYER_NAME, PCT_PTS_2PT_MR, PCT_PTS_3PT, PCT_PTS_FB, PCT_UAST_FGM) %>%
    mutate_at(3:6, as.numeric)
  
  stats <- stats %>%
    left_join(scoring, by = c("PLAYER_ID","PLAYER_NAME"))
  
  pu <- nba_leaguedashptstats(season = String, pt_measure_type = "PullUpShot", player_or_team = "Player")
  drive <- nba_leaguedashptstats(season = String, pt_measure_type = "Drives", player_or_team = "Player")
  post <- nba_leaguedashptstats(season = String, pt_measure_type = "PostTouch", player_or_team = "Player")
  
  pu <- pu[["LeagueDashPtStats"]]
  drive <- drive[["LeagueDashPtStats"]]
  post <- post[["LeagueDashPtStats"]]
  
  pu <- pu %>%
    select(PLAYER_ID, PLAYER_NAME, PULL_UP_PTS, PULL_UP_FGA) %>%
    mutate_at(3:4, as.numeric)
  drive <- drive %>%
    select(PLAYER_ID, PLAYER_NAME, DRIVE_PTS, DRIVE_FGA, DRIVE_FTA) %>%
    mutate_at(3:5, as.numeric)
  post <- post %>%
    select(PLAYER_ID, PLAYER_NAME, POST_TOUCH_PTS, POST_TOUCH_FGA, POST_TOUCH_FTA) %>%
    mutate_at(3:5, as.numeric)
  
  stats <- stats %>%
    left_join(pu, by = c("PLAYER_ID","PLAYER_NAME")) %>%
    left_join(drive, by = c("PLAYER_ID","PLAYER_NAME")) %>%
    left_join(post, by = c("PLAYER_ID","PLAYER_NAME"))
  
  hustle <- nba_leaguehustlestatsplayer(season = String)
  hustle <- hustle[["HustleStatsPlayer"]]
  
  hustle <- hustle %>%
    select(PLAYER_ID, PLAYER_NAME, CONTESTED_SHOTS_2PT, CONTESTED_SHOTS_3PT, DEFLECTIONS,
           SCREEN_ASSISTS) %>%
    mutate_at(3:6, as.numeric)
  
  stats <- stats %>%
    left_join(hustle, by = c("PLAYER_ID","PLAYER_NAME"))
  
  stats <- stats %>%
    mutate(Season = season) %>%
    select(1:3, Season, 4:33)
  
  return(stats)
}



empty <- c()

for (yr in c(2017:2022)) {
  df <- gather_stats(yr)
  empty <- rbind(empty, df)
  print(yr)
}


all <- all %>%
  left_join(breakout, by = c("PLAYER_NAME" = "Player"), suffix = c("",".breakout"))

non <- all %>%
  filter(is.na(Season.breakout)) %>%
  filter(Season < 2022) %>%
  filter(PLAYER_NAME %in% stars == FALSE)
yes <- all %>%
  filter(!is.na(Season.breakout)) %>%
  filter(Season < Season.breakout) %>%
  filter(Season < 2022) 

draft <- drafts(draft_years = 2010:2021)

draft <- draft %>%
  select(namePlayer, yearDraft, numberPickOverall)

stats <- rbind(non, yes) %>%
  replace_na(list(Season.breakout = 0)) %>%
  mutate(Breakout = ifelse(Season.breakout - Season == 1, 1, 0)) %>%
  select(-Season.breakout)

stats <- stats %>%
  left_join(draft, by = c("PLAYER_NAME" = "namePlayer"))

stats <- stats %>%
  replace_na(list(numberPickOverall = 80)) %>%
  mutate(PTS = PTS/POSS*100,
         OREB = OREB/POSS*100,
         DREB = DREB/POSS*100,
         AST = AST/POSS*100,
         TOV = TOV/POSS*100,
         STL = STL/POSS*100,
         BLK = BLK/POSS*100,
         PULL_UP_PTS = PULL_UP_PTS/POSS*100,
         PULL_UP_FGA = PULL_UP_FGA/POSS*100,
         PULL_UP_EFF = PULL_UP_PTS/PULL_UP_FGA,
         PULL_UP_EFF = (2/(1 + exp(-PULL_UP_FGA)) - 1)*PULL_UP_EFF,
         PULL_UP_EFF = ifelse(PULL_UP_FGA == 0, 0, PULL_UP_EFF),
         DRIVE_PTS = DRIVE_PTS/POSS*100,
         DRIVE_FGA = (DRIVE_FGA + 0.44*DRIVE_FTA)/POSS*100,
         DRIVE_EFF = DRIVE_PTS/DRIVE_FGA,
         DRIVE_EFF = (2/(1 + exp(-DRIVE_FGA)) - 1)*DRIVE_EFF,
         DRIVE_EFF = ifelse(DRIVE_FGA == 0, 0, DRIVE_EFF),
         POST_TOUCH_PTS = POST_TOUCH_PTS/POSS*100,
         POST_TOUCH_FGA = (POST_TOUCH_FGA + 0.44*POST_TOUCH_FTA)/POSS*100,
         POST_TOUCH_EFF = POST_TOUCH_PTS/POST_TOUCH_FGA,
         POST_TOUCH_EFF = (2/(1 + exp(-POST_TOUCH_FGA)) - 1)*POST_TOUCH_EFF,
         POST_TOUCH_EFF = ifelse(POST_TOUCH_FGA == 0, 0, POST_TOUCH_EFF),
         CONTESTED_SHOTS_2PT = CONTESTED_SHOTS_2PT/POSS*100,
         CONTESTED_SHOTS_3PT = CONTESTED_SHOTS_3PT/POSS*100,
         DEFLECTIONS = DEFLECTIONS/POSS*100,
         SCREEN_ASSISTS = SCREEN_ASSISTS/POSS*100,
         AST_TOV = AST/TOV) %>%
  select(-DRIVE_FTA, -POST_TOUCH_FTA)

stats <- stats %>% mutate(TSP_sqrd = TS_PCT^2)

fit <- glm(stats, family = binomial, 
           formula = Breakout ~ log(numberPickOverall) + TOV + STL + TS_PCT + PULL_UP_EFF + POST_TOUCH_EFF)
summary(fit)


stats <- stats %>%
  add_predictions(model = fit, var = 'phat_breakout', type = 'response')

rmse <- stats %>%
  mutate(se = (phat_breakout - Breakout)^2)
rmse <- round(sqrt(mean(rmse$se)),4)
print(rmse)


players22 <- all %>%
  filter(Season == 2022) %>%
  filter(is.na(Season.breakout)) %>%
  filter(PLAYER_NAME %in% stars == FALSE)

players22 <- players22 %>%
  left_join(draft, by = c("PLAYER_NAME" = "namePlayer")) %>%
  replace_na(list(numberPickOverall = 80)) %>%
  mutate(PTS = PTS/POSS*100,
         OREB = OREB/POSS*100,
         DREB = DREB/POSS*100,
         AST = AST/POSS*100,
         TOV = TOV/POSS*100,
         STL = STL/POSS*100,
         BLK = BLK/POSS*100,
         PULL_UP_PTS = PULL_UP_PTS/POSS*100,
         PULL_UP_FGA = PULL_UP_FGA/POSS*100,
         PULL_UP_EFF = PULL_UP_PTS/PULL_UP_FGA,
         PULL_UP_EFF = (2/(1 + exp(-PULL_UP_FGA)) - 1)*PULL_UP_EFF,
         PULL_UP_EFF = ifelse(PULL_UP_FGA == 0, 0, PULL_UP_EFF),
         DRIVE_PTS = DRIVE_PTS/POSS*100,
         DRIVE_FGA = (DRIVE_FGA + 0.44*DRIVE_FTA)/POSS*100,
         DRIVE_EFF = DRIVE_PTS/DRIVE_FGA,
         DRIVE_EFF = (2/(1 + exp(-DRIVE_FGA)) - 1)*DRIVE_EFF,
         DRIVE_EFF = ifelse(DRIVE_FGA == 0, 0, DRIVE_EFF),
         POST_TOUCH_PTS = POST_TOUCH_PTS/POSS*100,
         POST_TOUCH_FGA = (POST_TOUCH_FGA + 0.44*POST_TOUCH_FTA)/POSS*100,
         POST_TOUCH_EFF = POST_TOUCH_PTS/POST_TOUCH_FGA,
         POST_TOUCH_EFF = (2/(1 + exp(-POST_TOUCH_FGA)) - 1)*POST_TOUCH_EFF,
         POST_TOUCH_EFF = ifelse(POST_TOUCH_FGA == 0, 0, POST_TOUCH_EFF),
         CONTESTED_SHOTS_2PT = CONTESTED_SHOTS_2PT/POSS*100,
         CONTESTED_SHOTS_3PT = CONTESTED_SHOTS_3PT/POSS*100,
         DEFLECTIONS = DEFLECTIONS/POSS*100,
         SCREEN_ASSISTS = SCREEN_ASSISTS/POSS*100,
         AST_TOV = AST/TOV) %>%
  select(-DRIVE_FTA, -POST_TOUCH_FTA, -Season.breakout)

players22 <- players22 %>%
  add_predictions(model = fit, var = 'phat_breakout', type = 'response')

by_team <- players22 %>%
  mutate(Potential = ifelse(phat_breakout >= 0.05,1,0)) %>%
  group_by(TEAM) %>%
  summarize(n = n(),
            Exp_Breakout = sum(phat_breakout),
            Top = max(phat_breakout),
            Good = sum(Potential)) %>%
  ungroup() %>%
  arrange(-Exp_Breakout)




#ANALYZING SINGLE VARIABLES-----------------------------------------------------
draft_pick <- stats %>%
  mutate(Group = cut(numberPickOverall, breaks = seq(0,45,5))) %>%
  group_by(Group) %>%
  summarize(n = n(),
            breakout = mean(Breakout)) %>%
  ungroup()

dp_graph <- draft_pick %>%
  filter(!is.na(Group)) %>%
  ggplot(aes(x = Group, y = breakout)) +
  geom_col(aes(fill = breakout), width = 0.8, color = 'black', show.legend = F) +
  geom_smooth(level = 0) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("1-5","6-10","10-15","16-20","21-25","26-30","31-35","36-40","41-45")) +
  labs(x = "Draft Pick", y = "Breakout Rate",
       title = "Higher Draft Picks are More Likely to Break Out",
       size = "# Observations",
       subtitle = "Breakout = First-time All-star | 2018-2022",
       caption = "Graph by Ayush Batra") +
  theme_bbs()

tov_graph <- stats %>%
  mutate(Group = cut(TOV, breaks = seq(0,7.5,0.5))) %>%
  group_by(Group) %>%
  summarize(avgTOV = mean(TOV),
            n = n(),
            breakout = mean(Breakout)) %>%
  ungroup() %>%
  ggplot(aes(x = avgTOV, y = breakout)) +
  geom_point(aes(size = n)) +
  geom_smooth(level = 0) +
  labs(x = "Average Turnovers per 100 Possessions",
       y = "Breakout Rate",
       size = "# Observations",
       title = "Turnovers: A Good Sign?",
       subtitle = "Breakout = First-time All-star | 2018-2022",
       caption = "Graph by Ayush Batra") +
  scale_y_continuous(labels = scales::percent) +
  theme_bbs() +
  theme(panel.background = element_rect(fill = "#E4DBD7"))

pull_post_graph <- stats %>%
  filter(MIN >= 1000 | Breakout == 1) %>%
  mutate(Color = ifelse(Breakout == 1, 'green', 'red')) %>%
  ggplot(aes(x = PULL_UP_EFF, y = POST_TOUCH_EFF)) +
  geom_point(aes(fill = Color), shape = 21, color = 'black', size = 3, alpha = 0.7) +
  scale_fill_identity() +
  labs(x = "Pull-Up Efficiency (Pts/Shot)",
       y = "Post Touch Efficiency (Pts/Shot)",
       fill = "Breakout",
       title = "Breakout Players can Create their own Shot",
       subtitle = "Breakout = First-time All-star | 2018-2022",
       caption = "Graph by Ayush Batra") +
  theme_bbs() 

ts_tov_plot <- stats %>%
  filter(TOV >= 2.5) %>%
  ggplot(aes(x = TOV, y = TS_PCT)) +
  geom_hline(aes(yintercept = mean(stats$TS_PCT)), linetype = 'dashed') +
  geom_point(aes(fill = as.factor(Breakout)), shape = 21, size = 4, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = c("No","Yes")) +
  labs(x = "Turnovers per 100 Possessions",
       y = "True Shooting %",
       fill = "Breakout?",
       title = "Shooting Efficiency Matters",
       subtitles = "Includes Players with at least 2.5 TOV/100 from 2018-2022",
       caption = "Graph by Ayush Batra") +
  theme_bbs() +
  theme(legend.position = "top")



#RESULTS OF MLR-----------------------------------------------------------------
predictions <- stats %>%
  mutate(PLAYER_ID = as.numeric(PLAYER_ID)) %>%
  left_join(df_dict_nba_players, by = c("PLAYER_ID" = "idPlayer")) %>%
  filter(phat_breakout > 0.2) %>%
  mutate(Season = Season + 1) %>%
  ggplot(aes(y = Season, x = phat_breakout)) +
  geom_image(aes(image = urlPlayerHeadshot), size = 0.12) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Predicted Chance of First-Time All-Star",
       y = "Season",
       title = "Predicted Breakouts",
       subtitle = "2018-2022",
       caption = "Graph by Ayush Batra") +
  theme_bbs()

df_dict_nba_teams <- df_dict_nba_teams %>%
  filter(idTeam >= 1610612737)

thisSeason <- players22 %>%
  mutate(PLAYER_ID = as.numeric(PLAYER_ID)) %>%
  left_join(df_dict_nba_teams, by = c("TEAM" = "slugTeam")) %>%
  mutate(Color1 = str_sub(colorsTeam, 1, 7),
         Color2 = str_sub(colorsTeam, 10, 16)) %>%
  filter(phat_breakout > 0.1) %>%
  ggplot(aes(y = reorder(PLAYER_NAME, phat_breakout), x = phat_breakout)) +
  geom_col(aes(fill = Color1, color = Color2), width = 0.8) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Chance of Being First-Time All-Star in 2023",
       y = "Player",
       title = "Predicted Breakouts in 2022-23",
       subtitle = "Based on stats from 2021-22 Season",
       caption = "Graph by Ayush Batra") +
  theme_bbs()



#ggsave(dp_graph, filename = "Projects/2022/Q4 Oct-Dec/Breakout Players/dp_graph.png", height = 7, width = 7)
#ggsave(tov_graph, filename = "Projects/2022/Q4 Oct-Dec/Breakout Players/tov_graph.png", height = 7, width = 8)
#ggsave(pull_post_graph, filename = "Projects/2022/Q4 Oct-Dec/Breakout Players/pull_post_graph.png", height = 7, width = 7)
#ggsave(ts_tov_plot, filename = "Projects/2022/Q4 Oct-Dec/Breakout Players/ts_tov_plot.png", height = 8, width = 7)
#ggsave(predictions, filename = "Projects/2022/Q4 Oct-Dec/Breakout Players/predictions.png", height = 7, width = 7)
#ggsave(thisSeason, filename = "Projects/2022/Q4 Oct-Dec/Breakout Players/thisSeason.png", height = 7, width = 7)




