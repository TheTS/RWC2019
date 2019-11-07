library(tidyverse)
library(ggthemes)

# Prepare data ------------------------------------------------------------

# Read in the game predictions
preds <- read_csv('pool_game_predictions.csv') %>% 
  mutate(game = paste0(pool, ' ', game)) %>% 
  select(-pool) %>% 
  rbind(read_csv('knockout_stage_predictions.csv')) %>% 
  mutate(game = paste0(team_1, '_', team_2),
         order = c(1,6,9,14,18,22,26,31,36,40,4,5,11,15,20,23,27,29,34,37,3,7,12,
                   13,19,25,28,30,35,38,2,8,10,16,17,21,24,32,33,39,41:48)) %>% 
  select(-team_1, -team_2)

# Read in Ed's expert picks
ed <- read_csv('data/betting_sim/ed_preds.csv') %>% 
  left_join(read_rds('data/rwc_2019_teams.rds'), by = c('ed_pred' = 'name')) %>% 
  rename(eds_pred = abr) %>% 
  select(-ed_pred)


# Read in all saved TAB odds, join country abbreviations, only keep 
# the oldest odd for each game (as they can change each day), and
# join the predictions and actual results
df <- map_df(list.files('data/tab_odds', full.names = T), read_csv, col_types = cols()) %>% 
  left_join(read_rds('data/rwc_2019_teams.rds'), by = c('team1_name' = 'name')) %>% 
  left_join(read_rds('data/rwc_2019_teams.rds'), by = c('team2_name' = 'name')) %>% 
  rename(team1_abr = abr.x, 
         team2_abr = abr.y) %>% 
  mutate_at(vars(ends_with('_abr')), ~ifelse(is.na(.), 'USA', as.character(.))) %>% 
  mutate(game = paste0(team1_abr, '_', team2_abr)) %>% 
  group_by(game) %>%
  top_n(-1, date) %>% 
  ungroup() %>% 
  left_join(preds, by = 'game') %>% 
  left_join(read_csv('data/betting_sim/actual_results.csv'), by = 'game') %>%
  left_join(ed, by = 'order') %>% 
  arrange(order) %>% 
  select(game, date, team1_name, team2_name, team1_abr,team2_abr, everything())

write_csv(df, 'data/betting_sim/rwc_preds_and_results.csv')
rm(preds, ed)

# Betting sim! ------------------------------------------------------------

betting_sim <- function(bet_amount) {
  df %>% 
    mutate(winner_p1 = ifelse(team1_m1 > 0.5, team1_abr, team2_abr),
           winner_p2 = ifelse(team1_m2 > 0.5, team1_abr, team2_abr)) %>%
    
    mutate(correct_m1 = winner_p1 == winner,
           correct_m2 = winner_p2 == winner,
           correct_m3 = eds_pred == winner) %>%
    
    mutate(winner_odds1 = ifelse(substr(game, 1, 3) == winner_p1, team1_odds, team2_odds),
           winner_odds2 = ifelse(substr(game, 1, 3) == winner_p2, team1_odds, team2_odds),
           winner_odds3 = ifelse(substr(game, 1, 3) == eds_pred, team1_odds, team2_odds)) %>% 
    
    mutate(return_m1 = ifelse(correct_m1, (bet_amount * winner_odds1)-bet_amount, -bet_amount),
           return_m2 = ifelse(correct_m2, (bet_amount * winner_odds2)-bet_amount, -bet_amount),
           return_m3 = ifelse(correct_m3, (bet_amount * winner_odds3)-bet_amount, -bet_amount)) %>% 
    
    select(game, date, winner, return_m1, return_m2, return_m3, order) %>%
    
    mutate(bet_amount = bet_amount,
           running_m1 = cumsum(coalesce(return_m1, 0)),
           running_m2 = cumsum(coalesce(return_m2, 0)),
           running_m3 = cumsum(coalesce(return_m3, 0)))
}



# Graph to see how models stack up ----------------------------------------

x <- betting_sim(5) %>% 
  pivot_longer(c(ends_with('1'), ends_with('2'), ends_with('3'))) %>% 
  separate(name, into = c('var', 'model')) %>% 
  pivot_wider(names_from = 'var', values_from = 'value') %>% 
  mutate(model = ifelse(model=='m1', 'Model 1', ifelse(model=='m2', 'Model 2', "Edward Ashworth's Expert Predictions")),
         game = gsub('_', ' v ', game)) %>% 
  mutate(model = factor(model, levels = c('Model 1', 'Model 2', "Edward Ashworth's Expert Predictions")))
  
ggplot(x, aes(x = reorder(game, rev(order)))) +
  geom_hline(yintercept = 0, color = 'grey20') +
  geom_col(aes(y = return, fill = (return < 0))) +
  geom_line(aes(y = running, group = 1), size = 1, color = 'grey10') +
  geom_point(aes(y = running), color = 'grey10', size = 1.5) +
  geom_label(data = filter(x, game == 'ENG v RSA'), aes(y = running, label = paste0('$' ,round(running, 2)), 
                                                        color=(running > 0)), nudge_x = 0.8, nudge_y = 1, size = 5) +
  scale_y_continuous(limits = c(-18, 21)) +
  scale_fill_fivethirtyeight() +
  facet_wrap(~model) +
  coord_flip() +
  theme_clean() +
  theme(legend.position = 'none', axis.title.y = element_blank()) +
  labs(x = 'game', y = 'Profit ($)', title = 'How much money would you have made?', 
  subtitle = 'Based on the NZ TAB odds and putting $5 on the predicted winner of each game',
  caption = 'The line represents the cumulative profit')

ggsave('graphs/betting_preds.png', width = 17, height = 12, dpi = 800)









