library(tidyverse)


# Getting data ready for plotting -----------------------------------------
df.plot <- read_csv('pool_game_predictions.csv') %>%
  rename(team1_n = team_1,
         team2_n = team_2) %>% 
  mutate(team2_m1 = 1-team1_m1,
         team2_m2 = 1-team1_m2) %>%
  mutate_all(as.character) %>%
  pivot_longer(cols = starts_with('team')) %>%
  separate(name, into = c('team', 'variable')) %>%
  pivot_wider(names_from = 'variable', values_from = 'value') %>%
  mutate(m1 = as.numeric(m1),
         m2 = as.numeric(m2),
         win1 = m1 >= 0.5,
         win2 = m2 >= 0.5,
         team = factor(team),
         pool = factor(pool, levels = LETTERS[1:4]),
         game = factor(game, levels = 1:10)) %>% 
  pivot_longer(5:8) %>% 
  mutate(name = gsub('([a-z])([1-2])', '\\1_\\2', name)) %>% 
  separate(name, into = c('var', 'number')) %>% 
  pivot_wider(names_from = 'var', values_from = 'value') %>% 
  mutate(n = factor(n),
         number = factor(number, levels = c('2', '1'), labels = c('m2', 'm1')),
         win = factor(win))
  
  
# One plot with all predictions -------------------------------------------
ggplot(df.plot, aes(x=number, y = m, fill = win)) +
  geom_col(position = position_stack()) +
  geom_hline(yintercept=0.5, color='black', linetype='dashed') +
  facet_grid(game~pool) +
  geom_text(aes(label = paste0(n, ' (', round(m,2), ')'), x = number,
                y = ifelse(win==1, 0.1, 0.9)), color = 'black', size=3) +
  scale_fill_manual(values = c(alpha('tomato', 0.9), alpha('skyblue', 0.9))) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none') +
  
  labs(y = 'Probability of winning',
       title = 'RWC 2019 pool game predictions',
       subtitle = 'Predictions obtained from two models `m1` and `m2`.',
       caption = "\n`m1` uses data scraped from lassen.co.nz (all test matches from 1987-2019)\n
                  `m2` uses data scraped from espnscrum.com (2003-2019) and historical world rankings (world.rugby/rankings)\n
                  All code and data can be found at github.com/TheTS/RWC2019") +
  coord_flip()


ggsave('graphs/pool_predictions.png', width=15, height=10)



