
library(ggplot2)

# count of each basic pitch type
ggplot(pitches_clean, aes(x = pitch_type_b)) + 
  geom_bar()

# Pitch Type vs B-S Count -------------------------------------------------

# number of pitches for combination of balls, strikes
count_table <- table(pitches_clean$b_count, pitches_clean$s_count)
count_prop <- prop.table(count_table)

rowSums(count_prop)
colSums(count_prop)

# creating column for ball-strike count
pitches_clean$bs_count <- paste(pitches_clean$b_count, pitches_clean$s_count, sep = "-")
head(pitches_clean["bs_count"],7)

# number of each pitch type for each ball-strike count
type_count_table <- table(pitches_clean$pitch_type_b, pitches_clean$bs_count)
type_count_prop <- prop.table(type_count_table)

library(broom) # tidy()
typeVcount <- type_count_table %>% 
  tidy() %>% 
  select(pitch_type_b = Var1, bs_count = Var2, count = n) %>% 
  spread(bs_count, count)

typeVcount_colSums <- colSums(typeVcount[,-1])

for (i in 1:(ncol(typeVcount) - 1)) {
  typeVcount[,i + 1] <- typeVcount[,i + 1]/typeVcount_colSums[i]
}

typeVcount_long <- typeVcount %>% 
  gather(bs_count, prop, -pitch_type_b)

ggplot(subset(typeVcount_long, pitch_type_b %in% c("BB","CU","FB")), 
       aes(x = bs_count, y = prop, fill = pitch_type_b)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab("Proportion") +
  xlab("Ball-Strike Count") +
  scale_x_discrete(limits = c("0-0","0-1","1-0","0-2","1-1","2-0","1-2","2-1","3-0","2-2","3-1")) +
  scale_fill_discrete(name = "Pitch Type", labels = c("Breaking Balls", "Changeups", "Fastballs")) +
  ggtitle("Pitch Type Used through an At Bat")



# Pitch Type vs Pitch Velocity --------------------------------------------

# histogram matrix - start_speed for each basic pitch type
speed_means <- pitches_clean %>% 
                subset(pitch_type_b %in% c("FB","BB","CU")) %>% 
                group_by(pitch_type_b) %>% 
                summarise(Mean = mean(start_speed),
                          SD = sd(start_speed))

ggplot(subset(pitches_clean, pitch_type_b %in% c("FB","BB","CU")), aes(x = start_speed, fill = pitch_type_b)) +
  geom_histogram(binwidth = 1, color = "grey30") +
  geom_vline(data = speed_means, aes(xintercept = Mean), linetype = "dashed") +
  facet_grid(~ pitch_type_b) +
  xlim(60,105) +
  ylab("Frequency") +
  xlab("Pitch Speed (mph)") +
  ggtitle("Pitch Velocity by Pitch Type") +
  scale_fill_discrete(name = "Pitch Type", labels = c("Breaking Balls", "Changeups", "Fastballs")) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank())

# Fun Stats ---------------------------------------------------------------

# highest attendance
games_clean %>% 
  filter(attendance == max(attendance)) %>% 
  select(date, attendance, away_team, home_team, away_final_score, home_final_score)

# longest game
atbats_clean %>% 
  filter(inning == max(inning)) %>% 
  select(g_id) %>% 
  unique() %>% 
  inner_join(games_clean, by = "g_id") %>% 
  select(date, elapsed_time, away_team, home_team, away_final_score, home_final_score) %>% 
  arrange(desc(elapsed_time))

# most runs scored in game
atbats_clean %>% 
  filter(p_score == max(p_score)) %>% 
  select(g_id) %>% 
  unique() %>% 
  inner_join(games_clean, by = "g_id") %>% 
  select(date, away_team, home_team, away_final_score, home_final_score)

# fastest pitch
pitches_clean %>% 
  filter(start_speed == max(start_speed)) %>% 
  inner_join(atbats_clean, by = "ab_id") %>% 
  select(inning, outs, bs_count, start_speed, pitch_type, type, batter_id, pitcher_id) %>% 
  inner_join(player_names_clean, by = c("pitcher_id" = "player_id")) %>% 
  inner_join(player_names_clean, by = c("batter_id" = "player_id"), suffix = c("_pitcher", "_batter")) %>% 
  select(-batter_id, -pitcher_id)

