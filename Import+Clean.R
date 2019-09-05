

# Load data ---------------------------------------------------------------

library(readr)

atbats <- read_csv("data/atbats.csv")
games <- read_csv("data/games.csv")
pitches <- read_csv("data/pitches.csv")
player_names <- read_csv("data/player_names.csv")

# Data Cleaning - atbats ------------------------------------------------------------
library(magrittr) # pipe
library(purrr) # map functions
library(tidyr) # gather
library(dplyr) # filter, select, arrange, inner_join

str(atbats)
summary(atbats)

sum(is.na(atbats))

atbats_clean <- atbats

atbats_clean$ab_id <- as.character(atbats_clean$ab_id)
atbats_clean$batter_id <- as.character(atbats_clean$batter_id)
atbats_clean$g_id <- as.character(atbats_clean$g_id)
atbats_clean$pitcher_id <- as.character(atbats_clean$pitcher_id)
atbats_clean$inning <- as.integer(atbats_clean$inning)
atbats_clean$o <- as.integer(atbats_clean$o)
atbats_clean$p_score <- as.integer(atbats_clean$p_score)

str(atbats_clean)

unique(atbats_clean$event)
unique(atbats_clean$p_throws)
unique(atbats_clean$stand)


# Data Cleaning - games -------------------------------------------------------------

str(games)
sum(is.na(games))

games %>% 
map_df(~sum(is.na(.))) %>% 
  gather(variable, num_missing) %>% 
  filter(num_missing > 0)

games_clean <- games
games_clean$g_id <- as.character(games_clean$g_id)
games_clean$attendance <- as.integer(games_clean$attendance)
games_clean$away_final_score <- as.integer(games_clean$away_final_score)
games_clean$home_final_score <- as.integer(games_clean$home_final_score)

str(games_clean)

summary(games_clean)

unique(games_clean$away_team)
unique(games_clean$home_team)
unique(games_clean$venue_name)


# Data Cleaning - pitches -------------------------------------------------

str(pitches)
sum(is.na(pitches))
colSums(is.na(pitches))

pitches %>% 
  select(ab_id, b_count, end_speed, outs, on_1b, on_2b, on_3b, pitch_type, s_count, start_speed, type) %>% 
  map_df(~sum(is.na(.))) %>% 
  gather(variable, num_missing) %>% 
  filter(num_missing > 0)

pitches_clean <- pitches %>% 
  select(ab_id, b_count, end_speed, outs, on_1b, on_2b, on_3b, pitch_type, s_count, start_speed, type) %>% 
  na.omit()
sum(is.na(pitches_clean))

pitches_clean$ab_id <- as.character(pitches_clean$ab_id)
pitches_clean$b_count <- as.integer(pitches_clean$b_count)
pitches_clean$outs <- as.integer(pitches_clean$outs)
pitches_clean$s_count <- as.integer(pitches_clean$s_count)

str(pitches_clean)

summary(pitches_clean)

unique(pitches_clean$type)

pitches_clean <- filter(pitches_clean, b_count < 4)

unique(pitches_clean$pitch_type)


pitch_type_table <- table(pitches_clean$pitch_type)
pitch_type_table %>% 
  prop.table() %>% 
  round(3) %>% 
  as_data_frame() %>% 
  rename(pitch = Var1, proportion = n) %>% 
  arrange(desc(proportion))

# simplified pitch types - less than 0.1% and intentional balls into Other (OT) category
pitches_clean$pitch_type_s <- pitches_clean$pitch_type
pitch_remove <- c("AB","EP","FA","FO","PO","SC","UN","IN")
pitches_clean$pitch_type_s[pitches_clean$pitch_type_s %in% pitch_remove] <- "OT"

pitch_type_s_table <- table(pitches_clean$pitch_type_s)
pitch_type_s_table %>% 
  prop.table() %>% 
  round(3) %>% 
  as_data_frame() %>% 
  rename(pitch = Var1, proportion = n) %>% 
  arrange(desc(proportion))


# basic pitch types - breaking down into FB, BB, CU, and OT
fastballs <- c("FC","FF","FS","FT","SI")
breakingballs <- c("CU","KC","SC","SL")
changeups <- c("CH","KN","EP")
other_pitches <- c("FO","PO","IN","UN","AB","FA")

pitches_clean$pitch_type_b <- pitches_clean$pitch_type
pitches_clean$pitch_type_b[pitches_clean$pitch_type %in% fastballs] <- "FB"
pitches_clean$pitch_type_b[pitches_clean$pitch_type %in% breakingballs] <- "BB"
pitches_clean$pitch_type_b[pitches_clean$pitch_type %in% changeups] <- "CU"
pitches_clean$pitch_type_b[pitches_clean$pitch_type %in% other_pitches] <- "OT"


pitch_type_b_table <- table(pitches_clean$pitch_type_b)
pitch_type_b_table %>% 
  prop.table() %>% 
  round(3) %>% 
  as_data_frame() %>% 
  rename(pitch = Var1, proportion = n) %>% 
  arrange(desc(proportion)) %>% 
  ggplot(aes(x = pitch, y = proportion, fill = pitch)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = proportion), vjust = -0.25) +
  ggtitle("Pitch Type Proportions")


# Data Cleaning - player_names --------------------------------------------

str(player_names)
sum(is.na(player_names))
player_names_clean <- player_names
names(player_names_clean)[1] <- "player_id"
player_names_clean$player_id <- as.character(player_names_clean$player_id)
str(player_names_clean)
