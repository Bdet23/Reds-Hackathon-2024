setwd("/users/iankleppinger/Desktop/Reds Hackathon")
pitchlevel <- read.csv("savant_pitch_level.csv")
seasonlevel <- read.csv("fangraphs_season_level.csv")
weighted_gmli <- read.csv("LImodded_fangraphs_season_level.csv")
data2023 <- read.csv("2023regression15%.csv")
library(dplyr)

dataallyears <- weighted_gmli %>% 
  mutate(IP_per_G = IP / G) %>%
  distinct() %>%
  mutate(Assigned_Role = case_when(
    Role == 'SP' ~ "Starter",
    IP_per_G <= 1 & gmLI <= 0.85 ~ "Short Low Leverage",
    IP_per_G > 1 & gmLI <= 0.85 ~ "Long Low Leverage",
    IP_per_G <= 1 & gmLI > 0.85 ~ "Short High Leverage",
    IP_per_G > 1 & gmLI > 0.85 ~ "Long High Leverage",
    TRUE ~ NA_character_
  )) %>% 
  filter(case_when(Assigned_Role == "Starter" ~ GS >= 5 & IP >= 10,
                   Assigned_Role != "Starter" ~ IP >= 10))

write.csv(datallyears, "roledata.csv", row.names = F)

filtered_data <- data2122 %>%
  group_by(PlayerId, Season) %>%
  filter(n() > 1) %>%
  ungroup()

baserunner_splits <- pitchlevel %>% 
  mutate(bases_empty = ifelse(is.na(on_1b) & is.na(on_2b) & is.na(on_3b), TRUE, FALSE)) %>%
  group_by(pitcher, game_year) %>% 
  summarise(woba_mean_empty = mean(woba_value[bases_empty], na.rm = TRUE),
            woba_mean_runners = mean(woba_value[!bases_empty], na.rm = TRUE)) %>% 
  arrange(pitcher, game_year)


write.csv(baserunner_splits, "baserunnersplits.csv", row.names = F)

regressiondata <- read.csv("regressiondata.csv")
starters <- regressiondata %>% 
  filter(Assigned_Role == "Starter")
longlowleverage <- regressiondata %>% 
  filter(Assigned_Role == "Long Low Leverage")
shortlowleverage <- regressiondata %>% 
  filter(Assigned_Role == "Short Low Leverage")
longhighleverage <- regressiondata %>% 
  filter(Assigned_Role == "Long High Leverage")
shorthighleverage <- regressiondata %>% 
  filter(Assigned_Role == "Short High Leverage")

starters_model <- lm(data = starters, formula = FIP_minus ~ Pitches_Per_Batter + 
              Stuff_plus + Location_plus + num_mix + 
              stamina + woba_value_L + woba_value_R + 
              woba_mean_empty + woba_mean_runners)
ll_model <- lm(data = longlowleverage, formula = FIP_minus ~ Pitches_Per_Batter + 
                       Stuff_plus + Location_plus + num_mix + 
                       stamina + woba_value_L + woba_value_R + 
                       woba_mean_empty + woba_mean_runners)
lh_model <- lm(data = longhighleverage, formula = FIP_minus ~ Pitches_Per_Batter + 
                       Stuff_plus + Location_plus + num_mix + 
                       stamina + woba_value_L + woba_value_R + 
                       woba_mean_empty + woba_mean_runners)
sl_model <- lm(data = shortlowleverage, formula = FIP_minus ~ Pitches_Per_Batter + 
                       Stuff_plus + Location_plus + num_mix + 
                       stamina + woba_value_L + woba_value_R + 
                       woba_mean_empty + woba_mean_runners)
sh_model <- lm(data = shorthighleverage, formula = FIP_minus ~ Pitches_Per_Batter + 
                       Stuff_plus + Location_plus + num_mix + 
                       stamina + woba_value_L + woba_value_R + 
                       woba_mean_empty + woba_mean_runners)
summary(starters_model)
summary(ll_model)
summary(lh_model)
summary(sl_model)
summary(sh_model)


predictions <- data2023 %>% 
  mutate(sFIP_minus = predict(starters_model, newdata = data2023),
         llFIP_minus = predict(ll_model, newdata = data2023),
         lhFIP_minus = predict(lh_model, newdata = data2023),
         slFIP_minus = predict(sl_model, newdata = data2023),
         shFIP_minus = predict(sh_model, newdata = data2023)) %>% 
  left_join(dataallyears, by = join_by(MLBAMID, Season, Assigned_Role)) %>% 
  filter(Assigned_Role == "Long High Leverage", 
         IP_per_G >= 1.2)
write.csv(predictions, "Ian Hamilton.csv", row.names = F)

analysis_df <- predictions %>% 
  mutate(role_proj = ifelse(Assigned_Role == 'Starter', sFIP_minus,
                            ifelse(Assigned_Role == 'Long Low Leverage', llFIP_minus,
                                  ifelse(Assigned_Role == 'Long High Leverage', lhFIP_minus,
                                          ifelse(Assigned_Role == 'Short Low Leverage', slFIP_minus, shFIP_minus)))),
         sDifference = role_proj - sFIP_minus,
         llDifference = role_proj - llFIP_minus,
         lhDifference = role_proj - lhFIP_minus,
         slDifference = role_proj - slFIP_minus,
         shDifference = role_proj - shFIP_minus) %>% 
  select(MLBAMID, IP_per_G, Season, sDifference, llDifference, lhDifference, slDifference, shDifference)

smean <- mean(analysis_df$sDifference)
llmean <- mean(analysis_df$llDifference)
lhmean <- mean(analysis_df$lhDifference)
slmean <- mean(analysis_df$slDifference)
shmean <- mean(analysis_df$shDifference)

analysis_plus <- analysis_df %>% 
  mutate(sDifference_plus = (sDifference*100/smean),
         llDifference_plus = (llDifference*100/smean),
         lhDifference_plus = (lhDifference*100/smean),
         slDifference_plus = (slDifference*100/smean),
         shDifference_plus = (shDifference*100/smean)
         ) %>% 
  select(MLBAMID, NameASCII, Season, sDifference_plus, llDifference_plus, lhDifference_plus, 
         slDifference_plus, shDifference_plus)
                              
write.csv(analysis_df, "role_differentials.csv", row.names = F)



weighted_gmli_2122rp <- seasonlevel %>%
  #group_by(PlayerId) %>%
  #summarize(GS = sum(GS)) %>%
  filter(Season %in% c(2021, 2022), Role == 'RP') %>%
  mutate(weighted_gmLI = IP*gmLI) %>%
  select(NameASCII, Season, gmLI, IP, weighted_gmLI)

newdf <- weighted_gmli_2122rp %>%
  filter(Season != 2023, Role == "RP") %>%
  group_by(PlayerId) %>%
  reframe(NameASCII = NameASCII, avggmLI = avggmLI, IP_per_G = sum(IP)/sum(G)) %>%
  distinct() #%>%
  #filter(IP_per_G >= 1)

quantile(newdf$avggmLI, probs = seq(0, 1, .25), na.rm = TRUE)

plot(newdf$IP_per_G, newdf$avggmLI, ylab = "leverage", xlab = "length of appearance")
abline(v=1, col = 'red')
abline(h = .85, col = 'blue')



newdf <- weighted_gmli_2122rp %>%
  filter(Season != 2023, Role == "RP") %>%
  group_by(PlayerId) %>%
  reframe(NameASCII = NameASCII, avggmLI = avggmLI, P_per_G = sum(Pitches)/sum(G)) %>%
  distinct() #%>%
#filter(IP_per_G >= 1)

quantile(newdf$P_per_G, probs = seq(0, 1, .25), na.rm = TRUE)
length(newdf$P_per_G)
length(newdf$avggmLI)
plot(newdf$IP_per_G, newdf$avggmLI, ylab = "leverage", xlab = "length of appearance")
abline(v = 1, col = 'blue', lwd = 2)
abline(h = c(.85), col = 'red', lwd = 2)

weighted_gmli %>% filter(NameASCII == "Andrew Saalfrank")  
# 
# 
# length(newdf$avggmLI[newdf$avggmLI <= .85]) +
# length(newdf$avggmLI[newdf$avggmLI > .85 & newdf$avggmLI <= 1]) +
# length(newdf$avggmLI[newdf$avggmLI > 1])
