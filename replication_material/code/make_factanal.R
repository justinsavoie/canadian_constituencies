library(tidyverse)
library(RColorBrewer)

df <- read_csv("~/Dropbox (Vox Pop Labs)/Data/VoteCompass/Canada2015/Canada2015Clean.csv")

df_sub <- df %>%
  select(riding,province,voteChoice,selfPlacement,paste0("q",1:30)) %>%
  filter(!is.na(riding), !is.na(province), !is.na(selfPlacement)) %>%
  filter(voteChoice %in% c("NDP","CON","LIB","GRN","BQ"))

df_sub[is.na(df_sub)] <- 3

model <- factanal(df_sub[,paste0("q",1:30)],1, scores = "regression")

df_sub$score <- c(model$scores)

to_plot <- df_sub %>% 
  group_by(riding,province) %>%
  summarise(`Mean score 1D` = mean(score),
            `Mean self placement` = mean(selfPlacement)) %>%
  ungroup() %>%
  mutate(`Mean score 1D` = scales::rescale(`Mean score 1D`, to = c(0,1)))

to_plot$`Province` <- c("British Columbia"="British Columbia",
                        "Quebec"="Quebec",
                        "New Brunswick"="Atlantic",
                        "Ontario"="Ontario",
                        "Newfoundland and Labrador"="Atlantic", 
                        "Alberta"="Alberta",
                        "Saskatchewan"="Manitoba-Saskatchewan",
                        "Manitoba"="Manitoba-Saskatchewan",
                        "Nova Scotia"="Atlantic",
                        "Prince Edward Island"="Atlantic", 
                        "Northwest Territories"="Territories",
                        "Nunavut"="Territories",
                        "Yukon"="Territories")[to_plot$province]

saveRDS(to_plot,"~/Dropbox (Personal)/UofT/canadian/essay2/replication_material/data/to_plot.rds")
to_plot <- readRDS("~/Dropbox (Personal)/UofT/canadian/essay2/replication_material/data/to_plot.rds")