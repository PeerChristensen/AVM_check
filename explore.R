
library(ggridges)
library(tidyverse)
library(ggthemes)

df <- read_csv("avm_transtype_clean.csv")

df %>% 
  group_by(PROP_TRANSTYPE_NAME) %>%
  summarise(m =mean(difference_val,na.rm=T))

df %>%
  ggplot(aes(x=difference_val,y=PROP_TRANSTYPE_NAME, fill = PROP_TRANSTYPE_NAME)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_fill_tableau() +
  theme_ridges() +
  ggtitle("Untrimmed")

df %>% 
  filter(MUNI_NAME=="Lyngby-Taarbæk",str_detect(ACADR_NAME, "Norgesvej")) %>% 
  select(-UNADR_KEY,-PROP_AVMCONFIDENCE_CODE,-MUNI_NAME,-PROP_VALUASQM_AVM)

df_filter <- read_csv("avm_transtype_clean_filter.csv")

df_filter %>% 
  group_by(PROP_TRANSTYPE_NAME) %>%
  summarise(m =mean(difference_val))

df_filter %>%
  ggplot(aes(x=difference_val,y=PROP_TRANSTYPE_NAME, fill = PROP_TRANSTYPE_NAME)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_fill_tableau() +
  theme_ridges() +
  ggtitle("Trimmed")

