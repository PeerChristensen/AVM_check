
library(tidyverse)
library(ggthemes)

df <- read_csv("subsetted_df.csv") %>%
  filter(Kommune == 173)

df <- df %>% mutate(target = ifelse(Kvhx == "1730611014B","target","other"))

df <- df %>% 
  mutate(OffentligVurderingVedSenesteHandel = as.numeric(OffentligVurderingVedSenesteHandel),
         VaegtetPris = as.numeric(VaegtetPris),
         SenesteHandelspris = as.numeric(SenesteHandelspris))

df %>%
  filter(Overdragelsesmaade == 1) %>%
  ggplot(aes(y = VaegtetPris, x = OffentligVurderingVedSenesteHandel, colour = target)) +
  geom_point(size = 2, alpha = .6) +
  scale_colour_tableau() + 
  theme_light() +
  ggtitle("Overdragelsesmåde 1")


df %>%
  filter(Overdragelsesmaade == 2) %>%
  ggplot(aes(y = VaegtetPris, x = OffentligVurderingVedSenesteHandel, colour = target)) +
  geom_point(size = 2, alpha = .6) +
  scale_colour_tableau() + 
  theme_light() +
  ggtitle("Overdragelsesmåde 2")

df %>%
  filter(Overdragelsesmaade == 3) %>%
  ggplot(aes(y = VaegtetPris, x = OffentligVurderingVedSenesteHandel, colour = target)) +
  geom_point(size = 2, alpha = .6) +
  scale_colour_tableau() + 
  theme_light() +
  ggtitle("Overdragelsesmåde 3")

df %>%
  filter(Overdragelsesmaade == 4) %>%
  ggplot(aes(y = VaegtetPris, x = OffentligVurderingVedSenesteHandel, colour = target)) +
  geom_point(size = 2, alpha = .6) +
  scale_colour_tableau() + 
  theme_light() +
  ggtitle("Overdragelsesmåde 4")


df %>%
  filter(!is.numeric(Overdragelsesmaade)) %>%
  ggplot(aes(y = VaegtetPris, x = OffentligVurderingVedSenesteHandel, colour = target)) +
  geom_point(size = 2, alpha = .6) +
  scale_colour_tableau() + 
  theme_light() +
  ggtitle("Overdragelsesmåde NULL")

# correlations

df %>% filter(Overdragelsesmaade == 1) %>% 
  select(OffentligVurderingVedSenesteHandel,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor()

df %>% filter(Overdragelsesmaade == 2) %>% 
  select(OffentligVurderingVedSenesteHandel,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor()

df %>% filter(Overdragelsesmaade == 3) %>% 
  select(OffentligVurderingVedSenesteHandel,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor()

df %>% filter(Overdragelsesmaade == 4) %>% 
  select(OffentligVurderingVedSenesteHandel,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor()

df %>% filter(!is.numeric(Overdragelsesmaade)) %>% 
  select(OffentligVurderingVedSenesteHandel,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor()


              