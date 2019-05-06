# exploring AVM performance for all "overdragelsesmåder" for houses sold during last 12 months

library(tidyverse)
library(ggthemes)
library(ggcorrplot)

theme_set(theme_minimal())

df <- read_csv("avm_transaction_types_1year.csv") %>%
  mutate(SenesteHandelspris = as.numeric(SenesteHandelspris),
         SenesteUdbudspris  = as.numeric(SenesteUdbudspris),
         VaegtetPris    = as.numeric(VaegtetPris),
         VaegtetPris    = as.numeric(VaegtetPris),
         PrisB              = as.numeric(PrisB),
         PrisC              = as.numeric(PrisC))


################################################
# SenesteHandelspris < 10000000

df <- df %>% filter(SenesteHandelspris < 10000000)

types <- levels(factor(df$Overdragelsesmaade))


# vægtet pris
df %>% 
  ggplot(aes(x= SenesteHandelspris, y = VaegtetPris) ) +
  geom_point(alpha=.2,size=2) +
  coord_equal() +
  facet_wrap(~Overdragelsesmaade)

for (type in types) {
  
  corr <- df %>% 
    filter(Overdragelsesmaade == type) %>%
    select(SenesteHandelspris,VaegtetPris) %>% 
    drop_na() %>% 
    as.matrix() %>% 
    cor() %>% .[2]
  
  print(paste0(type,": ", corr))
  
}

# A
df %>% 
  ggplot(aes(x= SenesteHandelspris, y = PrisA) ) +
  geom_point(alpha=.2,size=2) +
  coord_equal() +
  facet_wrap(~Overdragelsesmaade)

for (type in types) {
  
  corr <- df %>% 
    filter(Overdragelsesmaade == type) %>%
    select(SenesteHandelspris,PrisA) %>% 
    drop_na() %>% 
    as.matrix() %>% 
    cor() %>% .[2]
  
  print(paste0(type,": ", corr))
  
}

# B
df %>% 
  ggplot(aes(x= SenesteHandelspris, y = PrisB) ) +
  geom_point(alpha=.2,size=2) +
  coord_equal() +
  facet_wrap(~Overdragelsesmaade)

for (type in types) {
  
  corr <- df %>% 
    filter(Overdragelsesmaade == type) %>%
    select(SenesteHandelspris,PrisB) %>% 
    drop_na() %>% 
    as.matrix() %>% 
    cor() %>% .[2]
  
  print(paste0(type,": ", corr))
  
}
# C
df %>% 
  ggplot(aes(x= SenesteHandelspris, y = PrisB) ) +
  geom_point(alpha=.2,size=2) +
  coord_equal() +
  facet_wrap(~Overdragelsesmaade)

for (type in types) {
  
  corr <- df %>% 
    filter(Overdragelsesmaade == type) %>%
    select(SenesteHandelspris,PrisB) %>% 
    drop_na() %>% 
    as.matrix() %>% 
    cor() %>% .[2]
  
  print(paste0(type,": ", corr))
  
}

df3 <- df %>% filter(Overdragelsesmaade == 4)

df3  %>% 
  select(SenesteHandelspris,VaegtetPris,PrisA,PrisB,PrisC) %>% 
  drop_na() %>%
  cor() %>%
  ggcorrplot(type   = "lower") +
  ggtitle("Correlations")

################################################
# PRIS A, B, C

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = PrisA,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1) + # reference, slope = 1, perfect match
  geom_abline(intercept = 0, slope = .5) + # y = x / 2
  geom_abline(intercept = 0, slope = 2) # y = x * 2


df3 %>%
  select(SenesteHandelspris,PrisA) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = PrisB,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal()


df3 %>%
  select(SenesteHandelspris,PrisB) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = PrisC,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal()

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = PrisC,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1) + # reference, slope = 1, perfect match
  geom_abline(intercept = 0, slope = 2) # y = x * 2

df3 %>%
  select(SenesteHandelspris,PrisC) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

################################################
# Norgesvej 14B Lyngby

df %>% 
  filter(Kvhx == "1730611014B") %>%
  select(VaegtetPris,SenesteHandelspris,PrisA,PrisB,PrisAB,PrisC,MaanederSidenSenesteHandel)

df


