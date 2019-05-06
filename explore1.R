
library(tidyverse)
library(skimr)
library(ggthemes)
library(ggcorrplot)

theme_set(theme_minimal())

df <- read_csv("salg_i_ovrigt.csv") %>%
  mutate(SenesteHandelspris = as.numeric(SenesteHandelspris),
         VaegtetPris = as.numeric(VaegtetPris),
         EjendomVaerdi = as.numeric(EjendomVaerdi))


# antal handler for "salg i øvrigt"
nrow(df)

# add prisA,B,C

df_pris <- read_csv("s2.csv")

names(df_pris) <- c("id","Kvhx","prisA","prisB","prisC")

df_pris$Kvhx = as.character(df_pris$Kvhx)

df_pris <- df_pris %>%
  mutate(prisB = as.numeric(prisB),
         prisC = as.numeric(prisC))


df <- df %>% left_join(df_pris)

################################################
# full data set

# four houses were sold for exactly 3.417.918.991 DKK (??)
df %>% select(SenesteHandelspris) %>% drop_na() %>% arrange(desc(SenesteHandelspris))

df %>%
  ggplot(aes(x= SenesteHandelspris, y = VaegtetPris)) +
  geom_point()

df %>%
  select(SenesteHandelspris,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

################################################
# SenesteHandelspris < 3417918991

df2 <- df %>% filter(SenesteHandelspris < 3417918991)

df2 %>%
  ggplot(aes(x= SenesteHandelspris, y = VaegtetPris)) +
  geom_point()

df2 %>%
  select(SenesteHandelspris,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

################################################
# SenesteHandelspris < 10000000

df3 <- df %>% filter(SenesteHandelspris < 10000000)

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = VaegtetPris,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal()

df3 %>%
  select(SenesteHandelspris,VaegtetPris) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

df3  %>% 
  select(SenesteHandelspris,VaegtetPris, OffentligVurderingVedSenesteHandel,EjendomVaerdi,prisA,prisB,prisC) %>% 
  drop_na() %>%
  cor() %>%
  ggcorrplot(type   = "lower") +
  ggtitle("Correlations")

################################################
# PRIS A, B, C

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = prisA,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1) + # reference, slope = 1, perfect match
  geom_abline(intercept = 0, slope = .5) + # y = x / 2
  geom_abline(intercept = 0, slope = 2) # y = x * 2


df3 %>%
  select(SenesteHandelspris,prisA) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = prisB,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal()


df3 %>%
  select(SenesteHandelspris,prisB) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = prisC,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal()

df3 %>%
  ggplot(aes(x= SenesteHandelspris, y = prisC,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1) + # reference, slope = 1, perfect match
  geom_abline(intercept = 0, slope = 2) # y = x * 2

df3 %>%
  select(SenesteHandelspris,prisC) %>% 
  drop_na() %>% as.matrix() %>% cor() %>% .[2]

################################################
# Norgesvej 14B Lyngby

df %>% 
  filter(Kvhx == "1730611014B") %>%
  select(VaegtetPris,SenesteHandelspris,prisA,prisB,prisC,MaanederSidenSenesteHandel)



