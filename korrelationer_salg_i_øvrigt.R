
library(tidyverse)
library(ggthemes)
library(ggcorrplot)
library(ggthemes)
library(gridExtra)

theme_set(theme_minimal())


df <- read_csv("with_off_vurdering.csv") %>%
  mutate(SenesteHandelspris = as.numeric(SenesteHandelspris),
         SenesteUdbudspris  = as.numeric(SenesteUdbudspris),
         VaegtetPris        = as.numeric(VaegtetPris),
         VaegtetPris        = as.numeric(VaegtetPris),
         PrisB              = as.numeric(PrisB),
         PrisC              = as.numeric(PrisC),
         PrisAB             = (PrisA + PrisB) / 2)

df <- df %>% filter(SenesteHandelspris < 50000000)

# correlations
df  %>% 
  select(SenesteHandelspris,VaegtetPris,PrisA,PrisB,PrisAB,PrisC,OffentligVurderingVedSenesteHandel) %>% 
  drop_na() %>%
  cor() %>%
  ggcorrplot(type   = "lower") +
  ggtitle("Correlations")

a <- df %>% 
  select(OffentligVurderingVedSenesteHandel,PrisA) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)

b <- df %>% 
  select(OffentligVurderingVedSenesteHandel,PrisB) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)

ab <- df %>% 
  select(OffentligVurderingVedSenesteHandel,PrisAB) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)

#Hvis jeg forstår rigtigt, så bliver den vægtede pris en dårligere predictor af handelspris når man ikke tager model C med (korr: .57 vs. .9), OffentligVurdering har korr = .63. Hvis du mener

p1 <- df %>%
  ggplot(aes(x=PrisA,y=OffentligVurderingVedSenesteHandel,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_fixed(xlim = c(0, 10000000)) +
  geom_abline(intercept = 0, slope = 1) +# reference, slope = 1, perfect match
  annotate("text",x=5000000,y=15000000,, label = paste("corr:", a)) +
  theme(legend.position = "none")

p2 <- df %>%
  ggplot(aes(x=PrisB,y=OffentligVurderingVedSenesteHandel,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  coord_fixed(xlim = c(0, 10000000)) +
  geom_abline(intercept = 0, slope = 1) +# reference, slope = 1, perfect match
  annotate("text",x=5000000,y=15000000,, label = paste("corr:", b)) +
  theme(legend.position = "none")


p3 <- df %>%
  ggplot(aes(x=PrisAB,y=OffentligVurderingVedSenesteHandel,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
 #coord_fixed(xlim = c(0, 10000000)) +
  geom_abline(intercept = 0, slope = 1) +# reference, slope = 1, perfect match
  annotate("text",x=5000000,y=15000000, label = paste("corr:", ab)) +
  theme(legend.position = "none") 

grid.arrange(p1,p2,p3,ncol=3)

df %>% 
  filter(Kvhx == "1730611014B") %>%
  select(VaegtetPris,SenesteHandelspris,PrisA,PrisB,PrisAB,PrisC,MaanederSidenSenesteHandel,OffentligVurderingVedSenesteHandel)

 ###
# Hvor meget bedre bliver Vaegtet Pris sammenlignet med
#OffentligVurdering hvis vi fjerner Modell C for Øvrige handler?

# 1 seneste handelspris ~ Vaegtet pris

avm <- df %>% 
  select(SenesteHandelspris,VaegtetPris) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)

# 2 seneste handelspris ~ Pris AB (uden model C)
ab <- df %>% 
  select(SenesteHandelspris,PrisAB) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)

# 3 seneste handelspris ~ offentlig vurdering
off <- df %>% 
  select(SenesteHandelspris,OffentligVurderingVedSenesteHandel) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)

#####
# reported

vp <- df %>% 
  select(OffentligVurderingVedSenesteHandel,VaegtetPris) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)


old <- df %>%
  ggplot(aes(x=VaegtetPris,y=OffentligVurderingVedSenesteHandel,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  geom_abline(intercept = 0, slope = 1) +# reference, slope = 1, perfect match
  annotate("text",x=5000000,y=15000000, label = paste("corr:", vp)) +
  scale_x_continuous(labels = function(n) {
    trans = n / 1000000
    paste0(trans, "M")
  },limits = c(0,20000000)) +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000000
    paste0(trans, "M")
  },limits = c(0,20000000)) +
  theme(legend.position = "none") +
  ggtitle("Weighted Price",
          subtitle = "The reference line indicates perfect accuracy")

ab <- df %>% 
  select(OffentligVurderingVedSenesteHandel,PrisAB) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% .[2] %>%
  round(3)

new <- df %>%
  ggplot(aes(x=PrisAB,y=OffentligVurderingVedSenesteHandel,colour = Kvhx == "1730611014B")) +
  geom_point(alpha=.4,size=2) +
  scale_colour_tableau()  +
  geom_abline(intercept = 0, slope = 1) +# reference, slope = 1, perfect match
  annotate("text",x=5000000,y=15000000, label = paste("corr:", ab)) +
  scale_x_continuous(labels = function(n) {
    trans = n / 1000000
    paste0(trans, "M")
  },limits = c(0,20000000)) +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000000
    paste0(trans, "M")
  },limits = c(0,20000000)) +
  theme(legend.position = "none") +
  ggtitle("Average of model A and B",
          subtitle = "The reference line indicates perfect accuracy")

grid.arrange(old,new,ncol=2)
