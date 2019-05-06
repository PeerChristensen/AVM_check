
# clean data

library(lubridate)
library(tidyverse)

df <- read_csv("big_df.csv") %>%
  mutate(SenesteHandelsdato = as_date(SenesteHandelsdato))

df <- df %>% filter(SenesteHandelsdato > as_date("2018-05-01"))

df <- df %>% 
  mutate(SenesteHandelspris = as.numeric(SenesteHandelspris),
         SenesteUdbudspris = as.numeric(SenesteUdbudspris),
         SenesteKvmPris = as.numeric(SenesteKvmPris),
         ForecastedKvmPris = as.numeric(ForecastedKvmPris),
         NaboKvmPris = as.numeric(NaboKvmPris),
         NaboAvgKvmPris = as.numeric(NaboAvgKvmPris),
         NaboNearestKvmPris = as.numeric(NaboNearestKvmPris),
         NaboLatestKvmPris = as.numeric(NaboLatestKvmPris),
         Udbudspris = as.numeric(Udbudspris),
         OffentligVurderingVedSenesteHandel = as.numeric(OffentligVurderingVedSenesteHandel),
         ErKoebesumValideretModUdbud = factor(ErKoebesumValideretModUdbud),
         VaegtetPris = as.numeric(VaegtetPris),
         EstimeretKvmPris = as.numeric(EstimeretKvmPris),
         UdbudsKvmPris = as.numeric(UdbudsKvmPris),
         Old_VaegtetPris = as.numeric(Old_VaegtetPris))


write_csv(df,"big_df_clean.csv")

