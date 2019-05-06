# clean data
library(lubridate)
library(tidyverse)

df <- read_csv("avm_transtype_check2.csv")

df <- df %>%
  select(-UNADR_PRIMUNIT_RESITYP_GEO_CODE) %>%
  dplyr::filter(PROP_PURPRICE > 0,
         PROP_PURPRICE < 7000000,
         PROP_PURDATE >= as_date("2018-05-01")) %>%
  mutate(difference_val = PROP_VALUATION_SVUR - PROP_VALUATION_AVM)
  
write_csv(df,"avm_transtype_clean.csv")

## filter

df_filter <- read_csv("avm_transtype_check2.csv")

df_filter <- df_filter %>%
  select(-UNADR_PRIMUNIT_RESITYP_GEO_CODE) %>%
  dplyr::filter(PROP_PURPRICE > 0,
                PROP_PURPRICE < 7000000,
                PROP_PURDATE >= as_date("2018-05-01")) %>%
  mutate(difference_val = PROP_VALUATION_SVUR - PROP_VALUATION_AVM) %>%
  filter(abs(difference_val) < mean(abs(difference_val)) + (3 * sd(difference_val)))

write_csv(df_filter,"avm_transtype_clean_filter.csv")

