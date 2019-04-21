#?ster Land 30, S?nderho, 6720 Fan? 
# KVHX:5639850030

library(tidyverse)
library(ggthemes)

theme_set(theme_light())

df <- read_csv("avm_check.csv") %>%
  mutate(houseNums = str_split(UNADR_NAME,",",2) %>% 
           map(1) %>% unlist() %>% parse_number() %>% 
           as.character(),
         House = ifelse(houseNums == "30", "target","other"))

df <- df %>% select_if(function(x) n_distinct(x) > 1)

df <- df %>% mutate_if(~n_distinct(.[]) <= 3, factor)

df <- df %>%
  select(-Kvhx,-KVH,-X,-Y, -Id,-Ejendomsnummer, -UNADR_PRIMUNIT_USAGE_CODE,
         -Right_UNADR_KVHX,PROP_VALUATION_AVM_DISC_NAME,-Sold_Property_Flag,
         -True_Performance_Flag,-ACADR_KEY,-CELL100_NAME,-PROP_KEY,-PROP_NO,-PROP_UNADR_KEY,
         -Right_UNADR_KVHX, -UNADR_NAME)

# check for large number of NAs
NAs <- df %>% group_by(houseNums) %>% apply(1, function(x) sum(is.na(x)))

df <- df %>% add_column(NAs)

df %>%
  ggplot(aes(x=houseNums,y=NAs,fill=House)) + 
  geom_bar(stat="identity") +
  scale_fill_tableau() +
  ggtitle("Number of NA columns by housenumber")

#####################################################################
# ESTIMATED M2 PRICE
# ~ . (numeric, 3 x 3)

from <- seq(4,39,9)
to   <- seq(12,39,9)
batch = 1:length(from)

for (i in 1:length(from)) {
  
  p <- df %>%
    select_if(is.numeric) %>%
    select(-NAs,-BygningensAnvendelse) %>%
    add_column(House = df$House,houseNums = df$houseNums) %>%
    select(House,houseNums,EstimeretKvmPris,everything()) %>%
    select(c(1:3,from[i]:to[i])) %>%
    gather(variable, value,-House,-houseNums,-EstimeretKvmPris) %>%
    ggplot(aes(x=value,y=EstimeretKvmPris, colour = House)) +
    facet_wrap(~variable,scales="free",ncol=3) +
    geom_point(size=3, alpha = .8) +
    scale_color_tableau() +
    ggtitle(glue::glue("EstimeretKvmPris ~ Numeric variables: {batch[i]}")) +
    scale_y_continuous(labels = function(n) {
      trans = n / 1000
      paste0(trans, "K")
    })
  
  print(p)
}

# variables of interest
# (UbebyggetGrundareal and GrundVaerdi are almost identical)

df %>%
  select(House,houseNums,GrundVaerdi, MatrikelSamletAreal,
         EstimeretKvmPris) %>%
  gather(variable, value,-House,-houseNums,-EstimeretKvmPris) %>%
  ggplot(aes(x=value,y=EstimeretKvmPris, colour = House)) +
  facet_wrap(~variable,scales="free") +
  geom_point(size=3, alpha = .8) +
  scale_color_tableau() +
  ggtitle("EstimeretKvmPris ~ Numeric variables") +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000
    paste0(trans, "K")
  })

#####################################################################
# Last purchase price ~ . (numeric, 3 x 3)

for (i in 1:length(from)) {
  
  p <- df %>%
    select_if(is.numeric) %>%
    select(-NAs,-BygningensAnvendelse) %>%
    add_column(House = df$House,houseNums = df$houseNums) %>%
    select(House,houseNums,SenesteHandelspris,everything()) %>%
    select(c(1:3,from[i]:to[i])) %>%
    gather(variable, value,-House,-houseNums,-SenesteHandelspris) %>%
    ggplot(aes(x=value,y=SenesteHandelspris, colour = House)) +
    facet_wrap(~variable,scales="free",ncol=3) +
    geom_point(size=3, alpha = .8) +
    scale_color_tableau() +
    ggtitle(glue::glue("SenesteHandelspris ~ Numeric variables: {batch[i]}")) +
    scale_y_continuous(labels = function(n) {
      trans = n / 1000000
      paste0(trans, "M")
    })
  
  print(p)
}

# variables of interest

df %>%
  select(House,houseNums,GrundVaerdi, MatrikelSamletAreal, 
         PrisA, PrisB, VaegtetPris,SenesteHandelspris) %>%
  gather(variable, value,-House,-houseNums,-SenesteHandelspris) %>%
  ggplot(aes(x=value,y=SenesteHandelspris, colour = House)) +
  facet_wrap(~variable,scales="free",ncol=3) +
  geom_point(size=3, alpha = .8) +
  ggtitle("SenesteHandelspris ~ Numeric variables") +
  scale_color_tableau() +
  scale_y_continuous(labels = function(n) {
              trans = n / 1000000
              paste0(trans, "M")
            })

#####################################################################
# count ~ . (factor, 3 x 3)

from2 <- seq(3,29,9)
to2   <- seq(11,29,9)
batch2 = 1:length(from2)

for (i in 1:length(from2)) {
  
  p <- df %>% 
    select_if(is.factor) %>%
    add_column(houseNums = df$houseNums) %>% 
    select(House,houseNums,everything(),-UNADR_PRIMUNIT_USAGE_NAME) %>%
    select(c(1:2,from2[i]:to2[i])) %>%
    gather(variable, value,-House,-houseNums) %>%
    group_by(House,variable,value) %>%
    summarise(n=n()) %>%
    ggplot(aes(x=value,y=n,fill=House)) +
    geom_col(alpha = .8) +
    facet_wrap(~variable,scales="free") +
    scale_fill_tableau() +
    ggtitle(glue::glue("Categorical variables: {batch2[i]}"))
  
  print(p)
}

#####################################################################
# EST M2 PRICE ~ . (factor, 3 x 3)

from3  <- seq(4,30,9)
to3    <- seq(12,30,9)
batch3 <- 1:length(from3)

for (i in 1:length(from3)) {
  
  p <- df %>% 
    select_if(is.factor) %>%
    add_column(houseNums = df$houseNums, 
               EstimeretKvmPris = df$EstimeretKvmPris) %>% 
    select(House, houseNums, EstimeretKvmPris,everything(),-UNADR_PRIMUNIT_USAGE_NAME) %>%
    select(c(1:3,from3[i]:to3[i])) %>%
    gather(variable, value,-EstimeretKvmPris, -House,-houseNums) %>%
    ggplot(aes(x=value,y=EstimeretKvmPris)) +
    geom_boxplot(alpha = .7) +
    geom_jitter(aes(colour=House),alpha = .8) +
    facet_wrap(~variable,scales="free") +
    scale_colour_tableau() +
    ggtitle(glue::glue("EstimeretKvmPris ~ Categorical variables: {batch3[i]}")) +
    scale_y_continuous(labels = function(n) {
      trans = n / 1000
      paste0(trans, "K")
    })
  
  print(p)
}

#####################################################################
# LAST PURCHASE PRICE ~ . (factor, 3 x 3)

for (i in 1:length(from3)) {
  
  p <- df %>% 
    select_if(is.factor) %>%
    add_column(houseNums = df$houseNums, 
               SenesteHandelspris = df$SenesteHandelspris) %>% 
    select(House, houseNums, SenesteHandelspris,everything(),-UNADR_PRIMUNIT_USAGE_NAME) %>%
    select(c(1:3,from3[i]:to3[i])) %>%
    gather(variable, value,-SenesteHandelspris, -House,-houseNums) %>%
    ggplot(aes(x=value,y=SenesteHandelspris)) +
    geom_boxplot(alpha = .7) +
    geom_jitter(aes(colour=House),alpha = .8) +
    facet_wrap(~variable,scales="free") +
    scale_colour_tableau() +
    ggtitle(glue::glue("SenesteHandelspris ~ Categorical variables: {batch3[i]}")) +
    scale_y_continuous(labels = function(n) {
      trans = n / 1000000
      paste0(trans, "M")
    })
  
  print(p)
}

# avg <- df %>% 
#   summarise_if(is.numeric, mean,na.rm=T) 
# 
# target <- df %>% select_if(is.numeric) %>%
#   filter(houseNums == "30")
# 
# name = c("other","target")
# 
# num_df <- union(avg,target) %>% add_column(name) %>%
#   select(name,everything())
# 
# num_df %>% 
#   gather(x,y, SenesteHandelspris:NAs) %>%
#   ggplot(aes(x=x,y=y,fill=name)) +
#            geom_col(position="dodge") +
#            facet_wrap(~x)

#  geom_text(aes(label=houseNums),size=3,fontface="bold")

# k means