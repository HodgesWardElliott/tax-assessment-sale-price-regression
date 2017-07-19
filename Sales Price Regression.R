


# sales data ------------------------------------------------------------------
sale_augmented <- read_rds("data/sales_augmented.rds")


# function to quickly glimpse BBL
lookat <- function(boro= 1,blck = 829,lt = 16) sale_augmented %>% filter(BOROUGH == boro, BLOCK == blck, LOT == lt) %>% arrange(desc(SALE_DATE)) %>% glimpse()
lookhead <- function(boro= 1,blck = 829,lt = 16) sale_augmented %>% filter(BOROUGH == boro, BLOCK == blck, LOT == lt)


sale_augmented %>% filter(BOROUGH == 1, BLOCK == 829, LOT == 16) %>% 
  select(ADDRESS,SALE_DATE, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal)




sale_augmented %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio)) %>% 
  filter(AssessTotal<500000000) %>% 
  #filter(SALE.PRICE>5e+07) %>% 
  ggplot()+
  aes(x = SALE.PRICE, y=AssessTotal, group = Building_Type, color = Building_Type)+
  geom_point()+
  geom_smooth(se=F, method = "lm")+
  scale_y_continuous(labels=scales::comma)


library(modelr)

f1 <- lm(AssessTotal~SALE.PRICE, data = sale_augmented)

group_model <- function(df) {
  lm(AssessTotal ~ SALE.PRICE, data = df)
}

extract_coef <- function(model) coef(model)["SALE.PRICE"]

extract_coef(f1)

by_group<-
  sale_augmented %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio)) %>% 
  mutate(AssessSD = sd(AssessTotal), AssessZscore = scale(AssessTotal)) %>% 
  #filter(abs(AssessZscore)<3) %>% 
  group_by(Building_Type) %>% 
  nest() %>% 
  mutate(Number_of_Sales = map_dbl(data,nrow)) %>% 
  mutate(model = map(data, group_model)) %>% 
  mutate(Sale_Coef = map_dbl(model,extract_coef)) %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance) %>% 
  arrange(-Sale_Coef) %>% 
  filter(p.value <= (0.05))