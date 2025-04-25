#forecasting toolbox
#load the necessary libraries
library(tidyverse)
library(fpp3)

#data tidying
gddpc <- global_economy %>% 
  mutate( GDP_per_capita = GDP / Population) %>% 
  select(Country, Year, GDP,Population, GDP_per_capita)
gddpc

#data visualization
gddpc %>% 
  filter(Country == "Kenya") %>% 
  autoplot(GDP_per_capita) +
  labs(
    title = "GDP per Capita for Kenya",
    y = "$US"
  )

#specify a model
fit <- gddpc %>% 
  model(trend_model = TSLM(GDP_per_capita ~ trend()))
fit

#produce forecasts
fit %>% forecast(h= "3 years") %>% 
  filter(Country == "Kenya") %>% 
  autoplot(gddpc)+
  labs(
    title = "3 year forecast of Kenya GDP", y = "$US"
  )
