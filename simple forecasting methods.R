library(fpp3)
#fitting the simple models to aus production tsibble
brics <- aus_production %>% 
  filter(!is.na(Bricks)) %>% 
  model(
    mean_method = MEAN(Bricks),
    naive_method = NAIVE(Bricks),
    seasonal_naive = SNAIVE(Bricks),
    drift = RW(Bricks ~ drift())
  )
brics
#pass the mable to the forecast function
brics_forecast <- brics %>% 
  forecast( h = " 5 years")
view(brics_forecast)

#visualize
#level = null rmeans dont plot the prediction interval, for clearer visual
brics_forecast %>% 
  autoplot(aus_production, level = NULL) +
  labs(
    title = "Clay brick production forecast", y = "Millions of bricks")+
  guides(colour = guide_legend(title = "Forecast"))


#Example using facebook data
#Extracting and preprocessing data
#Important to mutate the trading date for methods like naive and drift to work
#Since trading isnt done daily
fb_stock <- gafa_stock %>% 
  filter(Symbol == "FB") %>% 
  mutate(
    trading_date = row_number(),
  ) %>% 
  update_tsibble(index = trading_date, regular = TRUE)
#specify and evaluate models
#no need for seasonal naive method as there is no seasonality 
fb_stock %>% 
  model(
    drift = RW(Close ~ drift()),
    naivee = NAIVE(Close),
    meann = MEAN(Close)
  ) %>% 
  forecast(h = 42) %>% 
  autoplot(fb_stock, level= NULL)+
  labs(
    title = "FB Closing stock price", y = "$US"
  )+
  guides(colour = guide_legend(title = "Forecast"))
