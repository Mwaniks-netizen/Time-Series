#loading necessary libraries
library(fpp3)

#Australian beer production
#beer production revisited
#Extract data of interest and visualize
recent_production = aus_production %>% 
  filter(year(Quarter)>= 1992)
recent_production %>% 
  autoplot(Beer)+
  labs(y = "Megalitres",
   title = "Australian quarterly beer production")

#forecast quarterly beer production
#We omit the first quarter
#define and fit model, specify the trend and season components
fit_beer = recent_production %>% 
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

#plot the fitted and the observed data
augment(fit_beer) %>% 
  ggplot(aes(x = Quarter))+
  geom_line(aes(y = .fitted, colour = "Fitted"))+
  geom_line(aes(y = Beer, colour = "Data"))+
  labs(y = "Megalitres",
     title = "Australian quarterly beer production")+
  scale_color_manual(values = c(Data = "black", Fitted = "green"))

#plot the Fitted values against the actual values
augment(fit_beer) |>
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

#checking the residuals
gg_tsresiduals(fit_beer)

#produce forecasts
fit_beer %>% forecast() %>% autoplot(recent_production)

#Harmonic Regression (BEER)
fourier_beer = recent_production %>% 
  model(TSLM(Beer~ trend() + fourier(K = 2)))
fourier_beer
report(fourier_beer)

#Harmonic Regression :eating out expenditure
#Extract the data of interest(2004-2008)
aus_eat_out = aus_retail %>% 
  filter(Industry == "Cafes, restaurants and catering services",
         year(Month) %in% 2004:2008) %>% 
  summarise(
    Turnover = sum(Turnover)
  )
aus_eat_out %>% autoplot()

#define and fit a harmonic regression model
#select one with lowest CV, AICc
fitt = aus_eat_out %>% 
  model(
    k1 = TSLM(log(Turnover) ~ trend() + fourier(K=1)),
    k2 = TSLM(log(Turnover) ~ trend() + fourier(K=2)),
    k3 = TSLM(log(Turnover) ~ trend() + fourier(K=3)),
    k4 = TSLM(log(Turnover) ~ trend() + fourier(K=4)),
    k5 = TSLM(log(Turnover) ~ trend() + fourier(K=5)),
    k6 = TSLM(log(Turnover) ~ trend() + fourier(K=6))
  )
fitt

#check models performance
glance(fitt) %>% 
  select(.model,r_squared, adj_r_squared,CV, AICc)

#forecasting with regression
recent_production %>% 
  model(TSLM(Beer ~ trend() + season())) %>% 
  forecast() %>% 
  autoplot(recent_production)

#scenario based forecasting
#US Consumption
#Define and fit regression model to the data
fit_consbest = us_change %>% 
  model(
    lm = TSLM(Consumption~Income + Savings + Unemployment)
  )
fit_consbest

#create the scenarios using scenarios()
future_scenarios = scenarios(
  increase = new_data(us_change,4) %>% 
    mutate(Income = 1, Savings = 0.5, Unemployment = 0),
  decrease = new_data(us_change,4) %>% 
    mutate(Income = -1, Savings = -0.5, Unemployment = 0),
  names_to = "scenarios"
)

#pass the mable and scenarios to the forecast function
fit_forecast = forecast(fit_consbest, new_data = future_scenarios)
fit_forecast

#visualize the scenarios
us_change %>% autoplot(Consumption)+
  labs(
    y = "%change in US consumption"
  )+
  autolayer(fit_forecast)+
  labs(
    y = "% change", title = "US Consumption"
  )
