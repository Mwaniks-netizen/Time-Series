#Souvenirs shop analysis from souvenirs tsibble in the fable package
#Loading the necessary libraries
library(fpp3)

#Extract the data of interest
souvenirs %>% autoplot(Sales)+labs(title = "Sales from 1988 - 1993")

#stabilize the variance by doing a log transformation
souvenirs_log = souvenirs %>% 
  mutate(
    log_sales = log(Sales)
  )
souvenirs_log

#plot a timeplot
souvenirs_log %>% autoplot(log_sales)+ labs(title = "Stabilizing variance")

#Fit a regression model with a linear trend and seasonal dummies
souvenirs_reg_model = souveneirs_log %>% 
  model(
    lm = TSLM(log_sales~ trend() + season())
  )
souvenirs_reg_model

#Reporting on the model
report(souvenirs_reg_model)

#plot the residuals against time
augment(souvenirs_reg_model) %>% 
  ggplot(aes(x = Month, y = .innov))+
  geom_point()+
  labs(
    title = "Residuals against time"
  )

#plot the residuals against the fitted values
augment(souvenirs_reg_model) %>% 
  ggplot(aes(x = .fitted, y = .innov))+
  geom_point()+
  labs(
    title = "Residuals against fitted values",
    x = "Residuals",
    y = "Fitted "
  )

#plot boxplots of residuals for each month
augment(souvenirs_reg_model) %>% 
  mutate(
    new_month = factor(month(Month, label = TRUE))
  ) %>% 
  ggplot(
    aes(x = new_month, y = .innov)
  )+
  geom_boxplot()

#perform a ljung box test
augment(souvenirs_reg_model)%>% 
  features(.innov, ljung_box, lag = 10)

#checking the residual diagnostic plots
souvenirs_reg_model %>% gg_tsresiduals()

#Predict sales for 1994, 1995, 1996
souvenirs_prediction = souvenirs_reg_model %>% 
  forecast(h = "3 years")
souvenirs_prediction

#produce a timeplot of the forecasts
souvenirs_prediction %>% 
  autoplot(souvenirs_log) + 
  labs(title = "Forecasts of sales using Regression")

#Plot the data against the fitted values
augment(souvenirs_reg_model) |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_sales, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Sales",
       title = "Souvenirs' shop monthly sales") +
  guides(colour = guide_legend(title = "Series"))

#Check five prediction accuracy measures
glance(souvenirs_reg_model) %>% 
  select(adj_r_squared, AICc, AIC, CV, BIC)

