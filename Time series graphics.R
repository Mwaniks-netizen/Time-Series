#load the necessary libraries
library(fpp3)
library(GGally)#visualizing the relationship among multiple variables, check correlations
#The index is the year in the global_economy dataset

global_economy
#A tsibble with 24320 rows and 5 columns
#The frequency of the data is quartely
#The index is the quarter column, the keys are region, state and purpose and the trips column is the measured variable
tourism

#creating a tsibble from a tibble
my_data <- tibble(
  year = 2015:2019,
  y = c(12,13,14,15,16)
  ) %>% 
  as_tsibble(
    index = year
  )
my_data

PBS
#select only the rows where ATC2 is A10
a_10=  PBS %>% filter(ATC2 == "A10") %>% 
  select(Month, Concession, Type, Cost) %>% #select only the relevant rows
  summarise(
    TotalC = sum(Cost)) %>%
  mutate(
    Cost = TotalC/1e6
  ) 
a_10
#TIME PLOT
#plotting a time plot, we'll use the autoplot which is inbuilt in ggplot2
a_10 %>%  autoplot(Cost)+
  labs(
    y = "$ (Millions)",
    title = "Australian antidiabetic drug sales"
  )
#under the hood,autoplot is combining geom point and geom line
#drawing a geom point
ggplot(a_10,
       aes(x = Month, y = Cost))+
  geom_point()
#geom line to connect the dots
ggplot(a_10,
       aes(x = Month, y = Cost))+
  geom_line()

#ANSETT
ansett
#We see that there are two keys and 30 time series
#We check distinct airports and class to derive the time series
ansett %>% distinct(Airports)
ansett %>% distinct(Class)
#10 airports *  3 class brings us 30 time series

#Lets filter all economy classes
economy = ansett %>% filter(Class == "Economy")
economy
autoplot(economy)
#All economy class flights plying the MEL-SYD route
economy_melsyd = economy %>%
  filter(Airports == "MEL-SYD") %>%
  mutate(Passengers = Passengers/1000)
economy_melsyd
autoplot(economy_melsyd)+
  labs(
    title = "Ansett Airlines Economy Class",
    subtitle = "Melbourne-Sydney",
    y = "Passengers ('000)"
  )
#investigating all the classes plying the MEL-SYD route
mel_syd = ansett %>% filter(Airports == "MEL-SYD")
mel_syd
autoplot(mel_syd)

#SEASONAL PLOTS
#let us start with a timeplot
a_10 %>%
  autoplot(Cost)
#plotting a seasonal plot
a_10 %>%
  gg_season(Cost, labels = "both")+
  labs(
    y = "$ (Millions)",
    title = "Seasonal plot : Antidiabetic drug sales"
  )
#we can see the seasonality more clearly 
#There is also a general downward trend
beer = aus_production %>%
  select(Quarter, Beer) %>%
  filter(year(Quarter)>= 1992)
#we combine the time plot with the geom point for better examination of the plot
beer %>% autoplot(Beer)+geom_point()

#We know plot the seasonal plot
beer %>% gg_season(Beer,
                   labels = "right")

#victorian electricity demand
vic_elec
#plot a time plot
vic_elec %>% autoplot()
#plot a seasonal plot
vic_elec %>% gg_season(Demand)
#plot showing weekly seasonal patterns
vic_elec %>% gg_season(Demand, period = "week")
#plot showing daily seasonal patterns
vic_elec %>% gg_season(Demand, period = "day")
#plot showing yearly seasonal patterns
vic_elec %>% gg_season(Demand, period = "year")

#SEASONAL SUBSERIES PLOTS
a_10 %>% gg_subseries(Cost)+
  labs(
    title = "Seasonal subseries: Antidiabetic drug sales",
    y = "$ (Millions) "
  )
a_10 %>% gg_season(Cost, labels = "both")
#investigating beer production in australia again
beer = aus_production %>%
  select(Quarter, Beer) %>%
  filter(year(Quarter) >= 1992)
beer

beer %>% autoplot()
beer %>% gg_season()
beer %>% gg_subseries()

#investigating australian tourism data
holidays <- tourism %>% 
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(
    Trips = sum(Trips)
  )
holidays
#starting with the timeplot
holidays %>% autoplot(Trips)
#We will use facet wrap for better visualization, seasonal plot
holidays %>% gg_season(Trips)+
  facet_wrap(vars(State), nrow= 2, scales = "free_y")
#visualizing the subseries plots
holidays %>% gg_subseries(Trips)+facet_wrap(vars(State), nrow = 2, scales = "free_y")


#Scatter plots for visualizing relationships between variables
vic_elec_day_type <- vic_elec %>%
  filter(year(Time)== 2014) %>%
  mutate(
    Day_Type = case_when(
      Holiday ~ 'Holiday',
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
vic_elec_day_type
#relationship between demand and temperature
#There is a clear non linear relationship
#The demand is higher when the temp is low due to heating
#The demand is also higher when the temperatures are high due to cooling
ggplot(vic_elec_day_type,
       aes(x = Temperature, y = Demand, colour = Day_Type))+
  geom_point()+
  scale_color_brewer(palette = 3)

#visualizing the relationship between more than two variables
#We will use the ggally package
us_change %>% ggpairs(columns = 2:6)

#lag plots are a type of scatterplots
#Beer production
new_prod = aus_production %>% filter(year(Quarter)>=1992)
new_prod
#We are interested with the beer production
new_prod %>% gg_lag(Beer, geom = "point")

#Autocorrelation
new_prod %>%
  ACF(Beer, lag_max = 9)
#then plot a time plot
#This plot is called a correlogram
#Quarter four and eight have strong positive correlations because peaks are plotted against peaks and troughs against troughs
#Quarter 2 and 6 have strong negative correlations because peaks are plotted aginst troughs
new_prod %>%
  ACF(Beer, lag_max = 9) %>% autoplot()
