#loading the necessary libraries 
library(fpp3) #for forecasting
library(readxl) #to load excel files
library(janitor)

#load the dataset using the readxl package
dataset_initial = read_excel("data/Kenyas_Agricultural_Production.xlsx")
dataset_initial
view(dataset_initial) #view

#clean up the column names using the janitor package
dataset_initial = janitor::clean_names(dataset_initial)
dataset_initial
view(dataset_initial) #view

#Data transformation
kenya_avocado = dataset_initial %>% 
  filter(item == "Avocados", flag == "A", element == "Production") %>% 
  select(-flag, -flag_description, -domain_code,-domain,-year_code,-area_code_m49,-area,-element_code,-item_code_cpc)
kenya_avocado
view(kenya_avocado)

#create a tsibble from the tibble
avocado_tsibble = kenya_avocado %>% 
  tsibble(
    index = year, regular = TRUE
  )
avocado_tsibble

#plot a time plot
avocado_tsibble %>% autoplot(value) +
  labs(
    title = "Kenya Annual Avocado Production:1990-2021",
    y = "Production (tonnes)"
  )+
  scale_y_continuous(labels = scales::comma)

#Extract the training set
avocado_train = avocado_tsibble %>% 
  filter(year<= 2010)
avocado_train
tail(avocado_train)

#Define and fit the benchmark models to the training set data
avocado_mable = avocado_train %>% 
  model(
    Naive = NAIVE(value),
    Mean = MEAN(value),
    Drift = RW(value ~ drift())
  )
avocado_mable
augment(avocado_mable)

#produce forecasts
avocado_fable = avocado_mable %>% 
  forecast(h = 11)
avocado_fable
#visualize the forecasts
avocado_fable %>% autoplot(avocado_tsibble)+
  labs(
   title =  "Model's Forecasts", 
   y = "Production (tonnes)"
  )+
  scale_y_continuous(labels = scales::comma)

#check accuracy
accuracy(avocado_fable, avocado_tsibble) %>% 
  select(-ME,-MPE,-ACF1)

#Forecasting with decomposition
#Decomposition
dcmp = avocado_train %>% 
  model(
    stlf = decomposition_model(
      STL(value ~ trend(window = 10), robust = TRUE),
      NAIVE(season_adjust)
    )
  )
dcmp 

#produce forecasts and visualize
dcmp_fc = dcmp %>% 
  forecast(h = 10)
dcmp_fc %>% autoplot(avocado_tsibble)+
  labs(
    title = "Decomposition forecasts on Kenya Avocado production",
    y = "Production (tonnes)"
  )+
  scale_y_continuous(labels = scales::comma)

#check accuracy
accuracy(dcmp_fc, avocado_tsibble) %>% 
  select(-ME,-MPE,-ACF1)

