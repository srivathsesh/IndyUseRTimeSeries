#-----------------------------
#   Library calls
#---------------------------
library(fpp3)
library(purrr)
library(xts)
# above package calls the following:

# ✓ tibble      3.0.1     ✓ tsibble     0.8.5
# ✓ dplyr       0.8.5     ✓ tsibbledata 0.1.0
# ✓ tidyr       1.0.2     ✓ feasts      0.1.2
# ✓ lubridate   1.7.4     ✓ fable       0.1.1
# ✓ ggplot2 

#------------------------------
#    Agenda to cover
#------------------------------

# 1. Whats this...?

    # 1. Contrast between ts() and tsibble()
    # 2. contrast between forecast and fable


# 2. dictionary of tsibble

# 3. plotting

# 4. modeling

# 5. inspection



#------------------------------------
#          The problem
#------------------------------------

# lets look at a tibble first

download.file("http://robjhyndman.com/data/tourism.xlsx", tourism_file <- tempfile())

my_tourism <- readxl::read_excel(tourism_file) 

my_tourism %>% skimr::skim()

RegionStatePurpose <- my_tourism %>% 
  select(Region,State,Purpose) %>% 
  distinct()

RegionStatePurpose %>% 
  pmap(function(Region,State,Purpose) my_tourism %>% 
         filter(Region == !!Region,State == !!State, Purpose == !!Purpose) %>% 
         pull(Trips) %>% ts(start = c(1998,1), frequency = 4)) -> tssets

tssets[1:10] %>% lapply(plot)

#--------------------
#      xts
#-------------------

# To use xts, the data must be all numeric and 

tourism_xts <- my_tourism %>% 
  slice(1:160) %>% 
  mutate(Key = rep(c("Business","Holiday"), each = 80)) %>% 
  select(Quarter,Trips, Key) %>% 
  pivot_wider(names_from = Key, values_from = Trips)

tourism_xts %>% 
  select(-Quarter) %>% 
  xts(order.by = ymd(tourism_xts$Quarter)) %>% 
  plot()


#---------------------
#     tsibble
#--------------------

# index
# key
# measured values


my_tourism_tsbl <- my_tourism %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(key = c(Region,State,Purpose), index = Quarter)


# watch it keeps the index automatically

my_tourism_tsbl %>% 
  group_by(Region) %>% 
  summarise(TotalVists = sum(Trips))

# use the tidyverse grammar



my_tourism_tsbl %>% 
  filter(Region == "Adelaide", Purpose == "Visiting") %>% 
  select(Trips)


# Guess whats wrong?

my_tourism_tsbl %>% 
  #filter(Region == "Adelaide", Purpose == "Visiting") %>% 
  select(Trips)

#------------------
#   Plotting
#-----------------

# autoplot's intelligence

my_tourism_tsbl %>% 
  autoplot(Trips) + theme(legend.position = "none")



# dplyr and ggplot's grammar

my_tourism_tsbl %>% 
  group_by(Region) %>% 
  summarise(TotalVists = sum(Trips)) %>% 
  filter(Region == "Adelaide") %>% 
  autoplot(size =0.3) + theme(legend.position = "none",
                     panel.background = element_rect(fill = "white",color = "black"),
                     panel.grid.major = element_line(color = "gray90"),
                    panel.grid.minor = element_line(color = "gray94")
                     ) + 
  geom_point(shape = 1,size = 2) +
  labs(x = "Year Quarter", title = "Visits in Adelaide") +
  geom_smooth(se = F,color = 'red')




# Plotly for time serirs - for the shiny developers

my_tourism_tsbl %>% 
  group_by(Region) %>% 
  summarise(TotalVists = sum(Trips)) %>% 
  filter(Region == "Adelaide") %>% 
  autoplot(size =0.3) + theme(legend.position = "none",
                              panel.background = element_rect(fill = "white",color = "black"),
                              panel.grid.major = element_line(color = "gray90"),
                              panel.grid.minor = element_line(color = "gray94")
  ) + 
  geom_point(shape = 1,size = 2) +
  labs(x = "Year Quarter", title = "Visits in Adelaide") +
  geom_smooth(se = F,color = 'red') -> p

plotly::ggplotly(p)



#  Exploring time series

my_tourism_tsbl %>% 
  filter(Region == 'Snowy Mountains') %>% 
  select(-State,-Region) %>% 
  gg_subseries(Trips) + theme_bw()


my_tourism_tsbl %>% 
  filter(Region == 'Snowy Mountains') %>% 
  select(-State,-Region) %>% 
  gg_season(Trips) + theme_bw()


my_tourism_tsbl %>% 
  filter(Region == 'Snowy Mountains',Purpose == "Holiday") %>% 
  select(-State,-Region) %>% 
  gg_tsdisplay(Trips)




my_tourism_tsbl %>% 
  filter(Region == 'Snowy Mountains',Purpose == "Holiday") %>% 
  select(-State,-Region) %>% 
  gg_lag(geom = "point") + theme_bw()




my_tourism_tsbl %>% 
  filter(Region == 'Snowy Mountains',Purpose == "Holiday") %>% 
  select(-State,-Region) %>% 
  ACF() %>% 
  autoplot()


my_tourism_tsbl %>% 
  filter(Region == 'Snowy Mountains',Purpose == "Holiday") %>% 
  select(-State,-Region) %>% 
  PACF() %>% 
  autoplot()



# What if there are more than one measured variables?

global_economy %>%
  filter(Code == "USA") %>%
  autoplot(.vars = vars(GDP,CPI))

# What if there are multiple seasonality within the data?

vic_elec <- tsibbledata::vic_elec


vic_elec %>% 
  autoplot(vars(Demand,Temperature), col = "gray") + theme_bw()



vic_elec %>% 
  gg_season(Demand) + theme_bw()


vic_elec %>% 
  gg_season(Demand,period = "month") + theme_bw() # try week , # day


library(sugrrants) 
vic_elec %>%
  filter(year(Date) == 2014) %>% mutate(Hour = hour(Time)) %>% 
  frame_calendar(
    x = Hour, y = Demand, date = Date,
    nrow = 4 ) %>%
  ggplot(aes(x = .Hour, y = .Demand, group = Date)) +
  geom_line() -> p1 

prettify(p1,
         size = 3,
         label.padding = unit(0.15, "lines"))












