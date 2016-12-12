# Soil Temperature 5 and 10 cm

climate <- snowfence2 %>% 
  select(dateTime, Tsoil5, Tsoil10, distance) %>% 
  filter(year(dateTime) == 2016  , distance %in% c("5m", "20m")) %>% 
  filter(!Tsoil5 < -8.69) # remove spike

ggplot(climate, aes(x = dateTime, y = Tsoil5, color = distance)) + geom_line() + facet_wrap(~ distance)



# calculate daily values
dailyClimate <- climate %>%
  select(-Tsoil10) %>% 
  mutate(distance = plyr:: mapvalues(distance, c("5m", "20m"), c("Snow", "Control"))) %>% 
  mutate(dateTime = ymd(format(dateTime, "%Y-%m-%d"))) %>%
  group_by(dateTime, distance) %>% 
  summarise(n = n(), value = mean(Tsoil5, na.rm = TRUE)) %>% 
  filter(!n < 90) %>% 
  #ggplot(aes(x = dateTime, y = value, color = distance)) + geom_line()
  
  
CumTemp <- dailyClimate %>%   
  mutate(doy = yday(dateTime)) %>% 
  # set all values below 5° to 0
  mutate(TempAboveTresh = ifelse(value < 1, 0, value)) %>% 
  group_by(distance) %>% 
  mutate(CumTemp = cumsum(TempAboveTresh)) %>% 
  select(-n, -TempAboveTresh, -value, -dateTime)
  #ggplot(aes(x = dateTime, y = CumTemp, color = distance)) + geom_line()



  
# calculate monthly values
monthlyClimate <- dailyClimate %>%  
  gather(key = distance, value = value, -dateTime, -n, -doy) %>% 
  mutate(month = month(dateTime)) %>%
  group_by(month, distance) %>% 
  summarise(n = n(), value = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = value, color = distance)) +
  geom_line()
  
