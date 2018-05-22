# Import Climate Data
# read libraries
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")
library("data.table")

### SOIL TEMPERATURE DATA
# Function to read in snowfence data
ReadExcelSheets <- function(sheet){
  #dat <- read_excel("/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/TransplantChina/ClimateData/Snowfence/SoilTempratureMoisture_Gongga.xlsx", sheet = sheet, col_names = TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text")) # read excel file
  dat <- read_excel("SoilTempratureMoisture_Gongga.xlsx", sheet = sheet, col_names = TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text")) # read excel file
  dat$distance <- substr(colnames(dat)[2],1,2) # grab first two characters
  names(dat) <- c("dateTime", "Tsoil5", "Tsoil10", "Tsoil20", "waterContent5", "waterContent10", "waterContent20", "Notes", "distance")
  dat
}

# Import
snowfence <- plyr::ldply(1:6, ReadExcelSheets)

# rename
snowfence <- snowfence %>% 
  mutate(distance = plyr::mapvalues(distance, c("0-", "2.", "5-", "10", "20", "CK"), c("0m", "2.5m", "5m", "10m", "20m", "Control"))) %>%  # rename
  mutate(distance = factor(distance, levels = c("0m", "2.5m", "5m", "10m", "20m", "Control"))) %>%  # order factor
  mutate(dateTime = ymd_hms(dateTime, tz = "Asia/Shanghai"))

# Data cleaning, remove spikes
snowfence2 <- snowfence %>% 
  mutate(Tsoil20 = ifelse(Tsoil20 > 25 | Tsoil20 < -10, NA, Tsoil20)) %>% 
  mutate(Tsoil5 = ifelse(Tsoil5 > 25 | Tsoil5 < -10, NA, Tsoil5)) %>% 
  mutate(Tsoil10 = ifelse(Tsoil10 > 25 | Tsoil10 < -10, NA, Tsoil10)) %>% 
  mutate(waterContent5 = ifelse(waterContent5 > 1000 , NA, waterContent5))

snowfence2 <- setDT(snowfence2)

#summary(snowfence2)  
ggplot(snowfence2, aes(x = dateTime, y = Tsoil10)) + geom_line() + facet_wrap(~distance) + geom_vline(xintercept = as.numeric(ymd_hms("2016-03-01 00:00:01")), color = "red") +
 geom_vline(xintercept = as.numeric(ymd_hms("2016-05-01 00:00:01")), color = "blue")

head(snowfence2)
dim(snowfence)

# Caclculate daily data
dailyData <- snowfence2 %>% 
  group_by(dateTime) %>% filter(row_number(dateTime) == 1) %>%
  gather(key = variable, value = value, Tsoil5, Tsoil10, Tsoil20, waterContent5, waterContent10, waterContent20) %>% 
  mutate(day = dmy(format(dateTime, "%d.%b.%Y"))) %>%
  group_by(day, variable, distance) %>%
  summarise(n = n(), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(n))


dailyData %>% 
  select(-n, -se) %>% 
  spread(key = distance, value = mean) %>% 
  mutate(Diff2.5 = `2.5m` - Control, Diff5 = `5m` - Control) %>% 
  gather(key = Diffs, value = value, Diff2.5, Diff5) %>% 
  filter(value > -10) %>% 
  ggplot(aes(x = day, y = value, colour = Diffs)) +
  geom_line()

dailyData %>% 
  filter(distance %in% c("Control", "2.5m"), mean < 60, variable == "Tsoil5") %>% 
  ggplot(aes(x = day, y = mean, colour = distance)) +
  labs(x = "", y = "mean daily temperature in °C") +
  geom_line()



# Calculate monthly data
monthlyData <- dailyData %>% 
  filter(!is.na(mean)) %>%
  mutate(date = dmy(paste0("15-",format(day, "%b.%Y")))) %>%
  group_by(date, variable, distance) %>%
  summarise(n = n(), value = mean(mean), se = sd(mean)/sqrt(n)) 
  
monthlyData %>% 
  filter(variable == "Tsoil10", distance %in% c("Control", "2.5m")) %>% 
  filter(n > 16) %>% 
  ggplot(aes(x = date, y = value, ymin = value - se, ymax = value + se, colour = distance)) +
  geom_line() +
  geom_point() +
  geom_errorbar() +
  labs(x = "", y = "Monthly temperature in °C")
ggsave(MonthlyTemp, filename = "Figures/MonthlyTemp.jpg", width = 8, height = 5, dpi = 300)

### SNOWDEPTH DATA
snowdepth <- read_excel("/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/TransplantChina/ClimateData/Snowfence/SnowDepth_Gongga.xlsx", col_names = TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "date"))
snowdepth <- read_excel("SnowDepth_Gongga.xlsx", col_names = TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "date"))
snowdepth$dateTime <- ymd_hms(snowdepth, tz = "Asia/Shanghai")
head(snowdepth)
summary(snowdepth)

snowdepth %>% 
  gather(key = Snowdepth, value = value, Snow_Depth_1, Snow_Depth_2_Control) %>%
  ggplot(aes(x = TIMESTAMP, y = value, color = Snowdepth)) +
  geom_line() +
  ylim(-0.25,1.4)
  
ggplot(snowdepth, aes(x = TIMESTAMP, y = Snow_Depth_1)) + geom_line() + ylim(-0.25,1.4) + scale_x_datetime(date_breaks = "month") + theme(axis.text.x = element_text(angle = 90))




control <- read_excel("Data/weather data in 2016.xlsx", col_names = TRUE, sheet = 1)
snow <- read_excel("Data/weather data in 2016.xlsx", col_names = TRUE, sheet = 2)

snow <- snow %>% 
  mutate(Treatment = "Snow") %>% 
  rename(Temperature_5 = `Tem-5cm`, Temperature_10 = `Tem-10cm`, Temperature_20 = `Tem-20cm`, Moisture_5 = `Mois-5cm`, Moisture_10 = `Tem-10cm__1`, Moisture_20 = `Tem-20cm__1`)

control %>% 
  mutate(Treatment = "Control") %>% 
  rename(Date = date, Temperature_5 = `Tem-5cm`, Temperature_10 = `Tem-10cm`, Temperature_20 = `Tem-20cm`, Moisture_5 = `Mois-5cm`, Moisture = `Mois-10cm`, Moisture_20 = `Mois-20cm`) %>% 
  bind_rows(snow) %>% 
  gather(key = Variable, value = Value, - Date, -Treatment) %>% 
  filter(Variable %in% c("Temperature_5", "Temperature_10", "Temperature_20")) %>% 
  ggplot(aes(x = Date, y = Value,  colour = Treatment)) +
    geom_line() +
  facet_wrap(~ Variable, ncol = 1)
  

   
    

ggplot(weather, aes(x = date, y = `Tem-5cm`)) +
  geom_line()
