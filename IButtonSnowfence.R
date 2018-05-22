### Reading in iButton data 2017
library("tidyverse")
library("lubridate")
library("readxl")
library("stringi")
library("data.table")

pn <- . %>% print(n = Inf)

# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = "~/Dropbox/Bergen/China/Master thesis Yao/Phenology/PhenologyYao/Data/ibutten_data_in_the_first_three_months/", pattern = "xls", recursive = TRUE, full.names = TRUE)


#### Read in iButtons Function
ReadIniButtons <- function(textfile){
  print(textfile)
  
  dat <- read_excel(textfile, sheet = 1, skip = 10, col_names = FALSE, col_types = c("date", "numeric", "numeric"))
  
  dat <- dat %>%
    setNames(nm = c("date", "temperature", "moisture"))
  
  # Extract siteID, iButtonID and Year from file name
  dat$ID <- basename(textfile)
  dat <- dat %>%
    mutate(ID = basename(textfile))
  return(dat)
}


# Read in iButton data
mdat <- map_df(myfiles, ReadIniButtons)

# Extract turfID and depth from ID
iButton <- mdat %>% 
  filter(!is.na(temperature)) %>% 
  mutate(plot = substring(ID, 4,4)) %>% 
  mutate(depth = substring(ID, 5,6)) %>%
  mutate(depth = plyr::mapvalues(depth, c("-0", "-3", "-5", "-1"), c("0cm", "30cm", "-5cm", "-10cm"))) %>% 
  mutate(depth = factor(depth, levels = c("-5cm", "0cm", "30cm", "-10cm"))) %>% 
  mutate(treatment = substring(ID, 10, 11)) %>% 
  mutate(treatment = plyr::mapvalues(treatment, c("-C", "C-", "-S", "S-"), c("Control", "Control", "Snow", "Snow"))) %>% 
  mutate(treatment = factor(treatment, levels = c("Control", "Snow")))

iButton <- setDT(iButton)


### CALCULATE DAILY DATA ###
dailyiButton <- iButton %>%
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  gather(key = variable, value = value, temperature, moisture) %>% 
  group_by(date, depth, treatment, variable) %>%
  summarise(n = n(), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(n))


SoilMoist <- dailyiButton %>% 
  filter(variable == "moisture", !is.na(mean)) %>% 
  ggplot(aes(x = date, y = mean, colour = treatment)) +
  labs(x = "", y = "Daily mean soil moisture in %") +
  geom_line() +
  scale_colour_manual(values = c("grey", "blue"))
ggsave(SoilMoist, filename = "Figures/SoilMoist.jpg", width = 8, height = 5, dpi = 300)


SoilTemp <- dailyiButton %>% 
  filter(variable == "temperature", !is.na(mean), depth == "-5cm") %>% 
  ggplot(aes(x = date, y = mean, colour = treatment)) +
  labs(x = "", y = "Daily mean soil temperature in °C") +
  geom_line() +
  scale_colour_manual(values = c("grey", "blue"))
ggsave(SoilTemp, filename = "Figures/SoilTemp.jpg", width = 8, height = 5, dpi = 300)

SoilMT <- dailyiButton %>% 
  filter(!is.na(mean), depth %in% c("-5cm", "-10cm")) %>% 
  ggplot(aes(x = date, y = mean, colour = treatment)) +
  labs(x = "", y = "Daily mean soil moisture/temperarure") +
  geom_line() +
  scale_colour_manual(values = c("grey", "blue")) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1)
ggsave(SoilMT, filename = "Figures/SoilMT.jpg", width = 8, height = 5, dpi = 300)

SoilTemp2 <- dailyiButton %>% 
  filter(variable == "temperature", !is.na(mean), depth != "-10cm") %>% 
  ggplot(aes(x = date, y = mean, colour = treatment)) +
  labs(x = "", y = "Daily mean temperarure in °C") +
  geom_line() +
  scale_colour_manual(values = c("grey", "blue")) +
  facet_wrap(~ depth)
ggsave(SoilTemp2, filename = "Figures/SoilTemp2.jpg", width = 8, height = 5, dpi = 300)

# Check for duplicate values
iButton %>% 
  group_by(date, turfID, depth, value) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# remove duplicates
iButton <- iButton %>%
  group_by(date, depth, value) %>% 
  slice(1)

# Clean data
iButton <- iButton %>% 
  ungroup() %>% 
  # remove everything before 1. May in 2017 (before put to the field)
  filter(!date < "2017-05-01 01:00:00") %>% 
  # zap strongly negative and positive spikes
  mutate(value = ifelse(depth == "soil" & value > 25, NA, value)) %>% # soil +25
  mutate(value = ifelse(depth %in% c("soil", "ground") & value < -7, NA, value)) # soil -7

save(iButton, file = "TemperatureiButton.RData")







# Check each turf
iButton %>% 
  filter(depth == "air") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  #scale_color_manual(values = c("blue", "green", "brown")) +
  facet_wrap( ~ turfID)

# Plot data from any hours of the day
iButton %>% 
  filter(between(hour(date), 3,5), depth == "air") %>% 
  mutate(month = ymd(format(date, "%Y-%m-15"))) %>% 
  select(-date) %>%
  group_by(month, site, treatment, depth) %>%
  filter(!is.na(value)) %>%
  summarise(Tmean = mean(value, na.rm = TRUE), n = n()) %>% 
  ggplot(aes(x = month, y = Tmean, color = treatment)) +
  geom_line() +
  labs(y = "Mean monthly night temperature") +
  facet_grid(depth ~ site)


# Plot day and night temperature
iButton %>% 
  filter(depth == "soil") %>% 
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(date, depth, site, treatment) %>%
  summarise(n = n(), min = min(value), max = max(value)) %>% 
  mutate(diff = max - min) %>% 
  ggplot(aes(x = date, y = diff, color = treatment)) +
  geom_line() +
  facet_grid(treatment ~ site)


# Calculate Monthly data
monthlyiButton <- iButton %>% 
  mutate(month = lubridate::ymd(format(date, "%Y-%m-15"))) %>% 
  select(-date) %>%
  group_by(month, site, treatment, depth) %>%
  filter(!is.na(value)) %>%
  summarise(n = n(), Tmean = mean(value, na.rm = TRUE), Tse = sd(value)/sqrt(n), Tmin = min(value, na.rm = TRUE), Tmax = max(value, na.rm = TRUE)) %>% 
  # remove April and September at L site, because there is too little data
  filter(n > 100) %>% 
  select(-n) %>% 
  ungroup() %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L")))

save(monthlyiButton, file = "Temperature_monthlyiButton.RData")


# Plot monthly data by site and depth
monthlyiButton %>% 
  ggplot(aes(x = month, y = Tmean, color = treatment)) +
  geom_line() +
  facet_grid(depth ~ site)



monthlyiButton %>% 
  group_by(site, month, depth, treatment) %>% 
  summarise(Mean = mean(Tmean)) %>% 
  spread(key = treatment, value = Mean) %>% 
  mutate(Difference = OTC - C) %>% 
  filter(month(month) != 4) %>% 
  arrange(site, depth, month) %>% pn
