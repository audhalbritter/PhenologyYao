library("tidyverse")
library("readxl")
library("broom")

### Soil CNP
cnp <- read_excel(path = "Data/soil data_CNP_2015.xlsx", sheet = 1)

cnp <- cnp %>% 
  rename(depth = `depth(cm)`, total_C = `total C（g/kg）`, organic_C = `organic C（g/kg）`, total_N = `total N（g/kg）`, available_N = `available N（mg/kg）`, total_P = `total P(%)`, available_P = `available P（mg/kg）`) %>% 
  gather(key = variable, value = value, total_C, organic_C, total_N, available_N, total_P, available_P)


cnp %>% 
  ggplot(aes(x = snow, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y")

dfCNP <- cnp %>% 
  group_by(variable) %>% 
  do(fit = lm(value ~ snow, data = .))

tidy(dfCNP, fit)



### Ingrowth core
ingrowth <- read_excel(path = "Data/root CN_2016_ingrowth core.xls", sheet = 1)
ingrowth <- ingrowth %>% 
  rename(total_C = `total C(g/kg)`, total_N = `total N(g/kg)`) %>% 
  mutate(depth = plyr::mapvalues(depth, c("0~10", "10~20"), c("0-10", "10-20"))) %>% 
  gather(key = variable, value = value, total_C, total_N)


ggplot(ingrowth, aes( x = treatment, y = value)) +
  geom_boxplot() +
  labs(x = "", y = "Total C or N in g/kg") +
  facet_wrap(~variable, scales = "free_y")

dfingrowth <- ingrowth %>% 
  group_by(variable) %>% 
  do(fit = lm(value ~ treatment, data = .))

tidy(dfingrowth, fit)



### Root traits
roottrait <- read_excel(path = "Data/Root-trait data_2016.xlsx", sheet = 1)
roottrait <- roottrait %>% 
  rename(Length = `Length(cm)`, RootVolume = `RootVolume(cm³)`, MeasuredWeight = `Measured Weight(g)`, Weight = `Weight(g)`, Length2 = Length, SomeVolume = `土壤体积(cm³)`) %>% 
  gather(key = Variable, value = Value, -Year, -ImageFileName, -Site, -Treatment, -Plot, -Hole, -Depth, -Class, -Sort)

roottrait %>% 
  filter(Year == 2015) %>% 
  ggplot(aes(x = Treatment, y = Value)) +
         geom_boxplot() +
         facet_grid(Variable ~ Depth, scales = "free_y")

 roottrait %>% 
  filter(Year == 2016) %>% 
  ggplot(aes(x = Treatment, y = Value)) +
  geom_boxplot() +
  facet_grid(Variable ~ Depth, scales = "free_y")   

dfroottrait <- roottrait %>% 
  group_by(Year, Depth, Variable) %>% 
  do(fit = lm(Value ~ Treatment, data = .))

tidy(dfroottrait, fit) %>% 
  filter(p.value < 0.05, term != "(Intercept)")
