# IMPORT DATA

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")


#### READ IN DATA ####
#options(warn = 1)
pheno.dat <- plyr::ldply(1:17, ReadExcelSheets)

# Calculate Sums of bud, flower etc.
pheno <- CalcSums(pheno.dat)

pheno <- pheno %>% 
  select(sp, plot, date, week, nr.b, nr.f, nr.s, nr.r) %>% 
  filter(!is.na(sp)) %>% 
  mutate(doy = yday(date)) %>% 
  mutate(plot = plyr::mapvalues(plot, c("SH-9",  "SH-1",  "SH-6",  "SH-2",  "SH-3",  "SH-4",  "GC-1",  "GC-2",  "GC-5",  "GC-10", "GC-9",  "GC-8",  "GC-7"), c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")))



# split authority from name
spNames <- strsplit(pheno$sp, " ")
nameAuthority <-plyr::ldply(spNames, function(x){
  if(any(grepl("var.", x, fixed = TRUE))){
    speciesName <- paste(x[1:4], collapse = " ")
    authority <- paste(x[-(1:4)], collapse = " ")
  } else {
    speciesName <- paste(x[1:2], collapse = " ")  
    authority <- paste(x[-(1:2)], collapse = " ")
  }
  if(is.na(authority)) authority <- ""
  data.frame(speciesName, authority, stringsAsFactors = FALSE)
})

pheno$species <- paste(sapply(spNames, function(x) x[1]), sapply(spNames, function(x) x[2]), sep = " ")
head(pheno)
unique(pheno$species)

# check these species
# 景天叶Gentiana crassula"
# "紫晶Primula amethystina"


### DATA CORRECTION



# Check data, make figures for pheno.stages 
pheno %>% 
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>% 
  #filter(plot == "2") %>% 
  filter(species == "Juncus leucomelas") %>% 
  group_by(species, pheno.stage) %>% 
  ggplot(aes(x = doy, y = value, color = pheno.stage)) +
  geom_line() +
  facet_wrap(~ plot, scales = "free")



#### CALCULATE FIRST, PEAK, END AND DURATION ####
### MAKE LONG DATA SET ###
pheno.long <- pheno %>%
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>% # make variable pheno.stage
  group_by(plot, species, pheno.stage) %>%  # group by turfID, species and phenological stage to calculate first, end etc for each stage
  mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  filter(value > 0) %>%
  summarize(first = first(doy), end = last(doy), peak = doy[which.max(value)]) %>%
  filter(first > minDoy) %>% # remove if plant is flowering in the first week
  ungroup() %>% 
  select(-minDoy) %>% # remove this variable
  #mutate_each(funs(as.numeric), first, peak, end) %>% # make variables numeric (probably not necessary)
  # make the data nice, rename variables and order them
  mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  mutate(duration = end - (first-1)) %>% # calculate duration
  gather(key = pheno.var, value = value, -plot, -species, -pheno.stage) %>%  # create pheno.var and gather 4 variable into 1 column
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end", "duration")))
head(pheno.long)




#### CALCULATE DAYS BETWEEN FIRST BUD AND FLOWER, FLOWER AND SEED ETC (PHENO.STAGES IN DAYS) ####
pheno.long <- pheno.long %>% 
  spread(key = pheno.stage, value = value) %>% 
  # calculate difference in days between bud-flower and flower-seed
  mutate(bf = ifelse(pheno.var == "peak", f-b, NA), fs = ifelse(pheno.var == "peak", s-f, NA), sr = ifelse(pheno.var == "peak", r-s, NA)) %>%
  gather(key = pheno.stage, value = value, b, f, s, r, bf, fs, sr) %>%
  mutate(pheno.unit = ifelse(pheno.var == "duration", "days", ifelse(pheno.var == "peak" & pheno.stage %in% c("bf", "fs", "sr"), "days", "doy"))) %>% # create variable pheno.unit, doy: b,f,s,r, days: duration, bf, fs, sr
  filter(!is.na(value))%>% # remove empty rows
  mutate(value = ifelse(value < 0, NA, value)) %>% # replace negative values with NA (e.g. if bud before flowering)
  mutate(pheno.stage = factor(pheno.stage, levels = c("b", "f", "s", "r", "bf", "fs", "sr"))) %>% 
  mutate(treatment = ifelse(plot %in% c("1", "2" ,"3", "4", "5", "6"), "Snow", "Control")) 


ggplot(pheno.long, aes(x = treatment, y = value)) +
  geom_boxplot() +
  facet_grid(pheno.stage ~ pheno.var)

save(pheno.long, file = "PhenoLong.RData")

  # Trait data
trait <- read_excel("SpeciesTraits2016_China.xlsx", col_names = TRUE)
head(trait)

# define flowering time
# early: <= 4 month until June
# mid: <= 4 month and between April and August
# late: <= 4 month from July
# late: >= 4 month
trait <- trait %>% mutate(flTime = 
                   ifelse(floweringTime %in% c("Apr-Jun", "Apr-May", "Jun", "May-Jun"), "early",
                                 ifelse(floweringTime %in% c("Jul-Aug", "Apr-Jul", "Jul", "Jun-Jul", "May-Jul", "May-Jul-(Aug)", "summer", "Jun-Aug", "Jun-Sep"), "mid", 
                                        ifelse(floweringTime %in% c("Aug-Nov", "Aug-Oct", "Aug-Sep", "Jul-Sep", "Jul-Oct"), "late", "always")))
                 ) %>%
  mutate(flTime = ifelse(sp %in% c("Car.sp.black","Car.sp.black.big","Car.sp.middle","Car.sp.yellow","Fes.sp.big","Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow"), "early", flTime))


# check species
setdiff(pheno.long$species, trait$sp)
setdiff(trait$sp, pheno.long$species)

pheno.long <- pheno.long %>% left_join(trait, by = c("species" = "sp"))


# Making Figures for each species
name <- pheno.long %>% 
  filter(treatment %in% c("OTC", "Control")) %>% 
  filter(origSite == "H") %>% 
  #filter(functionalGroup == "forb") %>% 
  #filter(flTime == "early") %>%
  filter(pheno.stage %in% c("f"), pheno.var %in% c("first", "duration")) %>% 
  #filter(pheno.stage %in% c("bf", "fs", "sr")) %>% 
  #filter(pheno.var %in% "duration") %>% 
  group_by(species, treatment, pheno.var) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  na.omit() %>% 
  gather(key = treatment, value = value, -species, -pheno.var) %>% 
  spread(key = pheno.var, value = value) %>% 
  ggplot(aes(x = first, y = species, color = treatment)) + geom_point() +
  geom_segment(aes(x=first, xend=(first+duration), y=species, yend=species),size=1)

name + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("H site first flowering")


PhenologicalStages <- pheno.long %>% 
    filter(treatment %in% c("OTC", "Control","Warm","Local")) %>% 
    filter(origSite %in% c("H", "A")) %>% 
    filter(functionalGroup %in% c("forb", "graminoid")) %>% 
    #filter(flTime == c("early", "late")) %>%
    #filter(pheno.stage %in% c("b", "f", "s"), pheno.var == "first") %>% 
    #filter(pheno.stage %in% c("bf", "fs", "sr")) %>% 
    filter(pheno.var %in% "duration") %>% 
    #group_by(species, treatment) %>% 
    #summarise(mean = mean(value)) %>% 
    ggplot(aes(x = treatment, y = value, color = functionalGroup)) +
    geom_boxplot() +
    facet_grid(origSite~pheno.stage)
  
PhenologicalStages + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("DOY")
PhenologicalDuration + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")
PhenologicalFirstEnd + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")


# Climate data
pheno.long <- pheno.long %>% mutate(climateID = paste(destSite, value, sep = "_"))
pheno.long <- pheno.long %>% left_join(CumSum2016, by = "climateID") %>% select(-site, -dateDaily, -n, -mean, -doy)



