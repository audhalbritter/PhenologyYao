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

### DATA CORRECTION
# Change ripe seed to bud in week 4, data entered in wrong cell
pheno.dat$r.20[pheno.dat$plot == "SH-4" & pheno.dat$sp == "Juncus leucomelas Royle ex D. Don" & pheno.dat$week == "4"] <- NA
pheno.dat$b.17[pheno.dat$plot == "SH-4" & pheno.dat$sp == "Juncus leucomelas Royle ex D. Don" & pheno.dat$week == "4"] <- 1

# Change seed to ripe seed in week 10, data entered in wrong cell
pheno.dat$s.31[pheno.dat$plot == "GC-5" & pheno.dat$sp == "Aletris pauciflora (Klotzsch) Handel-Mazzetti" & pheno.dat$week == "10"] <- NA
pheno.dat$r.32[pheno.dat$plot == "GC-5" & pheno.dat$sp == "Aletris pauciflora (Klotzsch) Handel-Mazzetti" & pheno.dat$week == "10"] <- 7

# Delete ripe seed that only occur in week 7 and 8 in subplot 7, probably plants form outsite
pheno.dat$r.28[pheno.dat$plot == "SH-1" & pheno.dat$sp == "Rhodiola yunnanensis (Franchet) S. H. Fu" & pheno.dat$week == "7"] <- NA
pheno.dat$r.28[pheno.dat$plot == "SH-1" & pheno.dat$sp == "Rhodiola yunnanensis (Franchet) S. H. Fu" & pheno.dat$week == "8"] <- NA

# Calculate Sums of bud, flower etc.
pheno <- CalcSums(pheno.dat)

pheno <- pheno %>% 
  select(sp, plot, date, week, nr.b, nr.f, nr.s, nr.r) %>% 
  filter(!is.na(sp)) %>% 
  mutate(doy = yday(date)) %>% 
  # Change plot number
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

# Remove species
pheno <- pheno %>% 
  filter(species != "Rhododendron websterianum") %>%  # Rhododendron flowered when observations startet
  filter(!species %in% c("Anemone demissa", "Gentiana trichotoma", "Kobresia cercostachys", "Parnassia pusilla", "Primula amethystina", "Ranunculus tanguticus", "Veronica rockii", "Agrostis sinorupestris", "Aletris pauciflora", "Aster asteroides", "Astragalus skythropos", "Oxygraphis glacialis")) # Species occur only in Snow or Control
head(pheno)



# Check data, make figures for pheno.stages 
pheno %>% 
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>% 
  #filter(plot == "2") %>% 
  filter(species == "Salix souliei") %>% 
  group_by(species, pheno.stage) %>% 
  ggplot(aes(x = doy, y = value, color = pheno.stage)) +
  geom_line() +
  facet_wrap(~ plot, scales = "free")



#### CALCULATE FIRST, PEAK, END AND DURATION, DAYS BETWEEN FIRST BUD AND FLOWER, FLOWER AND SEED ETC (PHENO.STAGES IN DAYS)####
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
  # make the data nice, rename variables and order them
  mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  mutate(duration = end - (first-1)) %>% # calculate duration
  gather(key = pheno.var, value = value, -plot, -species, -pheno.stage) %>%  # create pheno.var and gather 4 variable into 1 column
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end", "duration"))) %>% 
  # Calculate days between pheno.vars
  spread(key = pheno.stage, value = value) %>% 
  # calculate difference in days between bud-flower and flower-seed
  mutate(bf = ifelse(pheno.var == "peak", f-b, NA), fs = ifelse(pheno.var == "peak", s-f, NA), sr = ifelse(pheno.var == "peak", r-s, NA)) %>%
  gather(key = pheno.stage, value = value, b, f, s, r, bf, fs, sr) %>%
  mutate(pheno.unit = ifelse(pheno.var == "duration", "days", ifelse(pheno.var == "peak" & pheno.stage %in% c("bf", "fs", "sr"), "days", "doy"))) %>% # create variable pheno.unit, doy: b,f,s,r, days: duration, bf, fs, sr
  filter(!is.na(value)) %>% # remove empty rows
  mutate(value = ifelse(value < 0, NA, value)) %>% # replace negative values with NA (e.g. if bud before flowering)
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s", "r", "bf", "fs", "sr"), c("Bud", "Flower", "Seed", "Ripe", "BudFlower", "FlowerSeed", "SeedRipe"))) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "Ripe", "BudFlower", "FlowerSeed", "SeedRipe"))) %>% 
  mutate(treatment = ifelse(plot %in% c("1", "2" ,"3", "4", "5", "6"), "Snow", "Control"))




# check species
setdiff(pheno.long$species, trait$SpeciesName)
setdiff(trait$SpeciesName, pheno.long$species)



# MERGE TRAIT DATA
pheno.long <- pheno.long %>% left_join(trait, by = c("species" = "SpeciesName"))

pheno.long <- pheno.long %>% left_join(flowertime, by = c("species" = "Species"))
head(pheno.long)




save(pheno.long, file = "PhenoLong.RData")



# Climate data
pheno.long <- pheno.long %>% mutate(climateID = paste(destSite, value, sep = "_"))
pheno.long <- pheno.long %>% left_join(CumSum2016, by = "climateID") %>% select(-site, -dateDaily, -n, -mean, -doy)



