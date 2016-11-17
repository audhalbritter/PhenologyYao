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