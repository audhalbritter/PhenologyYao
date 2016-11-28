# Trait data
trait <- read_excel("plant functional groups-Snow Fence.xlsx", col_names = TRUE)
head(trait)





### Flowering time

pheno.long %>% 
  filter(treatment == "Control", pheno.stage == "b") %>% 
  ggplot(aes(x = value)) + geom_histogram()