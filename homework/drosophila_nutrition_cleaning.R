library(readr)
library(dplyr)
library(tidyr)

drosophila_original <- read_csv("drosophila_nutrition_raw.csv")

drosophila <- drosophila_original %>%
  select(gen_id, vial_id, F2_diet, diet_group, mass_mg, wing_length,
         `dev_L1-LP`, `dev_LP-A`) %>%
  rename(Generation = gen_id,
         Vial = vial_id,
         Diet = F2_diet,
         DietHistory = diet_group,
         Mass = mass_mg,
         WingLength = wing_length,
         LarvalTime = `dev_L1-LP`,
         PupalTime = `dev_LP-A`) %>%
  mutate(Diet = ifelse(Diet == "std", "standard", Diet)) %>%
  filter(WingLength < 10) # there are some weird lengths in the original data

write_csv(drosophila, "drosophila.csv")
