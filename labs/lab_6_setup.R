library(asw.cluster)
library(tidyverse)

individual_data <- read_csv("Individual Data for Faultlines.csv")
team_data <- read_csv("Team Data for Faultlines.csv")

data_for_asw <- individual_data %>%
  select(team.id, 
         Gender, 
         Ethnicity,
         Country) %>%
  data.frame()

my_ASW <- faultlines(data = data_for_asw, 
                     group.par = "team.id", 
                     attr.type = c("nominal",
                                   "nominal",
                                   "nominal"), 
                     attr.weight = c(1,1,1),
                     method = "thatcher")

fl_values <- c()
team_ids <- c()
for(i in 1:74){
  fl_values[i] <- my_ASW[[i]]$fl.value
  team_ids[i] <- my_ASW[[i]]$team
}


team_data <- team_data %>%
  select(team.id,
         team.size,
         final.performance,
         females) %>%
  mutate(gender_prop = females/team.size) %>%
  mutate(diversity = 1 - fl_values) %>%
  inner_join(individual_data %>%
               group_by(team.id) %>%
               summarize(mean_testosterone = mean(Testosterone, 
                                                  na.rm=T),
                         mean_cortisol = mean(Cortisol,
                                              na.rm=T)),
             by = "team.id")


team_data <- team_data %>%
  select(final.performance, 
         team.size, 
         females,
         diversity,
         mean_testosterone) %>%
  rename(performance = final.performance,
         size = team.size,
         testosterone = mean_testosterone)
