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


team_lm <- lm(final.performance ~ team.size + 
                diversity*mean_testosterone + 
                females,
              data = team_data)

team_lm_small <- lm(final.performance ~ team.size + 
                      diversity + mean_testosterone + 
                      females,
                    data = team_data)


# Before lab: have them do an activity where they work on 
# figuring out a model. Maybe talk about confounding variables
# Use the fire clusters data as an example. 
# Or maybe save this for another time?


# Fit a multiple regression model with team size, number 
# of women, diversity, and testosterone (no interaction).
# Look at the adjusted R^2. Explain why adjusted R^2 is < R^2
# Explain why this model does not 
# allow us to assess the hypothesis that diversity is 
# beneficial for groups with low testosterone, but not for
# groups with high testosterone

# Now let's add the interaction term. Fit the model, and write
# the equation of the estimated line

# Calculate the adjusted R^2 for the new model. Compare to 
# the old model. How much additional variability do we 
# explain by adding the interaction term?

# Use the model

# Which group does better -- low testosterone and high diversity,
# or high testosterone and low diversity?

# Does the relationship between diversity and performance
# depend on testosterone? Write the hypotheses, 
# do the nested F-test

# The alternative hypothesis for the nested f test 
# is that beta != 0. The researchers actually hypothesized 
# that beta < 0. Because we're only testing one parameter,
# we can also use a t-test. Do the t-test

# Calculate a confidence interval for the parameter

# Check the diagnostics. The assumptions look pretty 
# reasonable, except for a few potential outliers. 
# Refit the model without those points, and 
# re-do the hypothesis test and confidence interval. 
# Do your conclusions change?