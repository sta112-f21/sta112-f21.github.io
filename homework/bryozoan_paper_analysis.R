library(tidyverse)

metab <- read_csv("bryozoan_raw.csv")
metab_b <- metab[,1:4]
metab_w <- metab[,5:8]
colnames(metab_b) <- c("Stage", "Run", "Mass", "Metabolic")
colnames(metab_w) <- c("Stage", "Run", "Mass", "Metabolic")
metab_b <- metab_b %>%
  mutate(Species = "bugula")
metab_w <- metab_w %>%
  mutate(Species = "watersipora")

bryozoan <- metab_b %>%
  rbind(metab_w) %>%
  drop_na() %>%
  mutate(Mass = 10^Mass,
         Metabolic = 10^Metabolic,
         Stage = tolower(Stage)) %>%
  select(Species, Run, Stage, Mass, Metabolic) %>%
  arrange(Species, Run, Stage) %>%
  mutate(Run = cumsum(c(1, diff(Run) != 0)))




# Number of rows in the data
nrow(bryozoan)

# Number of individuals
bryozoan %>%
  filter(Stage != "late") %>%
  nrow()

# check the same number of individuals is 
# measured in early and late stage for each run
# (result is empty, so the numbers are always the same)
bryozoan %>%
  filter(Stage != "larvae") %>%
  group_by(Species, Run) %>%
  summarize(num_early = sum(Stage == "early"),
            num_late = sum(Stage == "late")) %>%
  ungroup() %>%
  filter(num_early != num_late)

# check whether mass and metabolic rate were measured once
# or twice
# For the Watersipora: there are no NAs after the full join,
# so each mass is measured once
bryozoan %>%
  filter(Species == "watersipora",
         Stage == "early") %>%
  full_join(bryozoan %>%
              filter(Species == "watersipora",
                     Stage == "late"),
            by = c("Species", "Run", "Mass")) %>%
  is.na() %>%
  sum()

# For the bugula:
# Lots of NAs
bryozoan %>%
  filter(Species == "bugula",
         Stage == "early") %>%
  full_join(bryozoan %>%
              filter(Species == "bugula",
                     Stage == "late"),
            by = c("Species", "Run", "Mass")) %>%
  is.na() %>%
  sum()

# NAs are 4*number of early bugula. So what we 
# would expect if each mass was measured twice
bryozoan %>%
  filter(Species == "bugula",
         Stage == "early") %>%
  nrow()


# distributions of mass and metabolic rate
# (before fixing the error)
bryozoan %>%
  mutate(Stage = fct_relevel(Stage, "larvae", "early", "late")) %>%
  ggplot(aes(x = Species, y = Mass, color = Stage)) +
  geom_boxplot(lwd=0.7) +
  theme_bw() +
  labs(y = "Mass (micrograms)")

bryozoan %>%
  mutate(Stage = fct_relevel(Stage, "larvae", "early", "late")) %>%
  ggplot(aes(x = Species, y = Metabolic, color = Stage)) +
  geom_boxplot(lwd=0.7) +
  theme_bw() +
  labs(y = "Metabolic rate (mJ/hour)")


# fixing errors
bugula_early_mass <- bryozoan %>%
  filter(Species == "bugula",
         Stage == "early") %>%
  pull(Mass)

bryozoan$Mass[bryozoan$Mass < 1] <- bugula_early_mass


# relationship between mass and metabolic rate

bryozoan %>%
  mutate(Stage = fct_relevel(Stage, "larvae", "early", "late")) %>%
  ggplot(aes(x = Mass, y = Metabolic, color = Stage)) +
  geom_point() +
  geom_smooth(se=F, method="lm") +
  facet_wrap(~Species) +
  theme_bw()


# differences by run

bryozoan %>%
  mutate(Stage = fct_relevel(Stage, "larvae", "early", "late")) %>%
  ggplot(aes(x = as.factor(Run), y = Mass, 
             color = Species)) +
  geom_boxplot() +
  facet_wrap(~Stage)
