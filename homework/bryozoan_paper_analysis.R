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
  ggplot(aes(x = as.factor(Run), y = Metabolic, 
             color = Species)) +
  geom_boxplot() +
  facet_wrap(~Stage) +
  theme_bw() +
  labs(x = "Run", y = "Metabolic rate (mJ/hour)")


# filtering

bugula_early <- bryozoan %>%
  filter(Species == "bugula",
         Stage == "early")

be_lm <- lm(Metabolic ~ Mass, data = bugula_early)

bugula_early %>%
  mutate(residuals = residuals(be_lm),
         predicted = predict(be_lm)) %>%
  ggplot(aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0, 
              color="blue", lwd=1.2) +
  labs(x = "Predicted metabolic rate (mJ/hour)",
       y = "Residuals")

bugula_early %>%
  mutate(residuals = residuals(be_lm)) %>%
  ggplot(aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  labs(x = "Theoretical normal quantiles",
       y = "Observed residual quantiles") +
  theme_bw()


summary(be_lm)

# confidence interval for the slope

0.006427 - qt(0.025, 195, lower.tail=F)*0.0013
0.006427 + qt(0.025, 195, lower.tail=F)*0.0013


# log transformations

bugula_early <- bugula_early %>%
  mutate(log_mass = log(Mass),
         log_metabolic = log(Metabolic))


be_lm <- lm(log_metabolic ~ log_mass,
            data = bugula_early)

summary(be_lm)

# confidence interval

0.5991 - qt(0.025, 195, lower.tail=F)*0.1162
0.5991 + qt(0.025, 195, lower.tail=F)*0.1162

# p-value
(0.5991 - 1)/0.1162
pt(-3.45, 195)



# Multiple regression

bryozoan <- bryozoan %>%
  mutate(log_mass = log(Mass),
         log_metabolic = log(Metabolic))

bryozoan_larvae_early <- bryozoan %>%
  filter(Stage != "late")

bryozoan_larvae_early %>%
  mutate(Stage = fct_relevel(Stage, "larvae", "early")) %>%
  ggplot(aes(x = log_mass, 
             y = log_metabolic,
             color = Stage)) +
  geom_point() +
  geom_smooth(se=F, method="lm") +
  facet_wrap(~Species) +
  theme_bw() +
  labs(x = "log(Mass)",
       y = "log(Metabolic rate)")

bryozoan_larvae_early <- bryozoan_larvae_early %>%
  mutate(Stage = fct_relevel(Stage, "larvae", "early"))


ble_lm <- lm(log_metabolic ~ Stage + Species + log_mass, 
             data = bryozoan_larvae_early)

# residual plot without interaction term

bryozoan_larvae_early %>%
  mutate(pred = predict(ble_lm),
         resid = residuals(ble_lm)) %>%
  ggplot(aes(x = pred, y = resid,
             color = Species)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0,
              color = "blue", lwd = 1.2) +
  labs(x = "Predicted log(metabolic rate)",
       y = "Residuals") +
  theme_bw()

ble_lm <- lm(log_metabolic ~ Stage*Species + log_mass, 
             data = bryozoan_larvae_early)

bryozoan_larvae_early %>%
  mutate(pred = predict(ble_lm),
         resid = residuals(ble_lm)) %>%
  ggplot(aes(x = pred, y = resid,
             color = Species)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0,
              color = "blue", lwd = 1.2) +
  labs(x = "Predicted log(metabolic rate)",
       y = "Residuals") +
  theme_bw()


# confidence interval

0.577 - qt(0.025, 563, lower.tail=F)*0.073
0.577 + qt(0.025, 563, lower.tail=F)*0.073

# test

pt((0.577 - 1)/0.073, 563)


# does the slope vary? Answer appears to be no

ble_lm_full <- lm(log_metabolic ~ Stage*Species*log_mass, 
                  data = bryozoan_larvae_early)

anova(ble_lm, ble_lm_full)



# Mixed effects models


ble_lme <- lmer(log_metabolic ~ (1|Run) + Stage*Species + 
                  log_mass, data = bryozoan_larvae_early)


# checking assumptions

data.frame(sample = coef(ble_lme)$Run[,1]) %>% 
  ggplot(aes(sample = sample)) + 
  geom_qq() + 
  geom_qq_line()


# do we need any random slopes?
# Note that in this case, we have a pretty simple design

ble_lme_2 <- lmer(log_metabolic ~ Stage*Species + 
                  log_mass + (log_mass|Run), data = bryozoan_larvae_early)


anova(ble_lme, ble_lme_2)


# Now what if we wanted to include the late-stage 
# bugula in our model too? 
# One way to do that could be to include a random effect 
# term for each individual

# first we need to make a term for the individual id

bryozoan_id <- bryozoan %>%
  filter(Stage != "larvae") %>%
  arrange(Species, Run, Mass, Stage) %>%
  mutate(id = cumsum(c(1, diff(Mass) != 0)) + 
           nrow(bryozoan %>% filter(Stage == "larvae"))) %>%
  rbind(bryozoan %>%
          filter(Stage == "larvae") %>%
          mutate(id = 1:n())) %>%
  arrange(Species, Run, Stage)

max(bryozoan_id$id)
max(table(bryozoan_id$id))


bryozoan_lme <- lmer(log_metabolic ~ (1|Run/id) + 
                       Stage*Species + log_mass, 
                     data = bryozoan_id)

bryozoan_lme_full <- lmer(log_metabolic ~ (1|Run/id) + 
                       Stage*Species + Stage*log_mass, 
                     data = bryozoan_id)

anova(bryozoan_lme, bryozoan_lme_full)



bryozoan %>%
  mutate(Stage = fct_relevel(Stage, "larvae", "early", "late")) %>%
  ggplot(aes(x = log_mass, 
             y = log_metabolic,
             color = Stage)) +
  geom_point() +
  geom_smooth(se=F, method="lm") +
  facet_wrap(~Species) +
  theme_bw() +
  labs(x = "log(Mass)",
       y = "log(Metabolic rate)")
