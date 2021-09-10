library(readr)
library(tidyr)
library(dplyr)

metab <- read_csv("bryozoan_raw.csv")
metab_b <- metab[,1:4]
metab_w <- metab[,5:8]
colnames(metab_b) <- c("Stage", "Run", "Mass", "Metabolic")
colnames(metab_w) <- c("Stage", "Run", "Mass", "Metabolic")
metab_b <- metab_b %>%
  mutate(Species = "bugula")
metab_w <- metab_w %>%
  mutate(Species = "watersipora")
metab <- metab_b %>%
  rbind(metab_w) %>%
  arrange(Species, Run) %>%
  filter(Mass > 0) %>% # they have reporting errors in their data
                      # they've accidentally repeated some Metabolic measurements from
                      # early stage bugula as Mass measurements from late stage bugula
                      # I'm leaving it out for now, but we could include it if we want
                      # to include some sort of lesson on data errors
  mutate(Run = cumsum(c(1, diff(Run) != 0)),
         Stage = tolower(Stage),
         Mass = 10^Mass, 
         Metabolic = 10^Metabolic) %>%
  drop_na() %>%
  select(Species, Stage, Run, Mass, Metabolic)

write_csv(metab, "bryozoan_data.csv")

metab %>%
  ggplot(aes(x = Stage, y = Metabolic)) +
  geom_boxplot() +
  facet_wrap(~Species)

metab %>%
  ggplot(aes(x = Stage, y = Mass)) +
  geom_boxplot() +
  facet_wrap(~Species)


plot(metab$`BUGULA LOG10MASS`, metab$`BUGULA LOG10MR`)

metab %>%
  filter(`BUGULA STAGE` == "LARVAE") %>%
  ggplot(aes(x = `BUGULA LOG10MASS`, y = `BUGULA LOG10MR`)) +
  geom_point() +
  geom_smooth(method="lm", se=F)

metab %>%
  filter(`WATERSIPORA STAGE` == "LARVAE") %>%
  ggplot(aes(x = `WATERSIPORA LOG10MASS`, y = `WATERSIPORA LOG10MR`,
             color = as.factor(`WATERSIPORA RUN`))) +
  geom_point() +
  geom_smooth(method="lm", se=F)
