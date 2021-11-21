library(readr)
scorecard <- read_csv("Most-Recent-Cohorts-Scorecard-Elements.csv")

scorecard <- scorecard %>%
  select(INSTNM, STABBR,
         CONTROL, SATVRMID, SATMTMID, 
         SATWRMID, ACTCMMID, ACTENMID,
         ACTMTMID, ACTWRMID,
         UGDS, NPT4_PUB, NPT4_PRIV, 
         PCTFLOAN, MD_EARN_WNE_P10, 
         GRAD_DEBT_MDN_SUPP, RPY_3YR_RT_SUPP) %>%
  mutate(NPT4 = ifelse(CONTROL == 1, NPT4_PUB, NPT4_PRIV)) %>%
  select(-c(NPT4_PUB, NPT4_PRIV))

scorecard <- scorecard %>%
  filter(!(INSTNM %in% c("NULL", "PrivacySuppressed")),
         !(STABBR %in% c("NULL", "PrivacySuppressed")),
         !(CONTROL %in% c("NULL", "PrivacySuppressed")),
         !(SATVRMID %in% c("NULL", "PrivacySuppressed")),
         !(SATMTMID %in% c("NULL", "PrivacySuppressed")),
         !(SATWRMID %in% c("NULL", "PrivacySuppressed")),
         !(ACTCMMID %in% c("NULL", "PrivacySuppressed")),
         !(ACTENMID %in% c("NULL", "PrivacySuppressed")),
         !(ACTMTMID %in% c("NULL", "PrivacySuppressed")),
         !(ACTWRMID %in% c("NULL", "PrivacySuppressed")),
         !(UGDS %in% c("NULL", "PrivacySuppressed")),
         !(NPT4 %in% c("NULL", "PrivacySuppressed")),
         !(PCTFLOAN %in% c("NULL", "PrivacySuppressed")),
         !(MD_EARN_WNE_P10 %in% c("NULL", "PrivacySuppressed")),
         !(GRAD_DEBT_MDN_SUPP %in% c("NULL", "PrivacySuppressed")),
         !(RPY_3YR_RT_SUPP %in% c("NULL", "PrivacySuppressed"))) %>%
  mutate(CONTROL = ifelse(CONTROL == 1, "public", "private"),
         RPY_3YR_70 = ifelse(RPY_3YR_RT_SUPP > 0.70, 1, 0))

scorecard[,4:17] <- apply(scorecard[,4:17], 2, as.numeric)


write_csv(scorecard, "scorecard.csv")
