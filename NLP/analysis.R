# ---------------------------------------------------------------------------- #
#         Statistical analysis of the data collected in the experiment         #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

# ---------------------------------- Imports --------------------------------- #
library(tidyr)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gt)  
library(gtsummary)
library(survival)
library(car)
library(psych)
library(lsr)

# ---------------------------------- Methods --------------------------------- #
perc <- function(v) {
  return(round(v * 100, digits = 2))
}

calc_job_ads_vars <- function(df, prefix, signaling_value, sector_affiliaton, manipulated) {
  intention_to_apply_suffix <- paste(prefix, c("02_01", "02_02"), sep = "")
  intention_to_apply_var <- paste(prefix, "_ItA", sep = "")
  
  Public_values_suffix <- paste(prefix, c("03_01", "03_02", "03_03"), sep = "")
  Public_values_var <- paste(prefix, "_Public_values", sep = "")
  
  Private_values_suffix <- paste(prefix, c("03_04", "03_05", "03_06"), sep = "")
  Private_values_var <- paste(prefix, "_Private_values", sep = "")
  
  PJfit_suffix <- paste(prefix, c("04_01", "04_02", "04_03", "04_04"), sep = "")
  PJfit_var <- paste(prefix, "_PJfit", sep = "")
  
  Org_attr_suffix <- paste(prefix, c("05_01", "05_02", "05_03", "05_04", "05_05", "05_06", "05_07", "05_08", "05_09", "05_10"), sep = "")
  Org_attr_var <- paste(prefix, "_Org_attr", sep = "")
  
  Form_suffix <- paste(prefix, "06_01", sep = "")
  Form_var <- paste(prefix, "_Form", sep = "")
  
  # Reverse scale
  df[, paste(prefix, "05_02", sep="")] <- 6 - df[, paste(prefix, "05_02", sep="")]
  
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ItA = mean(c_across(all_of(intention_to_apply_suffix)), na.rm = TRUE)) %>%
    mutate(Public_values = mean(c_across(all_of(Public_values_suffix)), na.rm = TRUE)) %>%
    mutate(Private_values = mean(c_across(all_of(Private_values_suffix)), na.rm = TRUE)) %>%
    mutate(PJfit = mean(c_across(all_of(PJfit_suffix)), na.rm = TRUE)) %>%
    mutate(Org_attr = mean(c_across(all_of(Org_attr_suffix)), na.rm = TRUE)) %>%
    mutate(Form = mean(c_across(all_of(Form_suffix)), na.rm = TRUE)) %>%
    mutate(Signaling_value = signaling_value) %>%
    mutate(Sector_affiliation = sector_affiliaton) %>%
    mutate(Manipulated = manipulated) %>%
    ungroup() %>%
    select(-all_of(intention_to_apply_suffix)) %>%
    select(-all_of(Public_values_suffix)) %>%
    select(-all_of(Private_values_suffix)) %>%
    select(-all_of(PJfit_suffix)) %>%
    select(-all_of(Org_attr_suffix)) %>%
    select(-all_of(Form_suffix)) %>%
    rename_at(vars(ItA), ~intention_to_apply_var) %>%
    rename_at(vars(Public_values), ~Public_values_var) %>%
    rename_at(vars(Private_values), ~Private_values_var) %>%
    rename_at(vars(PJfit), ~PJfit_var) %>%
    rename_at(vars(Org_attr), ~Org_attr_var) %>%
    rename_at(vars(Form), ~Form_var) %>%
    rename_at(vars(Signaling_value), ~paste(prefix, "_Signalingvalue", sep = "")) %>%
    rename_at(vars(Sector_affiliation), ~paste(prefix, "_Sectoraffiliation", sep = "")) %>%
    rename_at(vars(Manipulated), ~paste(prefix, "_Manipulated", sep = ""))

  return(df)
}

# http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# ---------------------------------- GPower ---------------------------------- #
power_analysis <- read.csv("input/GPower.csv")
power_analysis <- pivot_longer(power_analysis,
             cols = c("Power_0.8", "Power_0.95"),
             values_to = "value")

ggplot(power_analysis, aes(y = value, x = `Effect.size`, colour = name)) + 
  geom_line() +
  geom_point() +
  ylab("Total sample Size") +
  xlab("Effect size") + 
  labs(colour="Power") +
  theme_bw()
ggsave("output/gpower.pdf", width = 15, height = 8, units = "cm")

# ------------------------------ Data cleansing ------------------------------ #
data <- read.csv("input/data_job_market_survey_2024-05-03_16-27.csv")

# Remove first entry (test)
data = data[-1,]

# Remove data privacy not accepted
data <- data[data$DS02 == 1, ]

# Remove not finished survey with reporting
not_finished <- data[data$LASTPAGE != 11, ]
n_not_finished <- nrow(not_finished)
finished <- data[data$LASTPAGE == 11, ]
n_finished <- nrow(finished)

cat("Finished: ", n_finished, ", Not finished: ", n_not_finished, " (", perc(n_not_finished/(n_finished + n_not_finished)), "%)", "\n", sep = "")
rm(data)

# Attention check, var = SP02_16
finished_ac_passed <- finished[finished$SP02_16 == 5, ]
ac_passed <- nrow(finished_ac_passed)
finished_ac_not_passed <- finished[finished$SP02_16 != 5, ]
ac_not_passed <- nrow(finished_ac_not_passed)

cat("Attention check passed: ", ac_passed, ", Not passed: ", ac_not_passed, " (", perc(ac_not_passed/(ac_passed + ac_not_passed)), "%)", "\n", sep = "")
rm(not_finished, finished, finished_ac_not_passed)

# Remove unnecessary columns
finished_ac_passed <- finished_ac_passed %>%
  select(-c('STARTED','CASE', 'SERIAL', 'DS02','REF', "QUESTNNR", "MODE", "SD07_CP", "SD07x01", "SD07x02", "SD07x03", "SD07x04", "SD07x05", "SD07x06", "SP02_16","FINISHED", "Q_VIEWER", "LASTPAGE", "MAXPAGE"))

# Renaming single value columns
finished_ac_passed <- finished_ac_passed %>%
  rename_at('SD02', ~'Age') %>%
  rename_at('SD03', ~'Gender') %>%
  rename_at('SD04', ~'Studyprogramme') %>%
  rename_at('SD05_01', ~'Semesterstudied') %>%
  rename_at('SD06_01', ~'Workexperience') %>%
  rename_at('SP01', ~'Sectorpreference') %>%
  rename_at('SD04_07', ~'Studyprogramme_text')

# Correct scaling of likert scale
finished_ac_passed[, c(1:138)] <- finished_ac_passed[, c(1:138)] - 1
finished_ac_passed[, c(146:159)] <- finished_ac_passed[, c(146:159)] - 1

J4_05 <- paste("J4", paste("05_", sprintf("%02d", 1:4), sep = ""), sep = "")
J4_04_new <- paste("J4", paste("04_", sprintf("%02d", 1:4), sep = ""), sep = "")

J4_06 <- paste("J4", paste("06_", sprintf("%02d", 1:10), sep = ""), sep = "")
J4_05_new <- paste("J4", paste("05_", sprintf("%02d", 1:10), sep = ""), sep = "")

J4_07 <- "J407_01"
J4_06_new <- "J406_01"

J6_07 <- paste("J6", paste("07_", sprintf("%02d", 1:2), sep = ""), sep = "")
J6_02_new <- paste("J6", paste("02_", sprintf("%02d", 1:2), sep = ""), sep = "")

J6_08 <- paste("J6", paste("08_", sprintf("%02d", 1:6), sep = ""), sep = "")
J6_03_new <- paste("J6", paste("03_", sprintf("%02d", 1:6), sep = ""), sep = "")

J6_09 <- paste("J6", paste("09_", sprintf("%02d", 1:4), sep = ""), sep = "")
J6_04_new <- paste("J6", paste("04_", sprintf("%02d", 1:4), sep = ""), sep = "")

J6_10 <- paste("J6", paste("10_", sprintf("%02d", 1:10), sep = ""), sep = "")
J6_05_new <- paste("J6", paste("05_", sprintf("%02d", 1:10), sep = ""), sep = "")

J6_11 <- "J611_01"
J6_06_new <- "J606_01"

finished_ac_passed <- finished_ac_passed %>%
  rename_at(vars(J4_05), ~J4_04_new) %>%
  rename_at(vars(J4_06), ~J4_05_new) %>%
  rename_at(vars(J4_07), ~J4_06_new) %>%
  rename_at(vars(J6_07), ~J6_02_new) %>%
  rename_at(vars(J6_08), ~J6_03_new) %>%
  rename_at(vars(J6_09), ~J6_04_new) %>%
  rename_at(vars(J6_10), ~J6_05_new) %>%
  rename_at(vars(J6_11), ~J6_06_new)

rm(J4_05, J4_04_new, J4_06, J4_05_new, J4_07, J4_06_new, J6_07, J6_02_new, J6_08, J6_03_new, J6_09, J6_04_new, J6_10, J6_05_new, J6_11, J6_06_new)

# Reverse PSM scales
finished_ac_passed$SP02_01 <- 6 - finished_ac_passed$SP02_01
finished_ac_passed$SP02_02 <- 6 - finished_ac_passed$SP02_02
finished_ac_passed$SP02_03 <- 6 - finished_ac_passed$SP02_03
finished_ac_passed$SP02_10 <- 6 - finished_ac_passed$SP02_10
finished_ac_passed$SP02_11 <- 6 - finished_ac_passed$SP02_11

# Compute PSM Dimensions
finished_ac_passed <- finished_ac_passed %>%
  mutate(APM = rowMeans(select(., SP02_01:SP02_03))) %>%
  select(-c("SP02_01", "SP02_02", "SP02_03")) %>%
  mutate(CPI = rowMeans(select(., SP02_04:SP02_07))) %>%
  select(-c("SP02_04", "SP02_05", "SP02_06", "SP02_07")) %>%
  mutate(COM = rowMeans(select(., SP02_08:SP02_12))) %>%
  select(-c("SP02_08", "SP02_09", "SP02_10", "SP02_11", "SP02_12")) %>%
  mutate(SS = rowMeans(select(., SP02_13:SP02_15))) %>%
  select(-c("SP02_13", "SP02_14", "SP02_15")) %>%
  mutate(PSM = rowMeans(select(., c("APM", "CPI", "COM", "SS"))))

# Compute Intention to apply for job ads
finished_ac_passed <- calc_job_ads_vars(finished_ac_passed, "J1", 4.232, "Public", FALSE)
finished_ac_passed <- calc_job_ads_vars(finished_ac_passed, "J2", 4.134, "Private", TRUE)
finished_ac_passed <- calc_job_ads_vars(finished_ac_passed, "J3", 1.694, "Public", FALSE)
finished_ac_passed <- calc_job_ads_vars(finished_ac_passed, "J4", -4.254, "Private", FALSE)
finished_ac_passed <- calc_job_ads_vars(finished_ac_passed, "J5", -3.965, "Public", TRUE)
finished_ac_passed <- calc_job_ads_vars(finished_ac_passed, "J6", -1.275, "Private", FALSE)

# Reshape the columns starting with "J"
final_data <- pivot_longer(finished_ac_passed,
                        cols = starts_with("J"),
                        names_sep = "_",
                        names_to = c("job ad", ".value"),
                        values_to = "value")

rm(finished_ac_passed)

# Variable replacement
final_data <- final_data %>%
  mutate(Age = case_when(
    Age == 1 ~ "Jünger als 15 Jahre",
    Age == 2 ~ "15 bis 19 Jahre",
    Age == 3 ~ "20 bis 24 Jahre",
    Age == 4 ~ "25 bis 29 Jahre",
    Age == 5 ~ "30 bis 34 Jahre",
    Age == 6 ~ "35 bis 39 Jahre",
    Age == 7 ~ "Älter als 39 Jahre",
    TRUE ~ "Unknown"
  ))

final_data <- final_data %>%
  mutate(Gender = case_when(
    Gender == 1 ~ "weiblich",
    Gender == 2 ~ "männlich",
    Gender == 3 ~ "divers",
    Gender == 4 ~ "keine Angabe",
    TRUE ~ "Unknown"
  ))

final_data <- final_data %>%
  mutate(Studyprogramme = case_when(
    Studyprogramme == 3 ~ "Politik, Verwaltung und Organisation (Bachelor) bzw. Verwaltungswissenschaft (Master)",
    Studyprogramme == 4 ~ "Politikwissenschaft / Internationale Beziehungen (Bachelor / Master)",
    Studyprogramme == 2 ~ "Betriebswirtschaftslehre (Bachelor / Master)",
    Studyprogramme == 5 ~ "Rechtswissenschaft (Erste juristische Prüfung / Bachelor)",
    Studyprogramme == 6 ~ "Soziologie (Bachelor / Master)",
    Studyprogramme == 1 ~ "Wirtschaftsinformatik (Bachelor / Master)",
    Studyprogramme == 7 ~ "Keiner der genannten Studiengänge",
    TRUE ~ "Unknown"
  ))

final_data <- final_data %>%
  mutate(Sectorpreference = case_when(
    Sectorpreference == 1 ~ "Public",
    Sectorpreference == 2 ~ "Private",
    TRUE ~ "Unknown"
  ))

final_data$Sectorpreferencemet <- final_data$Sectorpreference == final_data$Sectoraffiliation
final_data$PSM_factor <- ifelse(final_data$PSM < 3, "PSM_low", "PSM_high")
final_data$Workexperience <- as.numeric(gsub("," ,".", final_data$Workexperience))
final_data$Form <- as.numeric(final_data$Form)
final_data$Semesterstudied <- as.numeric(final_data$Semesterstudied)
# sapply(final_data, typeof)

# -------------------------------- Statistics -------------------------------- #
# Descriptives
final_data %>%
  filter(`job ad` == 'J1') %>%
  tbl_summary(include = c(Age, Gender, Studyprogramme, Semesterstudied, Workexperience, PSM),
              type = list(Age ~ "categorical", Gender ~ "categorical", Studyprogramme ~ "categorical", Semesterstudied ~ "continuous", Workexperience ~ "continuous", PSM ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              by = Sectorpreference,
              missing="no") %>%
              add_overall() %>%
  as_gt() %>%
  gtsave(filename = "output/descriptives.tex")

# Control measures
final_data %>%
  tbl_summary(
    include = c(Sectoraffiliation, Signalingvalue, Public, Private, PJfit, Org, Form),
    type = list(Form ~ "continuous", Sectoraffiliation ~ "categorical", Signalingvalue ~ "categorical"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    by = `job ad`,
    missing="no") %>%
  as_gt() %>%
  gtsave(filename = "output/controlmeasures.tex")

# Hypothesis 1 (without manipulation)
ggplot(final_data %>% filter(Manipulated == FALSE), aes(y = ItA, x = as.factor(Sectorpreferencemet))) + 
  geom_boxplot(width = 0.3) +
  ylim(0, 6) +
  ylab("Intention to apply") +
  xlab("Sector preference met") +
  theme_bw()
ggsave("output/secprefmet.pdf", width = 10, height = 8, units = "cm")

ItA_secprefmet <- final_data %>% filter(Manipulated == FALSE & Sectorpreferencemet == TRUE)
ItA_secprefnotmet <- final_data %>% filter(Manipulated == FALSE & Sectorpreferencemet == FALSE)

describe(ItA_secprefmet$ItA)
hist(ItA_secprefmet$ItA)
describe(ItA_secprefnotmet$ItA)
hist(ItA_secprefnotmet$ItA)

t.test(ItA_secprefmet$ItA, ItA_secprefnotmet$ItA, paired = TRUE, alternative = "greater")

# Hypothesis 2b-3:
datac <- summarySE(final_data, measurevar = "ItA", groupvars = c("Signalingvalue", "Sectoraffiliation", "Sectorpreference"))

plot_1 <- ggplot(datac %>% filter(Sectorpreference == 'Public'), aes(y = ItA, x = Signalingvalue, colour = Sectoraffiliation)) + 
  geom_errorbar(aes(ymin=ItA-se, ymax=ItA+se), width=.1) +
  geom_line() +
  geom_point() +
  ylim(0, 6) +
  xlim(-5, 5) +
  ylab("Intention to apply") +
  xlab("Signaling value") + 
  labs(colour="Sector affiliation of job ads") +
  ggtitle("Sector preference: Public") +
  theme_bw()

plot_2 <- ggplot(datac %>% filter(Sectorpreference == 'Private'), aes(y = ItA, x = Signalingvalue, colour = Sectoraffiliation)) + 
  geom_errorbar(aes(ymin=ItA-se, ymax=ItA+se), width=.1) +
  geom_line() +
  geom_point() +
  ylim(0, 6) +
  xlim(-5, 5) +
  ylab("Intention to apply") +
  xlab("Signaling value") + 
  labs(colour="Sector affiliation of job ads") +
  ggtitle("Sector preference: Private") +
  theme_bw()

plot_1 + plot_2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("output/secprefcompare.pdf", width = 15, height = 10, units = "cm")

ItA_SecPref.lm <- lm(ItA ~ Sectorpreference*Signalingvalue + Sectorpreference*Sectoraffiliation, final_data)
summary(ItA_SecPref.lm)

etaSquared(ItA_SecPref.lm)

# (1) Linearity
plot(ItA_SecPref.lm, 1)
# (2) Check for multicollinearity
vif(ItA_SecPref.lm) # VIF > 5 is a problem
# (3) Independence of residuals
durbinWatsonTest(ItA_SecPref.lm) # we want p-value > 0.05
# (4) Constant variability, heteroscedasticity
plot(ItA_SecPref.lm, 3)
ncvTest(ItA_SecPref.lm) # we want p-value > 0.05 
# (5) Nearly normal residuals
hist(residuals(ItA_SecPref.lm), col = "steelblue")

# Hypothesis 4:
datac_psm <- summarySE(final_data, measurevar = "ItA", groupvars = c("Signalingvalue", "Sectoraffiliation", "PSM_factor"))

plot_1_psm <- ggplot(datac_psm %>% filter(PSM_factor == 'PSM_high'), aes(y = ItA, x = Signalingvalue, colour = Sectoraffiliation)) + 
  geom_errorbar(aes(ymin=ItA-se, ymax=ItA+se), width=.1) +
  geom_line() +
  geom_point() +
  ylim(0, 6) +
  xlim(-5, 5) +
  ylab("Intention to apply") +
  xlab("Signaling value") + 
  labs(colour="Sector affiliation of job ads") +
  ggtitle("PSM: High") +
  theme_bw()

plot_2_psm <- ggplot(datac_psm %>% filter(PSM_factor == 'PSM_low'), aes(y = ItA, x = Signalingvalue, colour = Sectoraffiliation)) + 
  geom_errorbar(aes(ymin=ItA-se, ymax=ItA+se), width=.1) +
  geom_line() +
  geom_point() +
  ylim(0, 6) +
  xlim(-5, 5) +
  ylab("Intention to apply") +
  xlab("Signaling value") + 
  labs(colour="Sector affiliation of job ads") +
  ggtitle("PSM: Low") +
  theme_bw()

plot_1_psm + plot_2_psm + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("output/psmcompare.pdf", width = 15, height = 10, units = "cm")

ItA_PSM.lm <- lm(ItA ~ PSM*Signalingvalue + PSM*Sectoraffiliation, final_data)
summary(ItA_PSM.lm)

etaSquared(ItA_PSM.lm)

# (1) Linearity
plot(ItA_PSM.lm, 1)
# (2) Check for multicollinearity
vif(ItA_PSM.lm) # VIF > 5 is a problem
# (3) Independence of residuals
durbinWatsonTest(ItA_PSM.lm) # we want p-value > 0.05
# (4) Constant variability, heteroscedasticity
plot(ItA_PSM.lm, 3)
ncvTest(ItA_PSM.lm) # we want p-value > 0.05 
# (5) Nearly normal residuals
hist(residuals(ItA_PSM.lm), col = "steelblue")

# Testing APM (Korac, 2018)
ItA_APM.lm <- lm(ItA ~ APM*Signalingvalue + APM*Sectoraffiliation, final_data)
summary(ItA_APM.lm)

ItA_COM.lm <- lm(ItA ~ COM*Signalingvalue + COM*Sectoraffiliation, final_data)
summary(ItA_COM.lm)

ItA_CPI.lm <- lm(ItA ~ CPI*Signalingvalue + CPI*Sectoraffiliation, final_data)
summary(ItA_CPI.lm)

ItA_SS.lm <- lm(ItA ~ SS*Signalingvalue + SS*Sectoraffiliation, final_data)
summary(ItA_SS.lm)

# Output of LM (dual view)
t1 <- tbl_regression(ItA_SecPref.lm,
                     show_single_row = c("Sectorpreference", "Sectoraffiliation", "Sectorpreference:Signalingvalue", "Sectorpreference:Sectoraffiliation"),
                     intercept = TRUE,
                     conf.level = 0.95) %>%
  add_significance_stars() %>%
  add_glance_table(include = c(adj.r.squared, statistic))

t2 <- tbl_regression(ItA_PSM.lm,
                     show_single_row = c("PSM_factor", "Sectoraffiliation", "PSM_factor:Signalingvalue", "PSM_factor:Sectoraffiliation"),
                     intercept = TRUE,
                     conf.level = 0.95) %>%
  add_significance_stars() %>%
  add_glance_table(include = c(adj.r.squared, statistic))

tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Sector preference**", "**PSM**")
) %>%
modify_table_body(~.x %>% arrange(row_type == "glance_statistic")) %>%
as_gt() %>%
gtsave(filename = "output/lm.tex")

# Sector preference and PSM
final_data %>%
  filter(`job ad` == 'J1') %>%
  ggplot(aes(y = SS, x = as.factor(Sectorpreference))) + 
  geom_boxplot(width = 0.3) +
  ylim(0, 6) +
  ylab("PSM") +
  xlab("Sector preference") +
  theme_bw()
ggsave("output/secprefpsm.pdf", width = 10, height = 8, units = "cm")

PSM_public <- final_data %>% filter(`job ad` == 'J1' & Sectorpreference == 'Public')
PSM_private <- final_data %>% filter(`job ad` == 'J1' & Sectorpreference == 'Private')

describe(PSM_public$PSM)
hist(PSM_public$PSM)

describe(PSM_private$PSM)
hist(PSM_private$PSM)

mean(PSM_public$PSM) - mean(PSM_private$PSM)

t.test(PSM_public$PSM, PSM_private$PSM, alternative = "greater")

# PJ fit and org attr
PJ_org.lm <- lm(PJfit ~ Org, final_data)
summary(PJ_org.lm)


