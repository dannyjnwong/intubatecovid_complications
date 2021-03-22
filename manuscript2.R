## ----setup, include=FALSE------------------------------------------------
library(tidyverse)
library(lubridate)
library(DiagrammeR) #Needs ver. 0.9.2 to work
library(DiagrammeRsvg)
library(finalfit)
library(tableone)
library(pander)
library(sjPlot)
library(cowplot)
library(lme4)
panderOptions('table.split.table', Inf)
knitr::opts_chunk$set(dpi = 300, echo = FALSE, warning = FALSE, message = FALSE)
options(digits = 4)

load("../data/intubatecovid_submission.RData")
#load("../data/intubatecovid.RData")

# Set the date of the latest data
datestamp <- file.info("../data/intubatecovid_20201024.RData")$mtime %>% date
#datestamp <- dmy("13/10/2020")

# Select the cases that fall under the dates
intubation <- intubation %>%
  filter(dmy_hm(Intubation.date) <= datestamp)

# Set a variable for LMIC status
LMICs <- c("India", "Mexico", "South Africa", "Pakistan", "Bolivia", "Panama", "Brasil", "Colombia", 
           "Ecuador", "Peru", "El Salvador", "Honduras", "Guatemala")

intubation <- intubation %>%
  mutate(LMIC = ifelse(Country %in% LMICs, "LMIC", "High-income"))

user <- user %>%
  mutate(LMIC = ifelse(Country %in% LMICs, "LMIC", "High-income"))

# Set a variable for first attempt success
intubation <- intubation %>%
  mutate(First.attempt.success = ifelse(Number.of.attempts < 2, TRUE, FALSE))

# Set a variable for failed intubation
intubation <- intubation %>%
  mutate(Failed.intubation = ifelse(Number.of.attempts >= 4 | 
                                      Final.airway.management.device == "Emergency front-of-neck airway" |
                                      Final.airway.management.device == "Supraglottic airway device", 
                                    TRUE, FALSE))

# Re-code the number of staff in the room to categorical
intubation <- intubation %>%
  mutate(Total.number.of.staff.intubation.room.cat = case_when(
    Total.number.of.staff.in.intubation.room == 1 ~ "1",
    Total.number.of.staff.in.intubation.room == 2 ~ "2",
    Total.number.of.staff.in.intubation.room == 3 ~ "3",
    Total.number.of.staff.in.intubation.room == 4 ~ "4",
    Total.number.of.staff.in.intubation.room >= 5 ~ "5+")) %>%
  # Re-code the small cell values into "other"
  mutate(Intubator.laryngoscopist = case_when(
    Intubator.laryngoscopist == "" ~ "Others",
    Intubator.laryngoscopist == "Other Nurse" ~ "Others",
    Intubator.laryngoscopist == "Others (please specify below)" ~ "Others",
    TRUE ~ as.character(Intubator.laryngoscopist)))

# Drop cases with missing hospital and country details
intubation <- intubation %>%
  filter(!is.na(Country)) %>%
  filter(!is.na(Institution.name)) %>%
  filter(!is.na(Institution.ID)) %>%
  filter(Institution.name != "") %>%
  filter(Country != "")

# Drop cases with erroneous/impossible age of intubator
intubation <- intubation %>%
  filter(Age < 100)

# STROBE table and exclusions
strobe_table <- data.frame(Description = "Total intubation episodes available", 
                           N = nrow(intubation))

# Remove episodes reported by assistants
intubation_clean <- intubation %>%
  filter(My.involvement == "Intubator/laryngoscopist")

strobe_table <- rbind(strobe_table, data.frame(Description = "Excluded: episodes reported by assistants",
                                               N = nrow(intubation_clean)))

# Remove elective tracheostomy insertions and GA for surgery etc.
intubation_clean <- intubation_clean %>%
  filter(Indication != "General anaesthetic for surgery")

strobe_table <- rbind(strobe_table, data.frame(Description = "Excluded: GA for surgery",
                                               N = nrow(intubation_clean)))

intubation_clean <- intubation_clean %>%
  filter(Indication != "Elective tracheostomy insertion") %>%
  filter(First.attempt.laryngoscopy.device != "Tracheostomy/front-of-neck airway ") %>%
  filter(Final.airway.management.device != "Elective tracheostomy")

strobe_table <- rbind(strobe_table, data.frame(Description = "Excluded: Elective tracheostomy insertions",
                                               N = nrow(intubation_clean)))

intubation_clean <- intubation_clean %>%
  filter(!(Indication %in% c("Tube exchange", "Other airway manipulation on ICU", 
                           "Tube exchange/other airway manipulation on ICU",
                           "Other indication")))

strobe_table <- rbind(strobe_table, data.frame(Description = "Excluded: Other ICU airway manipulations e.g. ETT exchanges",
                                               N = nrow(intubation_clean)))

strobe_table
intubation_clean$Indication %>% table()

# Modelling
intubation_clean <- intubation_clean %>% 
  mutate(COVID.status = Patient.s.COVID.19.status,
         Intubator = ifelse(Intubator.laryngoscopist == "Anaesthetic Doctor", "Anaesthetist", "Non-anaesthetist"),
         RSI = Rapid.sequence.induction..,
         Indication = factor(Indication, levels = c("Deteriorating respiratory failure",
                                                    "Cardiac arrest",
                                                    "Airway protection for low GCS")),
         Location = recode_factor(Location, `Labour ward` = "Other Hospital Location",
                           `Other` = "Other Hospital Location",
                           `General ward ` = "Other Hospital Location"),
         Male = ifelse(Gender == "Male", TRUE, FALSE), 
         First.Attempt.Device = factor(First.attempt.laryngoscopy.device, levels = c("Direct laryngoscope",
                                                                                     "Videolaryngoscope",
                                                                                     "Fibreoptic intubation")),
         Mask = case_when((FFP2 == TRUE | FFP3 == TRUE) & PAPR == FALSE ~ "Non-powered respirator only",
                          PAPR == TRUE ~ "PAPR",
                          SurgicalMask == FALSE & FFP2 == FALSE & FFP3 == FALSE & PAPR == FALSE ~ "No mask or surgical mask only",
                          SurgicalMask == TRUE & FFP2 == FALSE & FFP3 == FALSE & PAPR == FALSE ~ "No mask or surgical mask only"),
         Cons.Attending = ifelse(Grade == "Cons / SAS / Attending", TRUE, FALSE)) %>%
  mutate(Mask = factor(Mask, levels = c("Non-powered respirator only", "PAPR", "No mask or surgical mask only")))

# Create a variable for number of previous intubations prior to the current intubation episode (a measure of intubation experience)
intubation_number <- intubation_clean %>% 
  group_by(Account.ID, Intubation.date, Intubation.ID) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(Account.ID) %>% 
  mutate(Intubation.count = cumsum(n)-1) %>%
  ungroup() %>%
  select(Intubation.ID, Intubation.count)

intubation_clean <- intubation_clean %>%
  left_join(intubation_number, by = "Intubation.ID")

# Fix some of the variables for modelling
intubation_clean$First.attempt.success <- as.factor(intubation_clean$First.attempt.success)
intubation_clean$PlasticDrapeBox <- as.factor(intubation_clean$PlasticDrapeBox)
intubation_clean$Cons.Attending <- as.factor(intubation_clean$Cons.Attending)
intubation_clean$Age_rescaled <- scale(intubation_clean$Age)

# Construct mixed models with a random intercept for operator
model_null <- glmer(First.attempt.success ~ 
                      1 +
                      (1|Account.ID),
                    data = filter(intubation_clean, Age < 100), 
                    family = binomial(link = "logit"), 
                    control = glmerControl(optimizer = "bobyqa"))
model_LMIC <- glmer(First.attempt.success ~ 
                      LMIC +
                      (1|Account.ID),
                    data = filter(intubation_clean, Age < 100), 
                    family = binomial(link = "logit"), 
                    control = glmerControl(optimizer = "bobyqa"))
model_COVID.status <- glmer(First.attempt.success ~ 
                              COVID.status + 
                              (1|Account.ID),
                            data = filter(intubation_clean, Age < 100), 
                            family = binomial(link = "logit"), 
                            control = glmerControl(optimizer = "bobyqa"))
model_Location <- glmer(First.attempt.success ~ 
                          Location + 
                          (1|Account.ID),
                        data = filter(intubation_clean, Age < 100), 
                        family = binomial(link = "logit"), 
                        control = glmerControl(optimizer = "bobyqa"))
model_Indication <- glmer(First.attempt.success ~ 
                            Indication + 
                            (1|Account.ID),
                          data = filter(intubation_clean, Age < 100), 
                          family = binomial(link = "logit"), 
                          control = glmerControl(optimizer = "bobyqa"))
model_RSI <- glmer(First.attempt.success ~ 
                     RSI + 
                     (1|Account.ID),
                   data = filter(intubation_clean, Age < 100), 
                   family = binomial(link = "logit"), 
                   control = glmerControl(optimizer = "bobyqa"))
model_First.Attempt.Device <- glmer(First.attempt.success ~ 
                                      First.Attempt.Device + 
                                      (1|Account.ID),
                                    data = filter(intubation_clean, Age < 100), 
                                    family = binomial(link = "logit"), 
                                    control = glmerControl(optimizer = "bobyqa"))
model_Intubator <- glmer(First.attempt.success ~ 
                           Intubator + 
                           (1|Account.ID),
                         data = filter(intubation_clean, Age < 100), 
                         family = binomial(link = "logit"), 
                         control = glmerControl(optimizer = "bobyqa"))
model_PlasticDrapeBox <- glmer(First.attempt.success ~ 
                                 PlasticDrapeBox + 
                                 (1|Account.ID),
                               data = filter(intubation_clean, Age < 100), 
                               family = binomial(link = "logit"), 
                               control = glmerControl(optimizer = "bobyqa"))
model_Cons.Attending <- glmer(First.attempt.success ~ 
                                Cons.Attending + 
                                (1|Account.ID),
                              data = filter(intubation_clean, Age < 100), 
                              family = binomial(link = "logit"), 
                              control = glmerControl(optimizer = "bobyqa"))
model_Mask <- glmer(First.attempt.success ~ 
                      Mask + 
                      (1|Account.ID),
                    data = filter(intubation_clean, Age < 100), 
                    family = binomial(link = "logit"), 
                    control = glmerControl(optimizer = "bobyqa"))
model_Intubation.count <- glmer(First.attempt.success ~ 
                                  Intubation.count + 
                                  (1|Account.ID),
                                data = filter(intubation_clean, Age < 100), 
                                family = binomial(link = "logit"), 
                                control = glmerControl(optimizer = "bobyqa"))
model_Age_rescaled <- glmer(First.attempt.success ~ 
                              Age_rescaled + 
                              (1|Account.ID),
                            data = filter(intubation_clean, Age < 100), 
                            family = binomial(link = "logit"), 
                            control = glmerControl(optimizer = "bobyqa"))
model <- glmer(First.attempt.success ~ 
                 LMIC + 
                 #COVID.status + 
                 #Location + 
                 #Indication + 
                 RSI + 
                 First.Attempt.Device + 
                 #Intubator + 
                 #PlasticDrapeBox + 
                 Cons.Attending + 
                 Mask + 
                 Intubation.count + 
                 Age_rescaled + 
                 (1|Account.ID),
               data = filter(intubation_clean, Age < 100), 
               family = binomial(link = "logit"), 
               control = glmerControl(optimizer = "bobyqa"))

fit2df(model)

explanatory = c("LMIC", "COVID.status", "Location", "Indication", "RSI", "First.Attempt.Device", "Intubator", "PlasticDrapeBox", "Cons.Attending", "Mask", "Intubation.count", "Age_rescaled", "Age")
reduced_explanatoy = c("LMIC", "Indication", "RSI", "First.Attempt.Device", "Intubator", "Cons.Attending", "Mask", "Intubation.count", "Age_rescaled")
dependent = "First.attempt.success"
random_effect = "Account.ID"

intubation_clean %>% filter(Age < 100) %>% 
  mutate(First.attempt.success = as.factor(First.attempt.success)) %>%
  finalfit(dependent = dependent, 
           explanatory = explanatory, 
           explanatory_multi = c("LMIC", "Indication", "RSI", "First.Attempt.Device", "Intubator", "Cons.Attending", "Mask", "Intubation.count", "Age_rescaled"), 
           random_effect = random_effect) -> t4

#restricted_explanatory <- c("LMIC", "RSI", "First.Attempt.Device", "Mask", "Intubation.count")

#model <- glm(ff_formula(dependent, explanatory), data = intubation_clean, family = binomial)

#restricted_model <- update(model, . ~ . -COVID.status -Location -Indication -Intubator -PlasticDrapeBox -Age -Cons.Attending)

#restricted_model <- glm(ff_formula(dependent, restricted_explanatory), data = intubation_clean, family = binomial)

# Sensitivity analysis
model_First_Attempts <- glm(First.attempt.success ~ 
                 LMIC + 
                 #COVID.status + 
                 #Location + 
                 #Indication + 
                 RSI + 
                 First.Attempt.Device + 
                 #Intubator + 
                 #PlasticDrapeBox + 
                 Cons.Attending + 
                 Mask + 
                 #Intubation.count + 
                 Age_rescaled,
               data = filter(intubation_clean, Age < 100, Intubation.count == 0), 
               family = binomial(link = "logit"))

fit2df(model_First_Attempts) -> sens_table4


## ----fig_1_STROBE, echo=FALSE, message=FALSE, warning=FALSE, fig.height=6, fig.align='center', fig.cap='Figure 1: Flowchart of cases included and excluded from analysis.'----
#Set up the contents for each box
a1 <- paste0('Total available records\n',
             '(n = ',
             formatC(strobe_table$N[1], big.mark = ','), 
             ')')
b1 <- ''
c1 <- paste0('Records submitted by intubators\n(n = ',
             formatC(strobe_table$N[2], big.mark = ','),
             ')')
d1 <- ''
e1 <- paste0('Records included in analysis\n(n= ',
             formatC(strobe_table$N[5], big.mark = ','),
             ')')

a2 <- ''
b2 <- paste0('Excluded:\n',
             'Records submitted by assistants (n = ',
             formatC(strobe_table$N[1] - strobe_table$N[2], big.mark = ','),
             ')')
c2 <- ''
d2 <- paste0('Excluded:\n',
             'General anaesthetic for surgery (n = ',
             formatC(strobe_table$N[2] - strobe_table$N[3], big.mark = ','),
             ')\n',
             'Elective tracheostomy insertion (n = ',
             formatC(strobe_table$N[3] - strobe_table$N[4], big.mark = ','),
             ')\n',
             'Other ICU airway manipulations (n = ',
             formatC(strobe_table$N[4] - strobe_table$N[5], big.mark = ','),
             ')\n')
e2 <- ''

ndf <- create_node_df(
  n = 10,
  label = c(a1, b1, c1, d1, e1, #Column 1
            a2, b2, c2, d2, e2), #Column 2
  style = c('solid', 'invis', 'solid', 'invis', 'solid', #Column 1
            'invis', 'solid', 'invis', 'solid', 'invis'), #Column 2
  shape = c('box', 'point', 'box', 'point', 'box', #Column 1 
            'plaintext', 'box', 'point', 'box', 'point'), #Column 2
  width = c(3, 0.001, 3, 0.001, 3, #Column 1
            2, 5, 0.001, 5, 0.001), #Column 2
  height = c(1, 0.001, 1, 0.001, 1, #Column 1
             1, 1, 0.001, 1.5, 0.001), #Column 2
  fontsize = c(rep(14, 10)),
  fontname = c(rep('Helvetica', 10)),
  penwidth = 1.5,
  fixedsize = 'true')

edf <- create_edge_df(
  from = c(1, 2, 3, 4, #Column 1
           6, 7, 8, 9, #Column 2
           2, 4 #Horizontals
           ),
  to = c(2, 3, 4, 5, #Column 1
         7, 8, 9, 10, #Column 2
         7, 9 #Horizontals
         ),
  arrowhead = c('none', 'normal', 'none', 'normal', #Column 1
                'none', 'none', 'none', 'none', #Column 2
                'normal', 'normal' #Horizontals
                ),
  color = c('black', 'black', 'black', 'black', #Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', #Column 2
            'black', 'black' #Horizontals
            ),
  constraint = c(rep('true', 8), #Columns
                 rep('false', 2) #Horizontals
                 )
)
  
g <- create_graph(ndf,
                  edf,
                  attr_theme = NULL)

#render_graph(g)

export_graph(g, file_name = "../outputs/figures/Fig_1_strobe.png", width = 1200)

export_graph(g, file_name = "../outputs/figures/Fig_1_strobe.pdf")

knitr::include_graphics("../outputs/figures/Fig_1_strobe.png")


## ----fig2_map, echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.align='center', fig.cap='Figure 2: Countries with participants submitting data to the study, coloured by Organisation for Economic Co-operation and Development (OECD) income status.', dpi=600----
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

intubatecovid_countries <- intubation_clean %>% 
  group_by(Country, LMIC) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(LMIC = ifelse(LMIC == "LMIC", "Low- and middle-income", "High-income")) %>%
  rename(`OECD Income Level` = LMIC) %>%
  ungroup() %>%
  mutate(Country = recode(Country, `UK` = "United Kingdom",
                          `USA` = "United States of America",
                          `The Netherlands` = "Netherlands", 
                          `China` = "Hong Kong S.A.R.",
                          `Brasil` = "Brazil"))

subset_world <- world %>%
  filter(admin %in% intubatecovid_countries)

world <- world %>% left_join(intubatecovid_countries, by = c("admin" = "Country"))

ggplot(data = world) +
  geom_sf(aes(fill = `OECD Income Level`), col = "grey60", size = 0.05) +
  #geom_sf(data = subset_world) + 
  coord_sf(crs = '+proj=robin +ellps=WGS84 +datum=WGS84 +units=m +no_defs') +
  scale_fill_manual(values=c("#1967bb", "#8dd3c7"), 
                    name="OECD Income Level",
                    labels = c("High-income", "Low- and middle-income", "No data")) +
  theme_light() +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "../outputs/figures/fig2_map.pdf", device = cairo_pdf,
       width = 10, height = 6, units = "in", dpi = 600)


## ----table_1-------------------------------------------------------------
table_1 <- CreateTableOne(data = intubation_clean,
                          vars = c("Location",
                                   "Indication",
                                   "Total.number.of.staff.intubation.room.cat",
                                   "Intubator.laryngoscopist",
                                   "Airway.assistant",
                                   "Rapid.sequence.induction..",
                                   "First.attempt.laryngoscopy.device",
                                   "Apnoeic.oxygenation",
                                   "Was.bag.mask.ventilation.used.",
                                   "Mask"))

table_1a <- CreateTableOne(data = intubation_clean,
                           vars = c("Location",
                                    "Indication",
                                    "Total.number.of.staff.intubation.room.cat",
                                    "Intubator.laryngoscopist",
                                    "Airway.assistant",
                                    "Rapid.sequence.induction..",
                                    "First.attempt.laryngoscopy.device",
                                    "Apnoeic.oxygenation",
                                    "Was.bag.mask.ventilation.used.",
                                    "Mask"),
                           strata = "LMIC")

print(table_1, printToggle = FALSE, nonnormal = "Number.of.attempts") %>% 
  cbind(print(table_1a, printToggle = FALSE, nonnormal = "Number.of.attempts", test = TRUE)) %>%
  pander("Table 1: Tracheal Intubation characteristics. Data are frequency (%). LMIC = Low- and middle-income countries; ODP = operating department practictioner.")


## ----Fig3_PPE, echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=5, fig.align='center', fig.cap='Figure 3: Personal protective equipment used by operators during tracheal intubation episodes, stratified by OECD income status of the countries where the episode were reported from.', dpi=600----
intubation_clean %>%
    select(LMIC, Eyewear:None) %>%
    gather(key = "PPE", value = "value", -LMIC) %>%
    mutate(PPE = factor(PPE, c("None", "PlasticDrapeBox", "PAPR", "FFP3", "FFP2", "SurgicalMask", "Gloves", "Apron", "Gown", "Hat", "Eyewear"))) %>%
    mutate(PPE = recode(PPE,
                        `FFP2` = "Respirator Mask",
                        `FFP3` = "Respirator Mask",
                        `PlasticDrapeBox` = "Plastic Drape/Box",
                        `SurgicalMask` = "Surgical Mask",
                        `PAPR` = "Powered Air-Purifying\nRespirator")) %>%
    mutate(LMIC = ifelse(LMIC == "LMIC", "Low- and Middle-income", "High-income")) %>% 
  group_by(PPE, LMIC, value) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(key = "value", value = "n") %>% 
  mutate(Percent = `TRUE`/(`TRUE` + `FALSE`) * 100) %>%
  mutate(Percent = ifelse(LMIC == "High-income",
                          -Percent, Percent)) %>%
  ggplot(aes(x = PPE, y = Percent, fill = LMIC)) + 
  facet_wrap(~ LMIC, scales = "free_x") + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(breaks = seq(from = -100, to = 100, by = 20), 
                     labels = function(x) paste0(signif(abs(x), 3))) +
  scale_fill_manual(values = c("#1967bb", "#8dd3c7"),
                    name="OECD Income Level",
                    labels = c("High-income", "Low- and middle-income")) +
  labs(y = "Proportion (%)") +
  theme(panel.spacing.x = unit(-5, "mm")) +
  theme(legend.position = "none")

ggsave(filename = "../outputs/figures/fig3_ppe.pdf", device = cairo_pdf,
       width = 10, height = 5, units = "in", dpi = 600)


## ----table2--------------------------------------------------------------
user %>% filter(Account.ID %in% intubation_clean$Account.ID) %>%
  mutate(Male = ifelse(Gender == "Male", "Yes", "No")) %>%
  mutate(Specialty = recode(Specialty,
                            `Emergency Medicine` = "Other",
                            `ENT / H&N / Maxfax` = "Other",
                            `Respiratory Medicine` = "Other",
                            `Pre-Hospital Emergency Medicine` = "Other")) %>%
  mutate(Role = recode(Grade, 
                       `Cons / SAS / Attending` = "Physician (Consultant/Attending Grade)",
                       `Trainee grades` = "Physician (Training Grade)",
                       `Operating Department Practitioner (ODP)` = "Non-physician",
                       `Physician Associate (PA)` = "Non-physician",
                       `Certified Registered Nurse Anesthetist (CRNA)` = "Non-physician",
                       `Anaesthesia Associate (AA)` = "Non-physician",
                       `Advanced Critical Care Practitioner (ACCP)` = "Non-physician",
                       `Registered Nurse` = "Non-physician",
                       `Paramedic` = "Non-physician",
                       `Other` = "Non-physician")) %>%
  mutate(Country = recode(Country, 
                          `Argentina` = "Other",
                          `Bolivia` = "Other",
                          `Brasil` = "Other",
                          `China` = "Hong Kong S.A.R.",
                          `Colombia` = "Other",
                          `Ecuador` = "Other",
                          `El Salvador` = "Other",
                          `Guatemala` = "Other",
                          `Honduras` = "Other",
                          `Italy` = "Other",
                          `Mexico` = "Other",
                          `New Zealand` = "Other",
                          `Panama` = "Other",
                          `Paraguay` = "Other",
                          `Peru` = "Other",
                          `Singapore` = "Other",
                          `Uruguay` = "Other")) %>%
  CreateTableOne(data = .,
                 vars = c("Age",
                          "Male",
                          "Specialty",
                          "Role",
                          "Country",
                          "LMIC")) -> table_2

print(table_2, printToggle = FALSE, nonnormal = "Age") %>% 
  pander("Table 2: Participant characteristics. Data are frequency (%). Non-phyician roles include certified registered nurse anesthetists and other registered nursing professionals, operating department practitioners, physician associates, anesthesia associates, advanced critical care practitioners and paramedics. Countries with fewer than 10 participants have been recategorised as Other. These include Argentina, Bolivia, Brazil, Colombia, Ecuador, El Salvador, Guatemala, Honduras, Italy, Mexico, New Zealand, Panama, Paraguay, Peru, Singapore and Uruguay.")


## ----table_3-------------------------------------------------------------
intubation_clean %>% 
  mutate(Number.of.attempts = case_when(
    Number.of.attempts == 1 ~ "1",
    Number.of.attempts == 2 ~ "2",
    Number.of.attempts == 3 ~ "3",
    Number.of.attempts >= 4 ~ "4+")) %>%
  group_by(Number.of.attempts) %>% 
  summarise(n = n()) %>% 
  mutate(`%` = n/sum(n)*100) %>%
  rename(`Number of intubation attempts`= Number.of.attempts) %>%
  pander("Table 3: Number of tracheal intubation attempts.")


## ----table_4-------------------------------------------------------------
knitr::kable(t4, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r", "r"),
             caption = "Table 4: Factors associated with success at first intubation attempt. Data are reported as frequency (%), odds ratio (95% confidence interval, p-value) or mean (SD). OR = Odds Ratio; LMIC = Low- and middle-income country; GCS = Glasgow Coma Scale; SD = Standard Deviation; PAPR = Powered Air-Purifying Respirator.")
#knitr::kable(t3[[2]], row.names=FALSE, col.names="")


## ----Fig4_effects, echo=FALSE, message=FALSE, warning=FALSE, fig.width=6, fig.align='center', fig.cap='Figure 4: Plot of the association between number of previous COVID-19 tracheal intubations prior to current intubation episode and the likelihood of successfully intubating at first attempt (predicted success computed from the mixed-effects logistic regression model). In this plot the other model covariates are are set to the mean (for numeric variables), or set to their reference level (for categorical variables). The black line indicates the prediction estimate for a given number of prior COVID-19 tracheal intubations, and the grey area indicates the 95% confidence interval of the prediction estimate.', dpi=600----
plot_model(model, type = "pred", terms = "Intubation.count",
           title = "",
           digits = 0,
           axis.title = c("Prior COVID-19 tracheal intubations (n)", "First attempt success")) -> Fig4
Fig4 <- Fig4 + scale_y_continuous(limits = c(0, 1), labels = scales::percent)

ggsave(Fig4, filename = "../outputs/figures/fig4_effect.pdf", device = cairo_pdf,
       width = 8, height = 5, units = "in", dpi = 600)


## ------------------------------------------------------------------------
intubation_clean %>% group_by(Country, LMIC) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(LMIC = ifelse(LMIC == "LMIC", "Low- and middle-income", "High-income")) %>%
  rename(`OECD Income Level` = LMIC) %>%
  ungroup() %>%
  mutate(Country = recode(Country, `UK` = "United Kingdom",
         `USA` = "United States of America",
         `The Netherlands` = "Netherlands", 
         `China` = "Hong Kong S.A.R.",
         `Brasil` = "Brazil")) %>%
  pander("Countries contributing intubation records used analysis. OECD = Organisation for Economic Co-operation and Development; S. A. R. = Special Administrative Region.")


## ------------------------------------------------------------------------
intubation_clean %>% filter(Final.airway.management.device == "Emergency front-of-neck airway") %>%
  arrange(lubridate::dmy_hm(Intubation.date)) %>%
  select(Country,
         Grade,
         Specialty,
         Patient.s.COVID.19.status,
         Personal.Protective.Equipment.used,
         Location,
         Indication,
         Intubator.laryngoscopist,
         Airway.assistant,
         Total.number.of.staff.intubation.room.cat,
         First.attempt.laryngoscopy.device,
         Second.attempt.laryngoscopy.device..if.used.,
         Number.of.attempts,
         Was.a.supraglottic.airway.device.used.,
         Intubation.count) %>%
  pander()


## ------------------------------------------------------------------------
supp_table_3 <- CreateTableOne(data = intubation_clean,
                           vars = c("Location",
                                    "Indication",
                                    "Total.number.of.staff.intubation.room.cat",
                                    "Intubator.laryngoscopist",
                                    "Airway.assistant",
                                    "Rapid.sequence.induction..",
                                    "First.attempt.laryngoscopy.device",
                                    "Apnoeic.oxygenation",
                                    "Was.bag.mask.ventilation.used.",
                                    "Mask"),
                           strata = "Failed.intubation")

print(supp_table_3, printToggle = FALSE, nonnormal = "Number.of.attempts") %>%
  pander("Supplementary Table 3: Tracheal Intubation characteristics, stratified by whether the attempts resulted in a difficult or failed intubation (defined as 4 or more attempts, emergency front of neck airway or use of a supraglottic device as the final airway device). Data are frequency (%). LMIC = Low- and middle-income countries; ODP = operating department practictioner.")


## ------------------------------------------------------------------------
pander(sens_table4, "Supplementary Table 4: Sensitivity analysis. Fixed-effects only multivariable model of investigating the associations between successful first attempt at tracheal intubation and predictor variables, modelled in the subset of data from first recorded intubation. This analysis was performed to assess the effect of informative cluster size on the results of our main analysis.")

