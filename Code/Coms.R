## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Data Checks
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     April 3rd, 2023
##
## This version:      April 3rd, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

p_load(char = c(
  # Visualizations
  "showtext", "ggtext", "patchwork",
  
  # Data Loading
  "haven", "readxl", "xlsx",
  
  # Good 'ol Tidyverse
  "tidyverse", "writexl"
  
))

# Loading data
master_data.df <- bind_rows(read_dta("Data/LAC - Merged.dta"),
                            read_dta("Data/LAC - Merged (with CA).dta") %>% 
                              filter(year == 2022))

# Defining set of variables

media <- c("CAR_q60_G2", # Misinformation to shape public opinion in their favor
           "CAR_q64_G2", # Attack or discredit the media
           "q46e_G2" # The media  (TV, radio, newspapers) can freely expose cases of corruption
           )
elections <- c("CAR_q67_G2", # Attack or discredit the electoral system
               "CAR_q68_G2", # Manipulate the election process to win power
               "q46d_G1", # Local government officials are elected through a clean process
               "q46e_G1" #  People can vote freely without feeling harassed 
               )
crime <- c("q9", # How safe do you feel walking in your neighborhood at night?
           "EXP_q25b", # It is acceptable for people to take the law into their own hands
           "EXP_q25c", # It is pointless to hand over a criminal to the police
           "q49a_G1", # How confident you are that the criminal justice system as a whole
           "EXP_q24c_G2", # Victims of sexual crimes receive adequate care and protection
           "EXP_q24d_G2" # Victims of domestic violence receive adequate care and protection
           )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    1. Prepare Data                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

"%!in%" <- compose("!", "%in%")

data_subset.df <- master_data.df %>%
  # Latest year is different for Paraguay
  mutate(latestYear = if_else(country == "Paraguay", 2021, 2022)) %>%
  mutate(year = if_else(country == "Nicaragua" & year == 2021, NA_real_, year)) %>%
  filter(year == latestYear) %>%
  filter(country %!in% c("Venezuela", "Antigua and Barbuda", "St. Kitts and Nevis"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    2. Averages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Media
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Regional

# CAR_q60_G2: All countries with exception of Paraguay and Nicaragua
# CAR_q64_G2: All countries with exception of Paraguay, Suriname and Nicaragua
# q46e_G2: All countries

media.df <- data_subset.df %>%
  select(country, all_of(media)) %>%
  mutate(
    across(!country,
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_)))
  ) %>%
  summarise(across(!country,
                   mean,
                   na.rm = T)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", 
               values_to = "Regional Average") %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q60_G2" ~ "Top government officials of the national government use misinformation to shape public opinion in their favor",
             Variable == "CAR_q64_G2" ~ "Top government officials of the national government attack or discredit the media and civil society organizations that criticize them.",
             Variable == "q46e_G2" ~ "The media can freely expose cases of corruption by high-ranking government officers without fear of retaliation"
           ),
         `Regional Average` = paste0(round(`Regional Average`*100,0), "%"))

# Countries

mediaCountries.df <- data_subset.df %>%
  select(country, all_of(media)) %>%
  mutate(
    across(!country,
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_)))
  ) %>%
  group_by(country) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  pivot_longer(cols = !country, names_to = "Variable", 
               values_to = "Regional Average") %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q60_G2" ~ "Top government officials of the national government use misinformation to shape public opinion in their favor",
             Variable == "CAR_q64_G2" ~ "Top government officials of the national government attack or discredit the media and civil society organizations that criticize them.",
             Variable == "q46e_G2" ~ "The media can freely expose cases of corruption by high-ranking government officers without fear of retaliation"
           ),
         `Regional Average` = paste0(round(`Regional Average`*100,0), "%"))

# Gender

mediaGender.df <- data_subset.df %>%
  select(country, all_of(media), gend) %>%
  mutate(
    across(!c(country,gend),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    Gender = if_else(gend == 1, "Male", "Female")
  ) %>%
  select(!gend) %>%
  group_by(Gender) %>%
  summarise(across(!country,
                   mean,
                   na.rm = T)) %>%
  drop_na() %>%
  pivot_longer(cols = !Gender, names_to = "Variable", 
               values_to = "Regional Average") %>%
  pivot_wider(id_cols = Variable, names_from = Gender,
              values_from = `Regional Average`) %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q60_G2" ~ "Top government officials of the national government use misinformation to shape public opinion in their favor",
             Variable == "CAR_q64_G2" ~ "Top government officials of the national government attack or discredit the media and civil society organizations that criticize them.",
             Variable == "q46e_G2" ~ "The media can freely expose cases of corruption by high-ranking government officers without fear of retaliation"
           ),
         Female = paste0(round(Female*100,0), "%"),
         Male = paste0(round(Male*100,0), "%"))

# Gender Countries

mediaGenderCountries.df <- data_subset.df %>%
  select(country, all_of(media), gend) %>%
  mutate(
    across(!c(country, gend),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    Gender = if_else(gend == 1, "Male", "Female")
  ) %>%
  select(!gend) %>%
  group_by(country, Gender) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  drop_na() %>%
  pivot_longer(cols = !c(country, Gender), names_to = "Variable", 
               values_to = "Regional Average") %>%
  pivot_wider(id_cols = c(Variable, country), names_from = Gender,
              values_from = `Regional Average`) %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q60_G2" ~ "Top government officials of the national government use misinformation to shape public opinion in their favor",
             Variable == "CAR_q64_G2" ~ "Top government officials of the national government attack or discredit the media and civil society organizations that criticize them.",
             Variable == "q46e_G2" ~ "The media can freely expose cases of corruption by high-ranking government officers without fear of retaliation"
           ),
         Female = paste0(round(Female*100,0), "%"),
         Male = paste0(round(Male*100,0), "%")
         )

list_of_datasets <- list("Media" = media.df, 
                         "Media per Country" = mediaCountries.df,
                         "Media per Gender" = mediaGender.df,
                         "Media per Gender and Country" = mediaGenderCountries.df)

writexl::write_xlsx(list_of_datasets, path = "media.xlsx")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Elections
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Regional

# CAR_q67_G2: All countries with exception of Paraguay, Nicaragua and Suriname
# CAR_q68_G2: All countries with exception of Paraguay, Suriname and Nicaragua
# q46d_G1: All countries
# q46e_G1: All countries

elections.df <- data_subset.df %>%
  select(country, all_of(elections)) %>%
  mutate(
    across(!country,
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))
           )
    ) %>%
  summarise(across(!country,
                   mean,
                   na.rm = T)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", 
               values_to = "Regional Average") %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q67_G2" ~ "Attack or discredit the electoral system",
             Variable == "CAR_q68_G2" ~ "Manipulate the election process to win power",
             Variable == "q46d_G1" ~ "Local government officials are elected through a clean process",
             Variable == "q46e_G1" ~ "People can vote freely without feeling harassed"
           ),
         `Regional Average` = paste0(round(`Regional Average`*100,0), "%")
         )
  
# Countries

electionCountries.df <- data_subset.df %>%
  select(country, all_of(elections)) %>%
  mutate(
    across(!country,
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_)))
  ) %>%
  group_by(country) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T))  %>%
  pivot_longer(cols = !country, names_to = "Variable", 
               values_to = "Regional Average") %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q67_G2" ~ "Attack or discredit the electoral system",
             Variable == "CAR_q68_G2" ~ "Manipulate the election process to win power",
             Variable == "q46d_G1" ~ "Local government officials are elected through a clean process",
             Variable == "q46e_G1" ~ "People can vote freely without feeling harassed"
           ),
         `Regional Average` = paste0(round(`Regional Average`*100,0), "%"))

# Gender

electionsGender.df <- data_subset.df %>%
  select(country, all_of(elections), gend) %>%
  mutate(
    across(!c(country,gend),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    Gender = if_else(gend == 1, "Male", "Female")
  ) %>%
  select(!gend) %>%
  group_by(Gender) %>%
  summarise(across(!country,
                   mean,
                   na.rm = T)) %>%
  drop_na() %>%
  pivot_longer(cols = !Gender, names_to = "Variable", 
               values_to = "Regional Average") %>%
  pivot_wider(id_cols = Variable, names_from = Gender,
              values_from = `Regional Average`) %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q67_G2" ~ "Attack or discredit the electoral system",
             Variable == "CAR_q68_G2" ~ "Manipulate the election process to win power",
             Variable == "q46d_G1" ~ "Local government officials are elected through a clean process",
             Variable == "q46e_G1" ~ "People can vote freely without feeling harassed"
           ),
         Female = paste0(round(Female*100,0), "%"),
         Male = paste0(round(Male*100,0), "%"))

# Gender Countries

electionsGenderCountries.df <- data_subset.df %>%
  select(country, all_of(elections), gend) %>%
  mutate(
    across(!c(country, gend),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    Gender = if_else(gend == 1, "Male", "Female")
  ) %>%
  select(!gend) %>%
  group_by(country, Gender) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  drop_na() %>%
  pivot_longer(cols = !c(country, Gender), names_to = "Variable", 
               values_to = "Regional Average") %>%
  pivot_wider(id_cols = c(Variable, country), names_from = Gender,
              values_from = `Regional Average`) %>%
  mutate(Variable = 
           case_when(
             Variable == "CAR_q67_G2" ~ "Attack or discredit the electoral system",
             Variable == "CAR_q68_G2" ~ "Manipulate the election process to win power",
             Variable == "q46d_G1" ~ "Local government officials are elected through a clean process",
             Variable == "q46e_G1" ~ "People can vote freely without feeling harassed"
           ),
         Female = paste0(round(Female*100,0), "%"),
         Male = paste0(round(Male*100,0), "%")
  )

list_of_datasets <- list("Elections" = elections.df, 
                         "Elections per Country" = electionCountries.df,
                         "Elections per Gender" = electionsGender.df,
                         "Elections per Gender per Country" = electionsGenderCountries.df)

writexl::write_xlsx(list_of_datasets, path = "elections.xlsx")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Crime
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Regional

#q9: All countries
#EXP_q25b: Only for Belize, El Salvador, Guatemala, Honduras, Panama
#EXP_q25c: Only for Belize, El Salvador, Guatemala, Honduras, Panama
#q49a_G1: Only for Belize, El Salvador, Guatemala, Honduras, Panama
#EXP_q24c_G2: All countries with exception of Paraguay, Nicaragua
#EXP_q24d_G2: All countries with exception of Paraguay, Nicaragua

crime.df <- data_subset.df %>%
  select(country, all_of(crime)) %>%
  mutate(
    across(!country,
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))
    )
  ) %>%
  summarise(across(!country,
                   mean,
                   na.rm = T)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", 
               values_to = "Regional Average") %>%
  mutate(Variable = 
           case_when(
             Variable == "q9" ~ "Safety walking in neighborhood at night",
             Variable == "EXP_q25b" ~ "It is acceptable for people to take the law into their own hands",
             Variable == "EXP_q25c" ~ "It is pointless to hand over a criminal to the police",
             Variable == "q49a_G1" ~ "The criminal justice system is effective in bringing people who commit crimes to justice",
             Variable == "EXP_q24c_G2" ~ "Victims of sexual crimes receive adequate care and protection",
             Variable == "EXP_q24d_G2" ~ "Victims of domestic violence receive adequate care and protection"
           ),
         `Regional Average` = paste0(round(`Regional Average`*100,0), "%"))
  

# Countries
  
crimeCountries.df <- data_subset.df %>%
  select(country, all_of(crime)) %>%
  mutate(
    across(!country,
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))
    )
  ) %>%
  group_by(country) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  pivot_longer(cols = !country, names_to = "Variable", 
               values_to = "Regional Average") %>%
  mutate(Variable = 
           case_when(
             Variable == "q9" ~ "Safety walking in neighborhood at night",
             Variable == "EXP_q25b" ~ "It is acceptable for people to take the law into their own hands",
             Variable == "EXP_q25c" ~ "It is pointless to hand over a criminal to the police",
             Variable == "q49a_G1" ~ "The criminal justice system is effective in bringing people who commit crimes to justice",
             Variable == "EXP_q24c_G2" ~ "Victims of sexual crimes receive adequate care and protection",
             Variable == "EXP_q24d_G2" ~ "Victims of domestic violence receive adequate care and protection"
           ),
         `Regional Average` = paste0(round(`Regional Average`*100,0), "%")) 

# Gender

crimeGender.df <- data_subset.df %>%
  select(country, all_of(crime), gend) %>%
  mutate(
    across(!c(country,gend),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    Gender = if_else(gend == 1, "Male", "Female")
  ) %>%
  select(!gend) %>%
  group_by(Gender) %>%
  summarise(across(!country,
                   mean,
                   na.rm = T)) %>%
  drop_na() %>%
  pivot_longer(cols = !Gender, names_to = "Variable", 
               values_to = "Regional Average") %>%
  pivot_wider(id_cols = Variable, names_from = Gender,
              values_from = `Regional Average`) %>%
  mutate(Variable = 
           case_when(
             Variable == "q9" ~ "Safety walking in neighborhood at night",
             Variable == "EXP_q25b" ~ "It is acceptable for people to take the law into their own hands",
             Variable == "EXP_q25c" ~ "It is pointless to hand over a criminal to the police",
             Variable == "q49a_G1" ~ " The criminal justice system is effective in bringing people who commit crimes to justice",
             Variable == "EXP_q24c_G2" ~ "Victims of sexual crimes receive adequate care and protection",
             Variable == "EXP_q24d_G2" ~ "Victims of domestic violence receive adequate care and protection"
           ),
         Female = paste0(round(Female*100,0), "%"),
         Male = paste0(round(Male*100,0), "%"))

# Gender Countries

crimeGenderCountries.df <- data_subset.df %>%
  select(country, all_of(crime), gend) %>%
  mutate(
    across(!c(country, gend),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    Gender = if_else(gend == 1, "Male", "Female")
  ) %>%
  select(!gend) %>%
  group_by(country, Gender) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  drop_na() %>%
  pivot_longer(cols = !c(country, Gender), names_to = "Variable", 
               values_to = "Regional Average") %>%
  pivot_wider(id_cols = c(Variable, country), names_from = Gender,
              values_from = `Regional Average`) %>%
  mutate(Variable = 
           case_when(
             Variable == "q9" ~ "Safety walking in neighborhood at night",
             Variable == "EXP_q25b" ~ "It is acceptable for people to take the law into their own hands",
             Variable == "EXP_q25c" ~ "It is pointless to hand over a criminal to the police",
             Variable == "q49a_G1" ~ " The criminal justice system is effective in bringing people who commit crimes to justice",
             Variable == "EXP_q24c_G2" ~ "Victims of sexual crimes receive adequate care and protection",
             Variable == "EXP_q24d_G2" ~ "Victims of domestic violence receive adequate care and protection"
           ),
         Female = paste0(round(Female*100,0), "%"),
         Male = paste0(round(Male*100,0), "%")
  )

list_of_datasets <- list("Crime" = crime.df, 
                         "Crime per Country" = crimeCountries.df,
                         "Crime per Gender" = crimeGender.df,
                         "Crime per Gender per Country" = crimeGenderCountries.df)

writexl::write_xlsx(list_of_datasets, path = "crime.xlsx")
