## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Sociodemographics table
##
## Author(s):         A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:      December 9th, 2022
##
## This version:      December 9th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0. Settings                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


eastCaribbean.ls <- c(
  "Antigua and Barbuda", 
  "Barbados",
  "Dominica",
  "Grenada",
  "St. Kitts and Nevis",
  "St. Lucia",
  "St. Vincent and the Grenadines",
  "Trinidad and Tobago"
)

westCaribbean_and_guianas.ls <- c(
  "Bahamas",
  "Dominican Republic",
  "Guyana",
  "Haiti",
  "Jamaica",
  "Suriname"
)

southCone.ls <- c(
  "Argentina",
  "Brazil",
  "Paraguay"
)

andeanCountries.ls <- c(
  "Bolivia",
  "Colombia",
  "Ecuador",
  "Peru"
)

centralAmerica.ls <- c(
  "Belize",
  "Costa Rica",
  "El Salvador",
  "Guatemala",
  "Honduras",
  "Nicaragua",
  "Panama"
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Andean Countries                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sociodem_andean <- master_data.df %>%
  filter(country %in% andeanCountries.ls) %>%
  filter(year == 2022) %>%
  group_by(country) %>%
  mutate(n_obs = n()) %>%
  ungroup() %>%
  mutate( white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, 1 , 
                                   if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, 0, NA_real_)),
          young         =  if_else(age < 31, 1, 
                                   if_else(age > 30, 0, NA_real_)),
          poor          =  if_else(fin == 1 | fin == 2, 1,
                                   if_else(fin == 3 | fin == 4 | fin == 5, 0, NA_real_)),
          area          =  if_else(Urban == 1, 1, 0, NA_real_),
          gender        =  if_else(gend == 1, 1, 
                                   if_else(gend == 2, 0, NA_real_)),
          diploma       =  if_else(edu == 4 | edu == 5 | edu == 6, 1, 
                                   if_else(edu < 5, 0, NA_real_)))

`Skin Tone` <- sociodem_andean %>%
  filter(white == 1 | white == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_skin_tone = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_skin_tone) %>%
  summarise(`Light Skin Tone` = round(mean(white, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Dark Skin Tone` = 1 - `Light Skin Tone`) %>%
  pivot_longer(cols = c(`Light Skin Tone`, `Dark Skin Tone`), names_to = "Skin Tone", values_to = "Proportion Skin Tone") %>%
  select(country, year, n_obs, `Skin Tone`, n_obs_skin_tone, `Proportion Skin Tone`) %>%
  mutate(`Proportion Skin Tone` = paste0(`Proportion Skin Tone`*100, "%"))
  
Age <- sociodem_andean %>%
  filter(young == 1 | young == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_age = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_age) %>%
  summarise(`Younger Than 30` = round(mean(young, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Older Than 30` = 1 - `Younger Than 30`) %>%
  pivot_longer(cols = c(`Younger Than 30`, `Older Than 30`), names_to = "Age", values_to = "Proportion Age") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, Age , n_obs_age, `Proportion Age`)  %>%
  mutate(`Proportion Age` = paste0(`Proportion Age`*100, "%"))


`Financial Situation` <- sociodem_andean %>%
  filter(poor == 1 | poor == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_fin = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_fin) %>%
  summarise(`Financially Insecure` = round(mean(poor, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Financially Secure` = 1 - `Financially Insecure`) %>%
  pivot_longer(cols = c(`Financially Insecure`, `Financially Secure`), names_to = "Financial Situation", values_to = "Proportion Financial Situation") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Financial Situation` , n_obs_fin, `Proportion Financial Situation`) %>%
  mutate(`Proportion Financial Situation` = paste0(`Proportion Financial Situation`*100, "%"))


Urban <- sociodem_andean %>%
  filter(area == 1 | area == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_area = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_area) %>%
  summarise(Urban = round(mean(area, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Rural = 1 - Urban) %>%
  pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "Proportion Area") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Area` , n_obs_area, `Proportion Area`) %>%
  mutate(`Proportion Area` = paste0(`Proportion Area`*100, "%"))

Gender <- sociodem_andean %>%
  filter(gender == 1 | gender == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_gender = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_gender) %>%
  summarise(Male = round(mean(gender, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Female = 1 - Male) %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Proportion Gender") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Gender` , n_obs_gender, `Proportion Gender`) %>%
  mutate(`Proportion Gender` = paste0(`Proportion Gender`*100, "%"))

`Education Level` <- sociodem_andean %>%
  filter(diploma == 1 | diploma == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_edu = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_edu) %>%
  summarise(`High School Diploma or more` = round(mean(diploma, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`No High School Diploma` = 1 - `High School Diploma or more`) %>%
  pivot_longer(cols = c(`No High School Diploma`, `High School Diploma or more`), names_to = "Education Level", values_to = "Proportion Education Level") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Education Level` , n_obs_edu, `Proportion Education Level`) %>%
  mutate(`Proportion Education Level` = paste0(`Proportion Education Level`*100, "%"))

sociodemographics_andean <- cbind(`Skin Tone`, Age, `Financial Situation`, Urban, Gender, `Education Level`)

sociodemographics_andean <- sociodemographics_andean %>%
  select(!ends_with("drop"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  South Cone Countries                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


sociodem_cone <- master_data.df %>%
  filter(country %in% southCone.ls) %>%
  # Latest year is different for Paraguay
  mutate(year = if_else(country == "Paraguay" & year == 2021, 2022, year)) %>%
  filter(year == 2022) %>%
  group_by(country) %>%
  mutate(n_obs = n()) %>%
  ungroup() %>%
  mutate( white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, 1 , 
                                   if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, 0, NA_real_)),
          young         =  if_else(age < 31, 1, 
                                   if_else(age > 30, 0, NA_real_)),
          poor          =  if_else(fin == 1 | fin == 2, 1,
                                   if_else(fin == 3 | fin == 4 | fin == 5, 0, NA_real_)),
          area          =  if_else(Urban == 1, 1, 0, NA_real_),
          gender        =  if_else(gend == 1, 1, 
                                   if_else(gend == 2, 0, NA_real_)),
          diploma       =  if_else(edu == 4 | edu == 5 | edu == 6, 1, 
                                   if_else(edu < 5, 0, NA_real_)))

`Skin Tone` <- sociodem_cone %>%
  filter(white == 1 | white == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_skin_tone = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_skin_tone) %>%
  summarise(`Light Skin Tone` = round(mean(white, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Dark Skin Tone` = 1 - `Light Skin Tone`) %>%
  pivot_longer(cols = c(`Light Skin Tone`, `Dark Skin Tone`), names_to = "Skin Tone", values_to = "Proportion Skin Tone") %>%
  select(country, year, n_obs, `Skin Tone`, n_obs_skin_tone, `Proportion Skin Tone`) %>%
  mutate(`Proportion Skin Tone` = paste0(`Proportion Skin Tone`*100, "%"))

skin_para1 <- c("Paraguay", 2022, 1000, "Light Skin Tone", NA_real_, NA_real_)
skin_para2 <- c("Paraguay", 2022, 1000, "Dark Skin Tone", NA_real_, NA_real_)

`Skin Tone` <- rbind(`Skin Tone`, skin_para1, skin_para2)

Age <- sociodem_cone %>%
  filter(young == 1 | young == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_age = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_age) %>%
  summarise(`Younger Than 30` = round(mean(young, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Older Than 30` = 1 - `Younger Than 30`) %>%
  pivot_longer(cols = c(`Younger Than 30`, `Older Than 30`), names_to = "Age", values_to = "Proportion Age") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, Age , n_obs_age, `Proportion Age`) %>%
  mutate(`Proportion Age` = paste0(`Proportion Age`*100, "%"))


`Financial Situation` <- sociodem_cone %>%
  filter(poor == 1 | poor == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_fin = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_fin) %>%
  summarise(`Financially Insecure` = round(mean(poor, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Financially Secure` = 1 - `Financially Insecure`) %>%
  pivot_longer(cols = c(`Financially Insecure`, `Financially Secure`), names_to = "Financial Situation", values_to = "Proportion Financial Situation") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Financial Situation` , n_obs_fin, `Proportion Financial Situation`) %>%
  mutate(`Proportion Financial Situation` = paste0(`Proportion Financial Situation`*100, "%"))


Urban <- sociodem_cone %>%
  filter(area == 1 | area == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_area = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_area) %>%
  summarise(Urban = round(mean(area, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Rural = 1 - Urban) %>%
  pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "Proportion Area") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Area` , n_obs_area, `Proportion Area`) %>%
  mutate(`Proportion Area` = paste0(`Proportion Area`*100, "%"))

Gender <- sociodem_cone %>%
  filter(gender == 1 | gender == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_gender = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_gender) %>%
  summarise(Male = round(mean(gender, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Female = 1 - Male) %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Proportion Gender") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Gender` , n_obs_gender, `Proportion Gender`) %>%
  mutate(`Proportion Gender` = paste0(`Proportion Gender`*100, "%"))

`Education Level` <- sociodem_cone %>%
  filter(diploma == 1 | diploma == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_edu = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_edu) %>%
  summarise(`High School Diploma or more` = round(mean(diploma, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`No High School Diploma` = 1 - `High School Diploma or more`) %>%
  pivot_longer(cols = c(`No High School Diploma`, `High School Diploma or more`), names_to = "Education Level", values_to = "Proportion Education Level") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Education Level` , n_obs_edu, `Proportion Education Level`) %>%
  mutate(`Proportion Education Level` = paste0(`Proportion Education Level`*100, "%"))

sociodemographics_cone <- cbind(`Skin Tone`, Age, `Financial Situation`, Urban, Gender, `Education Level`)

sociodemographics_cone <- sociodemographics_cone %>%
  select(!ends_with("drop"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  East Caribbean                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sociodem_east_car <- master_data.df %>%
  filter(country %in% eastCaribbean.ls) %>%
  filter(year == 2022) %>%
  group_by(country) %>%
  mutate(n_obs = n()) %>%
  ungroup() %>%
  mutate( white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, 1 , 
                                   if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, 0, NA_real_)),
          young         =  if_else(age < 31, 1, 
                                   if_else(age > 30, 0, NA_real_)),
          poor          =  if_else(fin == 1 | fin == 2, 1,
                                   if_else(fin == 3 | fin == 4 | fin == 5, 0, NA_real_)),
          area          =  if_else(Urban == 1, 1, 0, NA_real_),
          gender        =  if_else(gend == 1, 1, 
                                   if_else(gend == 2, 0, NA_real_)),
          diploma       =  if_else(edu == 4 | edu == 5 | edu == 6, 1, 
                                   if_else(edu < 5, 0, NA_real_)))

`Skin Tone` <- sociodem_east_car %>%
  filter(white == 1 | white == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_skin_tone = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_skin_tone) %>%
  summarise(`Light Skin Tone` = round(mean(white, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Dark Skin Tone` = 1 - `Light Skin Tone`) %>%
  pivot_longer(cols = c(`Light Skin Tone`, `Dark Skin Tone`), names_to = "Skin Tone", values_to = "Proportion Skin Tone") %>%
  select(country, year, n_obs, `Skin Tone`, n_obs_skin_tone, `Proportion Skin Tone`) %>%
  mutate(`Proportion Skin Tone` = paste0(`Proportion Skin Tone`*100, "%"))

Age <- sociodem_east_car %>%
  filter(young == 1 | young == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_age = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_age) %>%
  summarise(`Younger Than 30` = round(mean(young, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Older Than 30` = 1 - `Younger Than 30`) %>%
  pivot_longer(cols = c(`Younger Than 30`, `Older Than 30`), names_to = "Age", values_to = "Proportion Age") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, Age , n_obs_age, `Proportion Age`) %>%
  mutate(`Proportion Age` = paste0(`Proportion Age`*100, "%"))


`Financial Situation` <- sociodem_east_car %>%
  filter(poor == 1 | poor == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_fin = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_fin) %>%
  summarise(`Financially Insecure` = round(mean(poor, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Financially Secure` = 1 - `Financially Insecure`) %>%
  pivot_longer(cols = c(`Financially Insecure`, `Financially Secure`), names_to = "Financial Situation", values_to = "Proportion Financial Situation") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Financial Situation` , n_obs_fin, `Proportion Financial Situation`) %>%
  mutate(`Proportion Financial Situation` = paste0(`Proportion Financial Situation`*100, "%"))

Urban <- sociodem_east_car %>%
  filter(area == 1 | area == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_area = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_area) %>%
  summarise(Urban = round(mean(area, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Rural = 1 - Urban) %>%
  pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "Proportion Area") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Area` , n_obs_area, `Proportion Area`) %>%
  mutate(`Proportion Area` = paste0(`Proportion Area`*100, "%"))

Gender <- sociodem_east_car %>%
  filter(gender == 1 | gender == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_gender = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_gender) %>%
  summarise(Male = round(mean(gender, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Female = 1 - Male) %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Proportion Gender") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Gender` , n_obs_gender, `Proportion Gender`) %>%
  mutate(`Proportion Gender` = paste0(`Proportion Gender`*100, "%"))

`Education Level` <- sociodem_east_car %>%
  filter(diploma == 1 | diploma == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_edu = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_edu) %>%
  summarise(`High School Diploma or more` = round(mean(diploma, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`No High School Diploma` = 1 - `High School Diploma or more`) %>%
  pivot_longer(cols = c(`No High School Diploma`, `High School Diploma or more`), names_to = "Education Level", values_to = "Proportion Education Level") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Education Level` , n_obs_edu, `Proportion Education Level`) %>%
  mutate(`Proportion Education Level` = paste0(`Proportion Education Level`*100, "%"))

sociodemographics_east_car <- cbind(`Skin Tone`, Age, `Financial Situation`, Urban, Gender, `Education Level`)

sociodemographics_east_car <- sociodemographics_east_car %>%
  select(!ends_with("drop"))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  West Caribbean                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sociodem_west_car <- master_data.df %>%
  filter(country %in% westCaribbean_and_guianas.ls ) %>%
  filter(year == 2022) %>%
  group_by(country) %>%
  mutate(n_obs = n()) %>%
  ungroup() %>%
  mutate( white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, 1 , 
                                   if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, 0, NA_real_)),
          young         =  if_else(age < 31, 1, 
                                   if_else(age > 30, 0, NA_real_)),
          poor          =  if_else(fin == 1 | fin == 2, 1,
                                   if_else(fin == 3 | fin == 4 | fin == 5, 0, NA_real_)),
          area          =  if_else(Urban == 1, 1, 0, NA_real_),
          gender        =  if_else(gend == 1, 1, 
                                   if_else(gend == 2, 0, NA_real_)),
          diploma       =  if_else(edu == 4 | edu == 5 | edu == 6, 1, 
                                   if_else(edu < 5, 0, NA_real_)))

`Skin Tone` <- sociodem_west_car %>%
  filter(white == 1 | white == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_skin_tone = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_skin_tone) %>%
  summarise(`Light Skin Tone` = round(mean(white, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Dark Skin Tone` = 1 - `Light Skin Tone`) %>%
  pivot_longer(cols = c(`Light Skin Tone`, `Dark Skin Tone`), names_to = "Skin Tone", values_to = "Proportion Skin Tone") %>%
  select(country, year, n_obs, `Skin Tone`, n_obs_skin_tone, `Proportion Skin Tone`) %>%
  mutate(`Proportion Skin Tone` = paste0(`Proportion Skin Tone`*100, "%"))

Age <- sociodem_west_car %>%
  filter(young == 1 | young == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_age = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_age) %>%
  summarise(`Younger Than 30` = round(mean(young, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Older Than 30` = 1 - `Younger Than 30`) %>%
  pivot_longer(cols = c(`Younger Than 30`, `Older Than 30`), names_to = "Age", values_to = "Proportion Age") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, Age , n_obs_age, `Proportion Age`) %>%
  mutate(`Proportion Age` = paste0(`Proportion Age`*100, "%"))


`Financial Situation` <- sociodem_west_car %>%
  filter(poor == 1 | poor == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_fin = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_fin) %>%
  summarise(`Financially Insecure` = round(mean(poor, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`Financially Secure` = 1 - `Financially Insecure`) %>%
  pivot_longer(cols = c(`Financially Insecure`, `Financially Secure`), names_to = "Financial Situation", values_to = "Proportion Financial Situation") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Financial Situation` , n_obs_fin, `Proportion Financial Situation`) %>%
  mutate(`Proportion Financial Situation` = paste0(`Proportion Financial Situation`*100, "%"))


Urban <- sociodem_west_car %>%
  filter(area == 1 | area == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_area = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_area) %>%
  summarise(Urban = round(mean(area, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Rural = 1 - Urban) %>%
  pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "Proportion Area") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Area` , n_obs_area, `Proportion Area`) %>%
  mutate(`Proportion Area` = paste0(`Proportion Area`*100, "%"))


Gender <- sociodem_west_car %>%
  filter(gender == 1 | gender == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_gender = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_gender) %>%
  summarise(Male = round(mean(gender, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(Female = 1 - Male) %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Proportion Gender") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Gender` , n_obs_gender, `Proportion Gender`) %>%
  mutate(`Proportion Gender` = paste0(`Proportion Gender`*100, "%"))

`Education Level` <- sociodem_west_car %>%
  filter(diploma == 1 | diploma == 0) %>%
  group_by(country, year, n_obs) %>%
  mutate(n_obs_edu = n()) %>%
  ungroup() %>%
  group_by(country, year, n_obs, n_obs_edu) %>%
  summarise(`High School Diploma or more` = round(mean(diploma, na.rm = T),2)) %>%
  ungroup() %>%
  mutate(`No High School Diploma` = 1 - `High School Diploma or more`) %>%
  pivot_longer(cols = c(`No High School Diploma`, `High School Diploma or more`), names_to = "Education Level", values_to = "Proportion Education Level") %>%
  rename(country_drop = country, year_drop = year, n_obs_drop = n_obs) %>%
  select(country_drop, year_drop, n_obs_drop, `Education Level` , n_obs_edu, `Proportion Education Level`) %>%
  mutate(`Proportion Education Level` = paste0(`Proportion Education Level`*100, "%"))

sociodemographics_west_car <- cbind(`Skin Tone`, Age, `Financial Situation`, Urban, Gender, `Education Level`) 

sociodemographics_west_car <- sociodemographics_west_car %>%
  select(!ends_with("drop"))

sheets <- list("Andean" = sociodemographics_andean, "South Cone" = sociodemographics_cone, 
               "East Caribbean" = sociodemographics_east_car, "West Caribbean" = sociodemographics_west_car) 
write_xlsx(sheets, "Outputs/table_sociodemographics.xlsx")
