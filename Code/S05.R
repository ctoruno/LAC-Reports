## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Section V Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Jeison Sabogal              (jeison.sabogal@gmail.com)
##
## Dependencies:      World Justice Project
##
## Creation date:     February 6th, 2022
##
## This version:      February 6th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 19                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure19A.fn(nchart = 19) {
  
  # Defining country parameters
  if (mainCountry == "Belize") {
    shpACRON  <- "BLZ"
    targetVar <- "city"
  }
  if (mainCountry == "El Salvador") {
    shpACRON  <- "SLV"
    targetVar <- "PSU"
  }
  if (mainCountry == "Guatemala") {
    shpACRON  <- "GTM"
    targetVar <- "PSU" 
  }
  if (mainCountry == "Honduras") {
    shpACRON  <- "HND"
    targetVar <- "city"
  }
  if (mainCountry == "Panama") {
    shpACRON  <- "PAN"
    targetVar <- "PSU"
  }

}

figure19B.fn(nchart = 19) {
  
  migrated <- LAC_Merged_draft %>%
    filter(country %in% "Guatemala") %>%
    filter(year == 2022) %>%
    mutate(migrated     = if_else(EXP_q31a == 0, 1, 
                                  if_else(EXP_q31a == 1, 0, NA_real_)),
           migrated3yrs = if_else(EXP22_q26b == 1, 1,
                                  if_else(EXP22_q26b == 0, 0, NA_real_)),
           white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, "White" , 
                                    if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, "No White", NA_character_)),
           young         =  if_else(age < 31, "Less than 30 years", 
                                    if_else(age > 30, "More than 30 years", NA_character_)),
           poor          =  if_else(fin == 1 | fin == 2, "Poor",
                                    if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
           area          =  if_else(Urban == 1, "Urban", "Rural"),
           gender        =  if_else(gend == 1, "Male", "Female"),
           diploma       =  if_else(edu == 4 | edu == 5 | edu == 6, "High Education Level", 
                                    if_else(edu < 5, "No High Education Level", NA_character_))
           )
  
  condition <- migrated  %>%
    filter(migrated == 1) %>%
    select(white, young, poor, area, gender, diploma) %>%
    mutate(counter = 1)
  
  color   <- condition_categories(main_data = condition, group_var = white, name_var = "white")
  age     <- condition_categories(main_data = condition, group_var = young, name_var = "young")
  income  <- condition_categories(main_data = condition, group_var = poor, name_var = "poor")
  area    <- condition_categories(main_data = condition, group_var = area, name_var = "area")
  gender  <- condition_categories(main_data = condition, group_var = gender, name_var = "gender")
  diploma <- condition_categories(main_data = condition, group_var = diploma, name_var = "diploma")
  
  selectables <- rbind(color, age, income, area, gender, diploma) %>%
    group_by(variable) %>%
    summarise(min_group = min(N_obs, na.rm = T),
              total_group = sum(N_obs, na.rm = T)) %>%
    filter(min_group > 30) %>%
    filter(min_group != total_group) %>%
    pull(variable)

  logit_demo <- function(mainData, 
                         Yvar) {
    
    data_logit <- mainData %>%
      mutate(Yvar = all_of({{Yvar}}))
    
    logit_data <- data_logit %>%
      select(Yvar, all_of(selectables)) %>%
      rowid_to_column("id") %>%
      pivot_longer(cols = !c(Yvar, id), names_to = "categories", values_to = "values") %>%
      mutate(values = if_else(categories %in% "young" & values %in% "More than 30 years", "1More than 30 years", values),
             values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values)) %>%
      pivot_wider(id_cols = c(Yvar, id), names_from = categories, values_from = values)
    
    logit_data<- logit_data %>%
      select(all_of(selectables),
             Yvar) # We didn't include the non answer
    
    formula <- selectables %>%
      t() %>%
      as.data.frame() %>%
      unite(., formula, sep = "+") %>%
      as.character()
    
    models <- lapply(list("Yvar"), 
                     function(depVar) {
                       formula  <- as.formula(paste(depVar, "~", formula))
                       logit  <- glm(formula,  
                                     data   = logit_data, 
                                     family = "binomial")})
    margEff    <- margins_summary(models[[1]], data = models[[1]]$model)
    
    data2plot <- margEff
    
    data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \nInsecure", "victimVictim" = "Previous Crime \nVictimization",
                               "areaUrban" = "Urban", "whiteWhite" = "Light Skin \nTone", "youngLess than 30 years" = "Younger than 30",
                               "diplomaNo High Education Level" = "No High School \nDiploma")
    
    data2plot <- data2plot %>%
      mutate(category = "Colombia",
             order_variable = if_else(factor %in% "Female", 1,
                                      if_else(factor %in% "White", 2,
                                              if_else(factor %in% "Poor", 3,
                                                      if_else(factor %in% "Victim", 4,
                                                              if_else(factor %in% "Urban", 5, 
                                                                      if_else(factor %in% "Young", 6, 7)))))))
  }
  
  # Panel 1
  
  data2plot_P1 <- logit_demo(mainData = migrated, Yvar = migrated)
  logit_plot <- logit_demo_panel(mainData = data2plot_P1, line_size = 1.5)
  
  # Panel 2
  
  data2plot_P2 <- logit_demo(mainData = migrated, Yvar = migrated3yrs)
  logit_plot <- logit_demo_panel(mainData = data2plot_P2, line_size = 1.5)
  
  }


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 20                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure20A.fn(nchart = 20) {
  
  # CARLOS!!!!
  
}

figure20B.fn(nchart = 20) {
  
  # CARLOS!!!!
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 22                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure22A.fn(nchart = 22) {
  
  # JEISON!!!!
  
}

figure22B.fn(nchart = 22) {

  data2plot <- data_subset.df %>%
    filter(country %in% mainCountry) %>%
    filter(year == 2022) %>%
    select(country,EXP22_q27g_1, EXP22_q27g_2, EXP22_q27g_3, EXP22_q27g_4, EXP22_q27g_5, EXP22_q27g_other) %>%
    group_by(country) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(cols = !country,
                 names_to   = "category", 
                 values_to  = "value2plot") %>%
    mutate(value2plot  = round(value2plot,2),
           category    = case_when(
             category == "EXP22_q27g_1"      ~ "Physical violence",
             category == "EXP22_q27g_2"      ~ "Sexual violence",
             category == "EXP22_q27g_3"      ~ "Threats or coercion",
             category == "EXP22_q27g_4"      ~ "Exploitation",
             category == "EXP22_q27g_5"      ~ "Abuse",
             category == "EXP22_q27g_other"  ~ "Other"
           ),
           order_value = case_when(
             category == "Physical violence"   ~ 6,
             category == "Sexual violence"     ~ 5,
             category == "Threats or coercion" ~ 4,
             category == "Exploitation"        ~ 3,
             category == "Abuse"               ~ 2,
             category == "Other"               ~ 1,
           ))  
  
  plot <- lollipop_chart (data2plot     = data2plot,
                          line_size     = 3,
                          point_size    = 4,
                          line_color    = "#c4c4c4",
                          point_color   = "#2a2a94",
                          categories    = category, 
                          order_value   = order_value, 
                          values        = value2plot)


}

figure22C.fn(nchart = 22) {
  
  # JEISON!!!!
  
}