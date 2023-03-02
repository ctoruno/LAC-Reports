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
##    Figure 16B - Central America                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure16B_CA.fn(nchart = 16) {
  
  voluntary_contacts   <- c("q10a", "q10b", "q10c", "q10d", "q10e",
                            "q11f", "q11b", "q11c", "q11g", "q11d")
  involuntary_contacts <- c("q12a", "q12b", "q12c",
                            "q13a", "q14a")
  interactions_universe <- data_subset.df %>%
    select()
  
## +++++++++++++++++++
## Interactions
## ++++++++++++++++++

  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 19                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure19A.fn <- function(nchart = 19) {
  
  # Defining country parameters
  if (mainCountry == "Belize") {
    shpACRON  <- "BLZ"
  }
  if (mainCountry == "El Salvador") {
    shpACRON  <- "SLV"
  }
  if (mainCountry == "Guatemala") {
    shpACRON  <- "GTM"
  }
  if (mainCountry == "Honduras") {
    shpACRON  <- "HND"
  }
  if (mainCountry == "Panama") {
    shpACRON  <- "PAN"
  }
  
  # Drawing base map
  base_map.sf <- boundaries.sf %>%
    filter(shapeGroup == shpACRON)
  
  # Sub-setting data for map
  data4map <- map_data.ls[["CAmapdata"]] %>%
    filter(country == mainCountry) %>%
    mutate(
      across(c(intmig_person, recent_intmig, desire2emig, plans2emig, x, y),
             as.double),
      across(c(intmig_person, recent_intmig, desire2emig, plans2emig),
             ~to_percentage.fn(.x*100)),
      location = str_extract(location, ".+(?=,)"),
      location = case_when(
        location == "Ciudad De Guatemala" ~ "Guatemala City",
        location == "Belize"              ~ "Belize City",
        location == "Panamá"              ~ "Panama City",
        TRUE ~ location
        ),
      label    = paste0('<span style="color:#222221;font-size:3.514598mm;">', location, "</span><br>",
                        '<span style="color:#2a2a94;font-size:5.5mm;">&#9642; </span>', 
                        '<span style="color:#2a2a94;font-size:3.514598mm;">', intmig_person, "   </span>",
                        '<span style="color:#a90099;font-size:5.5mm;">&#9642; </span>', 
                        '<span style="color:#a90099;font-size:3.514598mm;">', recent_intmig, "</span>"),
      x = case_when(
        location == "Guatemala City" ~ x*0.9985,
        location == "Quetzaltenango" ~ x*1.0015,
        location == "Huehuetenango"  ~ x*1.0015,
        location == "Belmopan"       ~ x*1.0010,
        location == "Belize City"    ~ x*1.0010,
        location == "Santa Ana"      ~ x*0.9995,
        location == "San Salvador"   ~ x*1.0095,
        location == "San Miguel"     ~ x*1.0095,
        location == "David"          ~ x*1.0025,
        location == "Panama City"    ~ x*1.0025,
        location == "Colón"          ~ x*0.9995,
        TRUE ~ x
      ),
      y = case_when(
        location == "Guatemala City" ~ y*0.9875,
        location == "Quetzaltenango" ~ y*1.0195,
        location == "Huehuetenango"  ~ y*1.0195,
        location == "Belmopan"       ~ y*1.0115,
        location == "San Ignacio"    ~ y*0.9925,
        location == "Belize City"    ~ y*0.9865,
        location == "Santa Ana"      ~ y*1.0015,
        location == "San Salvador"   ~ y*0.9880,
        location == "San Miguel"     ~ y*0.9920,
        location == "David"          ~ y*1.0625,
        location == "Panama City"    ~ y*0.9400,
        location == "Colón"          ~ y*0.9995,
        TRUE ~ y
      )
    )
  
  # Drawing map
  map <- ggplot(data = base_map.sf) +
    geom_sf(color = "white",
            fill  = "#ebebf5") +
    geom_sf(data  = data4map,
            color = "black",
            size  = 1.5) +
    geom_richtext(data      = data4map,
                  aes(x     = x, 
                      y     = y, 
                      label = label),
                  family   = "Lato Full",
                  fontface = "bold", 
                  size  = 3,
                  fill  = NA, 
                  hjust = 0,
                  label.color = NA) +
    theme_void() +
    theme(panel.background   = element_blank(),
          plot.background    = element_blank())

  # Saving panels
  saveIT.fn(chart  = map,
            n      = nchart,
            suffix = "A",
            w      = 69.23758,
            h      = 73.10364)

}

figure19B.fn(nchart = 19) {
  
  migrated <- data_subset.df %>%
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

figure20A.fn <- function(nchart = 20) {
  
  # Defining data for plot
  data2plot <- data_subset.df %>%
    select(country, year, EXP_q31d, EXP_q31f) %>%
    filter(year > 2020) %>%
    filter(country %in% c("Belize", "Guatemala", "Honduras", "El Salvador", "Panama")) %>%
    mutate(
      EXP_q31d = case_when(
        EXP_q31d == 1  ~ 1,
        EXP_q31d == 2  ~ 0
      ),
      EXP_q31f = case_when(
        EXP_q31f == 1  ~ 1,
        EXP_q31f == 0  ~ 0
      )
    ) %>%
    group_by(country, year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!c(country, year),
                 names_to  = "category",
                 values_to = "value2plot") %>%
    mutate(
      label = to_percentage.fn(value2plot*100),
      high  = if_else(country == mainCountry, "Primary", "Secondary"),
      label = if_else(country == mainCountry & year == 2021,
                      label,
                      NA_character_)
    )
  
  # Producing ggplot
  chart <- ggplot() +
    geom_bar(data  = data2plot %>% filter(year == 2022),
             aes(x     = country,
                 y     = value2plot*100,
                 fill  = category,
                 alpha = high),
             stat     = "identity",
             position = "dodge",
             show.legend = F) +
    geom_point(data = data2plot %>% filter(year == 2021),
               aes(x     = country,
                   y     = value2plot*100,
                   fill  = category),
               position  = position_dodge(width = 0.9),
               color     = "black",
               show.legend = F) +
    geom_text(data = data2plot %>% filter(year == 2021),
              aes(x     = country,
                  y     = value2plot*100,
                  label = label,
                  alpha = high),
              nudge_x   = ifelse(data2plot$category == "EXP_q31d", -0.25, 0.25),
              nudge_y   = 8,
              family    = "Lato Full",
              fontface  = "bold",
              size      = 3.514598,
              show.legend = F) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100, 20),
                       labels = paste0(seq(0,100, 20),
                                       "%")) +
    scale_fill_manual(values = c("EXP_q31d" = "#2a2a94",
                                 "EXP_q31f" = "#a90099")) +
    scale_alpha_manual(values = c("Primary"   = 1,
                                  "Secondary" = 0.7)) +
    WJP_theme() +
    theme(axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color    = "#D0D1D3",
                                            linetype = "dashed"))
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = "A",
            w      = 189.7883,
            h      = 52.71897)
  
}

figure20B.fn <- function(nchart = 20) {
  
  # Defining country parameters
  if (mainCountry == "Belize") {
    shpACRON  <- "BLZ"
  }
  if (mainCountry == "El Salvador") {
    shpACRON  <- "SLV"
  }
  if (mainCountry == "Guatemala") {
    shpACRON  <- "GTM"
  }
  if (mainCountry == "Honduras") {
    shpACRON  <- "HND"
  }
  if (mainCountry == "Panama") {
    shpACRON  <- "PAN"
  }
  
  # Drawing base map
  base_map.sf <- boundaries.sf %>%
    filter(shapeGroup == shpACRON)
  
  # Sub-setting data for map
  data4map <- map_data.ls[["CAmapdata"]] %>%
    filter(country == mainCountry) %>%
    mutate(
      across(c(intmig_person, recent_intmig, desire2emig, plans2emig, x, y),
             as.double),
      across(c(intmig_person, recent_intmig, desire2emig, plans2emig),
             ~to_percentage.fn(.x*100)),
      location = str_extract(location, ".+(?=,)"),
      location = case_when(
        location == "Ciudad De Guatemala" ~ "Guatemala City",
        location == "Belize"              ~ "Belize City",
        location == "Panamá"              ~ "Panama City",
        TRUE ~ location
      ),
      label    = paste0('<span style="color:#222221;font-size:3.514598mm;">', location, "</span><br>",
                        '<span style="color:#2a2a94;font-size:5.5mm;">&#9642; </span>', 
                        '<span style="color:#2a2a94;font-size:3.514598mm;">', desire2emig, "   </span>",
                        '<span style="color:#a90099;font-size:5.5mm;">&#9642; </span>', 
                        '<span style="color:#a90099;font-size:3.514598mm;">', plans2emig, "</span>"),
      x = case_when(
        location == "Guatemala City" ~ x*0.9985,
        location == "Quetzaltenango" ~ x*1.0015,
        location == "Huehuetenango"  ~ x*1.0015,
        location == "Belmopan"       ~ x*1.0010,
        location == "Belize City"    ~ x*1.0010,
        location == "Santa Ana"      ~ x*0.9995,
        location == "San Salvador"   ~ x*1.0095,
        location == "San Miguel"     ~ x*1.0095,
        location == "David"          ~ x*1.0025,
        location == "Panama City"    ~ x*1.0025,
        location == "Colón"          ~ x*0.9995,
        TRUE ~ x
      ),
      y = case_when(
        location == "Guatemala City" ~ y*0.9875,
        location == "Quetzaltenango" ~ y*1.0195,
        location == "Huehuetenango"  ~ y*1.0195,
        location == "Belmopan"       ~ y*1.0115,
        location == "San Ignacio"    ~ y*0.9925,
        location == "Belize City"    ~ y*0.9865,
        location == "Santa Ana"      ~ y*1.0015,
        location == "San Salvador"   ~ y*0.9880,
        location == "San Miguel"     ~ y*0.9920,
        location == "David"          ~ y*1.0625,
        location == "Panama City"    ~ y*0.9400,
        location == "Colón"          ~ y*0.9995,
        TRUE ~ y
      )
    )
  
  # Drawing map
  map <- ggplot(data = base_map.sf) +
    geom_sf(color = "white",
            fill  = "#ebebf5") +
    geom_sf(data  = data4map,
            color = "black",
            size  = 1.5) +
    geom_richtext(data      = data4map,
                  aes(x     = x, 
                      y     = y, 
                      label = label),
                  family   = "Lato Full",
                  fontface = "bold", 
                  size  = 3,
                  fill  = NA, 
                  hjust = 0,
                  label.color = NA) +
    theme_void() +
    theme(panel.background   = element_blank(),
          plot.background    = element_blank())
  
  # Saving panels
  saveIT.fn(chart  = map,
            n      = nchart,
            suffix = "B",
            w      = 69.23758,
            h      = 73.10364)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 21                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure21A.fn(nchart = 21) {
  
  data2plot <- data_subset.df %>%
    filter(year == latestYear & country == mainCountry & EXP_q31j == 1) %>%
    select(starts_with("EXP22_q27d")) %>%
    mutate(counter = 1,
           universe = sum(counter)) %>%
    select(!counter) %>%
    mutate(across(
      !universe,
      ~ case_when(
        .x == 1 ~ 1,
        .x == 0 ~ 0,
        .x == 99 ~ NA_real_,
      )
    )) %>%
    pivot_longer(cols = !universe, names_to = "category", values_to = "value") %>%
    mutate(category = 
             case_when(
               category == "EXP22_q27d_1" ~ "Legal counsel/inmigration services",
               category == "EXP22_q27d_2" ~ "Medical services",
               category == "EXP22_q27d_3" ~ "Other NGOs",
               category == "EXP22_q27d_4" ~ "Local authorities",
               category == "EXP22_q27d_5" ~ "Local community members",
               category == "EXP22_q27d_6" ~ "Religious organizations",
               category == "EXP22_q27d_7" ~ "Family members/friends",
               category == "EXP22_q27d_8" ~ "Other",
               category == "EXP22_q27d_9" ~ "None of the above",
               category == "EXP22_q27d_99"~ "Don't know/No Answer" 
             )) %>%
    group_by(category) %>%
    summarise(values = sum(value, na.rm = T),
              universe = mean(universe)) %>%
    ungroup() %>%
    mutate(value = values/universe) %>%
    arrange(-values) %>%
    top_n(values, n = 3) %>%
    select(category, value) %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = as.numeric(as.factor(label))
    ) %>%
    group_by(category) %>%
    mutate(x_pos = mean(x_pos, na.rm = T)) %>%
    ungroup()
  
  a <- horizontal_edgebars(data2plot    = data2plot,
                           y_value      = value,
                           x_var        = category,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos + 0.15,
                           y_lab_pos    = 0,
                           bar_color    = "#a90099",
                           margin_top   = 0);a
  return(a)
}


figure21B.fn(nchart = 21) {
  
  # Preparing data for map
  data4map <- data_subset.df %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(EXP22_q27n) %>%
    mutate(
      EXP22_q27n = as.double(EXP22_q27n),
      EXP22_q27n = case_when(
        EXP22_q27n == 1 ~ "San Diego",
        EXP22_q27n == 2 ~ "El Centro",
        EXP22_q27n == 3 ~ "Yuma",
        EXP22_q27n == 4 ~ "Tucson",
        EXP22_q27n == 5 ~ "El Paso",
        EXP22_q27n == 6 ~ "Big Bend",
        EXP22_q27n == 7 ~ "Del Rio",
        EXP22_q27n == 8 ~ "Laredo",
        EXP22_q27n == 9 ~ "Rio Grande",
        EXP22_q27n == 10 ~ "Other",
        EXP22_q27n == 99 ~ NA_character_,
      )
    ) %>%
    group_by(EXP22_q27n) %>%
    summarise(n = n()) %>%
    filter(!is.na(EXP22_q27n)) %>%
    ungroup() %>%
    mutate(total = sum(n),
           value = n/total,
           label = to_percentage.fn(value*100)) %>%
    slice_max(value,
              n = 3,
              with_ties = F) %>%
    mutate(pos = row_number()) %>%
    rename(location = EXP22_q27n) %>%
    left_join(map_data.ls[["BorderPoints"]] %>%
                st_drop_geometry() %>%
                mutate(location = case_when(
                  str_detect(location, "San Diego")  ~ "San Diego",
                  str_detect(location, "El Centro")  ~ "El Centro",
                  str_detect(location, "Yuma")       ~ "Yuma",
                  str_detect(location, "Tucson")     ~ "Tucson",
                  str_detect(location, "El Paso")    ~ "El Paso",
                  str_detect(location, "Big Bend")   ~ "Big Bend",
                  str_detect(location, "Del Rio")    ~ "Del Rio",
                  str_detect(location, "Laredo")     ~ "Laredo",
                  str_detect(location, "Rio Grande") ~ "Rio Grande",
                )) %>%
                select(location, x, y)) %>%
    st_as_sf(coords  = c("x", "y"),
             crs     = 4326,
             remove  = F,
             na.fail = F)
  
  # Drawing map
  map <- ggplot(data  = data4map %>% filter(!is.na(x))) + 
    geom_sf(data  = map_data.ls[["USA_map"]],
            color = "#ebebf5",
            fill  = "#ebebf5") +
    geom_sf(color = "#a90099",
            size  = 5) +
    geom_text(aes(x     = as.double(x),
                  y     = as.double(y),
                  label = pos),
              family   = "Lato Full",
              fontface = "bold",
              color    = "white",
              size     = 3.5) +
    theme_void() +
    theme(panel.background   = element_blank(),
          plot.background    = element_blank())
  
  # Saving panels
  saveIT.fn(chart  = map,
            n      = nchart,
            suffix = "B",
            w      = 145.8558,
            h      = 72.40072)
    
   
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