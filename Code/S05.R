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
## This version:      March 14th, 2022
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

figure16B_CA.fn <- function(nchart = 23) {
  
  w <- 82.59305 # This is the width of the function
  
  voluntary_contacts   <- c("q10a", "q10b", "q10c", "q10d", "q10e", "q10f",
                            "q11f", "q11b", "q11c", "EXP_q10q", "q11g") # Variables of voluntary contacts
  involuntary_contacts <- c("q12a", "q12b", "q12c", "q12d", "q13a", "q14a",
                            "q13a", "q14a", 
                            "EXP_q14",
                            "EXP_q15a", "EXP_q15c", "EXP_q15f", "EXP_q15g", "EXP_q15m", "EXP_q15n") # Variables of involuntary contacts
  
  interactions.df <- data_subset.df %>%
    filter(country == mainCountry) %>%
    filter(year == 2021) %>%
    select(country, year, all_of(voluntary_contacts), all_of(involuntary_contacts)) %>%
    mutate(
      across(c(q10a,q10b,q10c,q10d,q10e, q10f),
             as.double),
      across(c(q10a,q10b,q10c,q10d,q10e),
             ~if_else(.x == 99, NA_real_, .x)),
      across(c(q12a,q12b,q12c),
             ~if_else(.x == 99, NA_real_, .x))
      ) %>%
    rowwise() %>%
    mutate(voluntarySum = sum(q10a,q10b,q10c,q10d,q10e, na.rm = T),
           involuntarySum = sum(q12a,q12b,q12c, na.rm = T)) %>%
    ungroup() %>%
    mutate(across(c(voluntarySum, involuntarySum),
                  ~case_when(
                    .x > 0 ~ 1,
                    T ~ .x
                  ))) 
  

           
  
  ## +++++++++++++++++++
  ## Interactions     -
  ## +++++++++++++++++++
  
  interactionsVol.df <- interactions.df %>% 
    mutate(filtro = if_else(q10f == 99 & voluntarySum == 1, 0, 1, 1)) %>%
    filter(filtro == 1)

  data2plot <- interactionsVol.df %>%
    summarise(
      voluntary_contacts   = mean(voluntarySum, na.rm = T),
      ) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = 1.15)
  
  # Voluntary
  
  voluntary.df <- data2plot %>%
    filter(category %in% "voluntary_contacts") %>%
    mutate(category = if_else(category %in% "voluntary_contacts", " ", category))
  
  voluntary <- horizontal_edgebars(data2plot    = voluntary.df,
                           y_value      = values,
                           x_var        = category,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 0);voluntary
  # Saving panels
  saveIT.fn(chart  = voluntary,
            n      = nchart,
            suffix = "A",
            w      = w,
            h      = 7.029196)
  
  # Involuntary
  
  interactionsIn.df <- interactions.df %>% 
    mutate(filtro = case_when(
      q12d == 99 ~ 0,
      q13a == 99 ~ 0,
      q14a == 99 ~ 0,
      T ~ 1
    )) %>%
    filter(filtro == 1)
  
  data2plot <- interactionsIn.df %>%
    summarise(
      involuntary_contacts   = mean(involuntarySum, na.rm = T),
    ) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = 1.15)
  
  involuntary.df <- data2plot %>%
    filter(category %in% "involuntary_contacts") %>%
    mutate(category = if_else(category %in% "involuntary_contacts", " ", category))
  
  involuntary <- horizontal_edgebars(data2plot    = involuntary.df,
                                     y_value      = values,
                                     x_var        = category,
                                     group_var    = group,
                                     label_var    = label,
                                     x_lab_pos    = x_pos,
                                     y_lab_pos    = 0,
                                     bar_color    = "#EA394F",
                                     margin_top   = 0);involuntary
  # Saving panels
  saveIT.fn(chart  = involuntary,
            n      = nchart,
            suffix = "B",
            w      = w,
            h      = 7.029196)
  
  ## +++++++++++++++++++
  ## Reasons     -
  ## +++++++++++++++++++
  
  # Voluntary
  
  reasonsVoluntary <- interactionsVol.df %>%
    filter(voluntarySum == 1) %>%
    select(q10a, q10b, q10c, q10d, q10e, q10f) %>%
    mutate(across(
      !q10f,
      ~ case_when(
        .x == 1  ~ 1,
        .x == 0  ~ 0,
        .x == 99 ~ NA_real_
      ) 
    )) %>%
    rowwise() %>%
    mutate(voluntary = sum(q10a,q10b,q10c,q10d,q10e, na.rm = T)) %>%
    ungroup() %>%
    mutate(`Report a crime` = if_else(q10a == 1 & voluntary == 1, 1,
                                      if_else(q10f == 1 & voluntary > 1, 1, 0)),
           `Report a case of domestic violence` = if_else(q10b == 1 & voluntary == 1, 1,
                                                          if_else(q10f == 2 & voluntary > 1, 1, 0)),
           `Report an incident or medical emergency` = if_else(q10c == 1 & voluntary == 1, 1,
                                                               if_else(q10f == 3 & voluntary > 1, 1, 0)),
           `Request help or information` = if_else(q10d == 1 & voluntary == 1, 1,
                                                   if_else(q10f == 4 & voluntary > 1, 1,
                                                           if_else(q10e == 1 & voluntary == 1, 1,
                                                                   if_else(q10f == 5 & voluntary > 1, 1,0))))) %>%
    select(!c(q10a, q10b, q10c, q10d, q10e, q10f, voluntary)) %>%
    pivot_longer(cols = everything(), names_to = "reasons_voluntary", values_to = "values") %>%
    group_by(reasons_voluntary) %>%
    summarise(value = mean(values, na.rm = T)) %>%
    mutate(value = value/sum(value),
           empty_value = 1 - value) %>%
    pivot_longer(!reasons_voluntary,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = if_else(reasons_voluntary %in% "Report a crime", 4.15,
                        if_else(reasons_voluntary %in% "Report a case of domestic violence", 3.15,
                                if_else(reasons_voluntary %in% "Report an incident or medical emergency", 2.15, 1.15))))
    
  voluntaryReasons  <- horizontal_edgebars(data2plot    = reasonsVoluntary,
                                           y_value      = values,
                                           x_var        = reasons_voluntary,
                                           group_var    = group,
                                           label_var    = label,
                                           x_lab_pos    = x_pos,
                                           y_lab_pos    = 0,
                                           bar_color    = "#2a2a94",
                                           margin_top   = 0);voluntaryReasons
  # Saving panels
  saveIT.fn(chart  = voluntaryReasons,
            n      = nchart,
            suffix = "C",
            w      = w,
            h      = 47.44707)
  
  # Involuntary
  
  reasonsInvoluntary <- interactionsIn.df %>%
    filter(involuntarySum == 1) %>%
    select(q13a, q14a) %>%
    mutate(
      q14a = case_when(
        q14a == 10 ~ "Routine check/provide assistance",
        q14a == 11 ~ "Routine check/provide assistance",
        q14a == 6  ~ "Ask for cooperation",
        q14a == 8  ~ "Ask for cooperation",
        q14a == 9  ~ "Ask for cooperation",
        q14a == 7  ~ "Pressure for money or harrasment",
        q14a == 1  ~ "Suspected illegal activity",
        q14a == 2  ~ "Suspected illegal activity",
        q14a == 3  ~ "Suspected illegal activity",
        q14a == 4  ~ "Suspected illegal activity",
        q14a == 5  ~ "Suspected illegal activity",
        q14a == 12 ~ "Other"),
      q13a = case_when(
        q13a == 2  ~ "Routine check/provide assistance",
        q13a == 3  ~ "Routine check/provide assistance",
        q13a == 6  ~ "Routine check/provide assistance",
        q13a == 7  ~ "Ask for cooperation",
        q13a == 8  ~ "Pressure for money or harrasment",
        q13a == 1  ~ "Suspected illegal activity",
        q13a == 4  ~ "Suspected illegal activity",
        q13a == 5  ~ "Suspected illegal activity",
        q13a == 9 ~  "Other"
      ),
      universe = n()) %>%
    pivot_longer(cols = !universe, names_to = "variable", values_to = "category") %>%
    mutate(counter = 1) %>%
    select(universe, counter, category) %>%
    group_by(category) %>%
    summarise(total = sum(counter, na.rm = T), universe = mean(universe)) %>%
    drop_na() %>%
    mutate(value = total/universe) %>%
    mutate(empty_value = 1 - value) %>%
    select(!c(universe, total, universe)) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = if_else(category %in% "Routine check/provide assistance", 5.15,
                      if_else(category %in% "Ask for cooperation", 4.15,
                              if_else(category %in% "Pressure for money or harrasment", 3.15, 
                                      if_else(category %in% "Suspected illegal activity", 2.15, 
                                              if_else(category %in% "Other", 1.15, 0))))))
  
  involuntaryReasons  <- horizontal_edgebars(data2plot  = reasonsInvoluntary,
                                           y_value      = values,
                                           x_var        = category,
                                           group_var    = group,
                                           label_var    = label,
                                           x_lab_pos    = x_pos,
                                           y_lab_pos    = 0,
                                           bar_color    = "#EA394F",
                                           margin_top   = 0);involuntaryReasons
  # Saving panels
  
  saveIT.fn(chart  = involuntaryReasons,
            n      = nchart,
            suffix = "D",
            w      = w,
            h      = 59.74817)
  
  ## +++++++++++++++++++
  ## Experience     -
  ## +++++++++++++++++++
  # Voluntary
  
  # Serve the public
  
  data2plot <- interactionsVol.df %>%
    select(q11f, q11b, q11c) %>%
    mutate(q11c_bin = if_else(q11b == 1 & q11c == 1 | q11c == 2, 1, 
                              if_else(q11b == 1 & q11c == 3 | q11c == 4 | q11c == 5, 0, NA_real_)),
           q11f     = case_when(
             q11f   == 1  ~ 1,
             q11f   == 0  ~ 0,
             q11f   == 99 ~ NA_real_
           )) %>%
    select(q11f, q11c_bin) %>%
    summarise(q11f     = mean(q11f, na.rm = T),
              q11c_bin = mean(q11c_bin, na.rm = T)) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
    mutate(category = case_when(
      category == "q11f" ~ "Controlled the situation",
      category == "q11c_bin" ~ "Arrived promptly"
    )) %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = if_else(category %in% "Controlled the situation", 2.15,1.15))
  
  servePublic  <- horizontal_edgebars(data2plot  = data2plot,
                                      y_value      = values,
                                      x_var        = category,
                                      group_var    = group,
                                      label_var    = label,
                                      x_lab_pos    = x_pos,
                                      y_lab_pos    = 0,
                                      bar_color    = "#2a2a94",
                                      margin_top   = 0);servePublic
  saveIT.fn(chart  = servePublic,
            n      = nchart,
            suffix = "E",
            w      = w,
            h      = 23.54781)
  # Involuntary
  
  data2plot <- interactionsIn.df %>%
    select(EXP_q14) %>%
    mutate(EXP_q14     = 
             case_when(
               EXP_q14   == 1  ~ 1,
               EXP_q14   == 0  ~ 0,
               EXP_q14   == 99 ~ NA_real_
           )) %>%
    summarise(EXP_q14     = mean(EXP_q14, na.rm = T)) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
    mutate(category = case_when(
      category == "EXP_q14" ~ "Controlled the situation",
    )) %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = if_else(category %in% "Controlled the situation", 1.15, 0))
  
  servePublic  <- horizontal_edgebars(data2plot  = data2plot,
                                      y_value      = values,
                                      x_var        = category,
                                      group_var    = group,
                                      label_var    = label,
                                      x_lab_pos    = x_pos,
                                      y_lab_pos    = 0,
                                      bar_color    = "#EA394F",
                                      margin_top   = 0);servePublic
  
  # Saving panels

  saveIT.fn(chart  = servePublic,
            n      = nchart,
            suffix = "F",
            w      = w,
            h      = 7.029196)
  
  # Due process
  
  # Voluntary
  
  data2plot <- interactionsVol.df %>%
    select(EXP_q10q, q11g) %>%
    mutate(across(everything(),
                  ~ case_when(
                    .x == 1  ~ 1,
                    .x == 0  ~ 0,
                    .x == 99 ~ NA_real_
                  ))) %>%
    summarise(EXP_q10q     = mean(EXP_q10q, na.rm = T),
              q11g         = mean(q11g, na.rm = T)) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
    mutate(category = case_when(
      category == "EXP_q10q" ~ "Listened to them",
      category == "q11g"     ~ "Treated them with respect"
    )) %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = if_else(category %in% "Listened to them", 2.15,1.15))
  
  dueProcess   <- horizontal_edgebars(data2plot  = data2plot,
                                      y_value      = values,
                                      x_var        = category,
                                      group_var    = group,
                                      label_var    = label,
                                      x_lab_pos    = x_pos,
                                      y_lab_pos    = 0,
                                      bar_color    = "#2a2a94",
                                      margin_top   = 0);dueProcess
  
  # Saving panels
  
  saveIT.fn(chart  = dueProcess,
            n      = nchart,
            suffix = "G",
            w      = w,
            h      = 23.54781)
  
  # Involuntary
  
  data2plot <- interactions.df %>%
    filter(involuntarySum == 1) %>%
    select(EXP_q15a, EXP_q15c, EXP_q15f, EXP_q15g, EXP_q15m, EXP_q15n) %>%
    mutate(across(everything(),
                  ~ case_when(
                    .x == 1  ~ 1,
                    .x == 0  ~ 0,
                    .x == 99 ~ NA_real_
                  ))) %>%
    summarise(EXP_q15a     = mean(EXP_q15a, na.rm = T),
              EXP_q15c     = mean(EXP_q15c, na.rm = T),
              EXP_q15f     = 1 - mean(EXP_q15f, na.rm = T),
              EXP_q15g     = 1 - mean(EXP_q15g, na.rm = T),
              EXP_q15m     = mean(EXP_q15m, na.rm = T),
              EXP_q15n     = mean(EXP_q15n, na.rm = T)) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
    mutate(category = case_when(
      category == "EXP_q15a" ~ "Had a legitimate reason to stop them",
      category == "EXP_q15c" ~ "Explained the reasons for their actions",
      category == "EXP_q15f" ~ "Did not threaten them",
      category == "EXP_q15g" ~ "Did not use physical force against them",
      category == "EXP_q15m" ~ "Listened to them",
      category == "EXP_q15n" ~ "Treated them with respect"
    )) %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!category,
                 names_to = "group",
                 values_to = "values") %>%
    mutate(
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(values*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
      x_pos = if_else(category %in% "Had a legitimate reason to stop them", 6.15,
                      if_else(category %in% "Explained the reasons for their actions", 5.15,
                              if_else(category %in% "Did not threaten them", 4.15, 
                                      if_else(category %in% "Did not use physical force against them", 3.15, 
                                              if_else(category %in% "Listened to them", 2.15, 1.15))))))
  
  dueProcess   <- horizontal_edgebars(data2plot  = data2plot,
                                      y_value      = values,
                                      x_var        = category,
                                      group_var    = group,
                                      label_var    = label,
                                      x_lab_pos    = x_pos,
                                      y_lab_pos    = 0,
                                      bar_color    = "#EA394F",
                                      margin_top   = 0);dueProcess
  
  # Saving panels
  
  saveIT.fn(chart  = dueProcess,
            n      = nchart,
            suffix = "H",
            w      = w,
            h      = 72.04926)
  
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

figure19B.fn <- function(nchart = 19) {
  
  migrated <- data_subset.df %>%
    filter(country %in% mainCountry) %>%
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
                               "areaUrban" = "Urban", "whiteWhite" = "Light Skin Tone", "youngLess than 30 years" = "Younger than 30",
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
  logit_plot <- logit_demo_panel(mainData = data2plot_P1, 
                                 line_size = 1.25, 
                                 point_color = "#a90099", 
                                 line_color  = "#a90099") +
    scale_y_continuous(limits = c(-0.30, 0.30),
                       breaks = seq(-0.30, 0.30, by = 0.15),
                       expand = expansion(mult = 0.075), position = "right",
                       labels = c("-30", "-15", "0", "+15","+30")) +
    labs(y = "Less likely                      More likely")
  # Saving panels
  
  saveIT.fn(chart  = logit_plot,
            n      = nchart,
            suffix = "B",
            w      = 87.16203,
            h      = 56.23357)
  # Panel 2
  
  data2plot_P2 <- logit_demo(mainData = migrated, Yvar = migrated3yrs)
  logit_plot <- logit_demo_panel(mainData = data2plot_P2, 
                                 line_size = 1.25, 
                                 point_color = "#a90099", 
                                 line_color  = "#a90099") +
    scale_y_continuous(limits = c(-0.30, 0.30),
                       breaks = seq(-0.30, 0.30, by = 0.15),
                       expand = expansion(mult = 0.075), position = "right",
                       labels = c("-30", "-15", "0", "+15","+30")) +
    labs(y = "Less likely                      More likely")
  
  # Saving panels
  
  saveIT.fn(chart  = logit_plot,
            n      = nchart,
            suffix = "C",
            w      = 87.16203,
            h      = 56.23357)
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
             color    = "white",
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
                                  "Secondary" = 0.5)) +
    WJP_theme() +
    theme(axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color    = "#D0D1D3",
                                            linetype = "dashed"));chart
  
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

figure20C.fn <- function(nchart = 20) {
  
  # Defining data to plot
  data2plot <- data_subset.df %>% 
    filter(country == mainCountry & year == latestYear) %>% 
    mutate(EXP_q31h_ajust = case_when(
      EXP_q31h == 0  ~ 0,
      EXP_q31h == 1  ~ 1,
      EXP_q31h == 99 ~ NA_real_)
    ) %>% 
    select(country, EXP_q31h_ajust) %>% 
    group_by(country, EXP_q31h_ajust) %>%  
    filter(!is.na(EXP_q31h_ajust)) %>% 
    summarise(n = n()) %>% 
    mutate(
      value2plot = round(100 * (n/sum(n)), 0),
      ymax = cumsum(value2plot),
      ymin = c(0, head(value2plot, n = -1)),
      labels     = case_when(
        EXP_q31h_ajust == 1 ~ paste0("<span style='color:#2a2a94;font-size:3.514598mm;font-weight:bold;'>",
                                       to_percentage.fn(value2plot), " Yes",
                                       "</span>"),
        EXP_q31h_ajust == 0 ~ paste0("<span style='color:#a90099;font-size:3.514598mm;font-weight:bold;'>",
                                       to_percentage.fn(value2plot), " No",
                                       "</span>")
      )
    )
  
  
  
  # Plotting data
  chart <- ggplot(data = data2plot, 
                  aes(x     = 2,
                      xmin  = 3,
                      xmax  = 4,
                      y     = value2plot,
                      ymin  = ymin,
                      ymax  = ymax,
                      fill  = labels)) +
    geom_rect(color = NA,
              show.legend = F) +
    geom_point(color = NA) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("#2a2a94","#a90099"),) +
    WJP_theme() +
    theme(panel.grid.major   = element_blank(),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_blank(),
          axis.text.x        = element_blank(),
          legend.text        = element_markdown(hjust = 0,
                                                family = "Lato Full",
                                                size = 2.460219*.pt),
          legend.title       = element_blank(),
          legend.key         = element_rect(fill = NA),
          legend.margin      = margin(-5,0,0,-15),
          plot.margin        = margin(-5,0,-5,-15), 
          plot.background = element_blank(),
          legend.background = element_blank(), 
          legend.box.background = element_blank()) +
    guides(fill = guide_legend(override.aes = list(shape  = 21,
                                                   size   = 3)))
  
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = "C",
            w      = 42.17518,
            h      = 16.87007)
  
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 21                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure21A.fn <- function(nchart = 21) {
  
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
               category == "EXP22_q27d_1" ~ "Legal counsel /<br>inmigration services",
               category == "EXP22_q27d_2" ~ "Medical services",
               category == "EXP22_q27d_3" ~ "Other NGOs",
               category == "EXP22_q27d_4" ~ "Local authorities",
               category == "EXP22_q27d_5" ~ "Local community <br>members",
               category == "EXP22_q27d_6" ~ "Religious <br> organizations",
               category == "EXP22_q27d_7" ~ "Family members/ <br> friends",
               category == "EXP22_q27d_8" ~ "Other",
               category == "EXP22_q27d_9" ~ NA_character_,
               category == "EXP22_q27d_99"~ NA_character_ 
             )) %>%
    drop_na() %>%
    group_by(category) %>%
    summarise(values = sum(value, na.rm = T),
              universe = mean(universe)) %>%
    ungroup() %>%
    mutate(value = values/universe) %>%
    arrange(-values) %>%
    slice_max(values, n = 3, with_ties = F) %>%
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
                           x_lab_pos    = x_pos + 0.05,
                           y_lab_pos    = 0,
                           bar_color    = "#a90099",
                           margin_top   = 7.5);a
  saveIT.fn(chart  = a,
            n      = nchart,
            suffix = "A",
            w      = 40.76934,
            h      = 36.90328)
  }


figure21B.fn <- function(nchart = 21) {
  
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

figure22A.fn <- function(nchart = 22) {
  
  # Function plot
  plot_bar <- function(data2plot, colors4plot) {
    ggplot(data2plot, 
           aes(x     = country,
               y     = value2plot,
               label = labels,
               fill  = category,
               alpha   = highlighted)) +
      geom_bar(stat = "identity",
               color = "white",
               show.legend = F,
               position = "dodge") +
      geom_text(aes(label = labels, alpha = highlighted), 
                position = position_dodge(width = 0.9), vjust = -0.25,
                color    = "#4a4a49",
                family   = "Lato Full",
                fontface = "bold", show.legend = F) +
      labs(y = "% of respondents", x = "") +
      scale_fill_manual(values = colors4plot) +
      scale_alpha_manual(values = c("Primary"   = 1,
                                    "Secondary" = 0.5)) +
      scale_y_continuous(limits = c(0, 100), 
                         labels = c("0%","20%", "40%", "60%", "80%", "100%"), 
                         breaks = c(0, 20, 40, 60, 80, 100)) + 
      WJP_theme()  +
      theme(axis.title.x       = element_blank(),
            axis.title.y       = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color    = "#D0D1D3",
                                              linetype = "dashed"))
    
  }
  
  ####### INTERNATIONAL MIGRATION VIOLENCE / BRIBERY BY COUNTRY 
  
  # Format
  barsPalette    <- c("#2a2a94", "#a90099")
  
  # First Group
  data2plot_1 <-  data_subset.df %>% 
    filter(! country %in% c("Nicaragua", "Costa Rica")) %>% 
    mutate(EXP22_q27f_ajust =
             case_when(EXP22_q27f == 0  ~ 0,
                       EXP22_q27f == 1  ~ 1,
                       EXP22_q27f == 99  ~ NA_real_)) %>% 
    group_by(country) %>%
    summarise(violence = round(mean(as.numeric(EXP22_q27f_ajust), na.rm = T),2)) %>%
    pivot_longer(!country,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(value2plot  = case_when(is.na(value2plot) ~ 0,
                                   !is.na(value2plot) ~ value2plot*100),
           highlighted = if_else(country == mainCountry, "Primary", "Secondary"),
           labels       = case_when(value2plot == 0 ~ "",
                                    value2plot != 0 ~ paste0(value2plot, "%")))
  # Second Group
  data2plot_2 <-  data_subset.df %>% 
    filter(! country %in% c("Nicaragua", "Costa Rica")) %>% 
    mutate(EXP22_q27i_ajust =
             case_when(EXP22_q27i == 0  ~ 0,
                       EXP22_q27i == 1  ~ 1,
                       EXP22_q27i == 99  ~ NA_real_)) %>% 
    group_by(country) %>%
    summarise(bribery = round(mean(as.numeric(EXP22_q27i_ajust), na.rm = T),2)) %>%
    pivot_longer(!country,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(value2plot  = case_when(is.na(value2plot) ~ 0,
                                   !is.na(value2plot) ~ value2plot*100),
           highlighted = if_else(country == mainCountry, "Primary", "Secondary"),
           labels       = case_when(value2plot == 0 ~ "",
                                    value2plot != 0 ~ paste0(value2plot, "%")))
  
  data2plot <- rbind(data2plot_1, data2plot_2)
  
  # Colores del grafico
  colors4plot <- barsPalette
  
  # Generate Plot
  barPlot <- plot_bar(data2plot, colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = barPlot,
            n      = nchart,
            suffix = "A",
            w      = 189.7883,
            # h      = 52.7189,
            h      = 38.06642            
  )
  
}

figure22B.fn <- function(nchart = 22) {

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
  # Saving panels
  saveIT.fn(chart  = plot,
            n      = nchart,
            suffix = "B",
            w      = 189.7883,
            # h      = 52.7189,
            h      = 38.06642  )
}

figure22C.fn <- function(nchart = 22) {
  
  # Plot function
  plot_bar_2 <- function(data, colors4plot) {
    ggplot(data, 
           aes(x = grp,
               y = perc,
               label = labels,
               fill = category)) +
      geom_bar(stat = "identity",
               color = "white",
               show.legend = F,
               position = "dodge") + #aes(alpha = highlighted)) +
      geom_text(aes(label = labels), 
                position = position_dodge(width = 0.9), vjust = -0.25,
                color = "#4a4a49",
                family = "Lato Full",
                fontface = "bold") +
      labs(y = "% of respondents", x = "") +
      scale_fill_manual(values = colors4plot) +
      scale_y_continuous(limits = c(0, 100), 
                         labels = c("0%","20%", "40%", "60%", "80%", "100%"), 
                         breaks = c(0, 20, 40, 60, 80, 100)) + 
      WJP_theme() +
      theme(axis.title.x       = element_blank(),
            axis.title.y       = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color    = "#D0D1D3",
                                              linetype = "dashed"))
  }
  
  
  barsPalette    <- c("#2a2a94", "#a90099")
  
  # Data2Plot
  vars4plot <- list("In your home \ncountry"                  = c("EXP22_q27h_1", "EXP22_q27j_1"),
                    "In another country \nin Central America" = c("EXP22_q27h_2", "EXP22_q27j_2"),
                    "In Mexico"                             = c("EXP22_q27h_3", "EXP22_q27j_3"),
                    "In the United States"                  = c("EXP22_q27h_4", "EXP22_q27j_4"))
  
  data2plot_c2  <-  data_subset.df %>% 
    filter(country == mainCountry) %>% 
    select(country, all_of(unlist(vars4plot,
                                  use.names = F))) %>%
    mutate(
      across(starts_with("EXP22_q27"),
             ~case_when(
               .x == 0  ~ 0,
               .x == 1  ~ 1,
               .x == 99  ~ NA_real_
             ))
    ) %>%
    group_by(country) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!country,
                 names_to  = "category",
                 values_to = "perc") %>%
    mutate(
      perc   = round(perc*100, 0),
      grp    = case_when(
        str_detect(category, "_1") ~ "In your home \ncountry",
        str_detect(category, "_2") ~ "In another country \nin Central America",
        str_detect(category, "_3") ~ "In Mexico",
        str_detect(category, "_4") ~ "In the United States"
      )) %>% 
    mutate(#highlighted = if_else(grp == "In another country in Central America", 1, 1),
      labels = paste0(perc, "%"))
  
  data2plot_c2$grp <- factor(data2plot_c2$grp, levels = names(vars4plot))
  
  colors4plot <- c("#2a2a94", "#2a2a94", "#2a2a94", "#2a2a94",
                   "#a90099", "#a90099", "#a90099", "#a90099")
                            
  
  # Generate Plot
  barPlot2 <- plot_bar_2(data2plot_c2, colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = barPlot2,
            n      = nchart,
            suffix = "C",
            # w      = 189.7883,
            w      = 179.7883,
            # h      = 52.7189,
            h      = 38.06642
  )
}

