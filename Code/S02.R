## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Section II Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 21st, 2022
##
## This version:      December 1st, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 7                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure07.fn <- function(nchart = 7) {
  
  # Variables to plot
  vars4plot = list("Legislative"  = c("q2a"), 
                   "Police"       = c("q2d"), 
                   "Executive"    = c("q2b", "q2c"), 
                   "Judiciary"    = c("q2e", "q2f", "q2g"))
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    select(year, all_of(unlist(vars4plot))) %>%
    mutate(
      across(!year,
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_))),
      year = paste0("'", str_sub(year, start = -2))
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!year,
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(value = value*100,
           label = paste0(format(round(value, 0),
                                 nsmall = 0),
                          "%"))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Plotting each panel of Figure 8
  imap(c("A" = "Legislative", 
         "B" = "Police", 
         "C" = "Executive", 
         "D" = "Judiciary"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(str_detect(category, varSet))
         
         # Defining colors4plot
         colors4plot    <- glinesPalette[1:length(vars4plot[[varSet]])]
         naming_vector  <- paste0(varSet, as.character(1:length(vars4plot[[varSet]])))
         
         if (length(vars4plot[[varSet]]) == 1) {
           naming_vector <- str_remove(naming_vector, "1")
         }
  
         names(naming_vector)
         
         # Applying plotting function
         chart <- LAC_lineChart(data           = data2plot,
                                target_var     = "value",
                                grouping_var   = "year",
                                ngroups        = data2plot$category, 
                                labels_var     = "label",
                                colors_var     = "category",
                                colors         = colors4plot,
                                repel          = T
         )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 91.37955,
                   h      = 76.9697)
         
       })
    
} 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 8                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure08.fn <- function(nchart = 8){
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Media"        = c("CAR_q6p", "CAR_q6q"),
    "Government"   = c("q2a", "q2c", "q2b"),
    "Public Admin" = c("CAR_q6i", "CAR_q6j", "CAR_q6k", "CAR_q6l", "CAR_q6m", "CAR_q6n", "CAR_q6o"),
    "Judiciary"    = c("q2g", "q2e", "q2f", "q2d", "CAR_q6h")
  )
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country %in% countrySet & year == latestYear) %>%
    select(country, unlist(vars4plot, 
                           use.names = F)) %>%
    mutate(
      across(!country,
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    group_by(country) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!country,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(
      labels = case_when(
        category == "q2a"     ~ "Members of the legislature",
        category == "q2c"     ~ "National government officers",
        category == "q2b"     ~ "Local government officers",
        category == "q2g"     ~ "Judges and magistrates",
        category == "q2e"     ~ "Prosecutors in charge of\ncriminal investigations",
        category == "q2f"     ~ "Public defense attorneys",
        category == "q2d"     ~ "Police officers", 
        category == "CAR_q6h" ~ "Members of the Armed Forces",
        category == "CAR_q6i" ~ "Tax/Revenue officers",
        category == "CAR_q6j" ~ "Customs officers",
        category == "CAR_q6k" ~ "Public utility company officers",
        category == "CAR_q6l" ~ "Doctors and nurses in\npublic hospitals", 
        category == "CAR_q6m" ~ "Teachers in public schools", 
        category == "CAR_q6n" ~ "Land registry officers", 
        category == "CAR_q6o" ~ "Car registration officers", 
        category == "CAR_q6p" ~ "News Media", 
        category == "CAR_q6q" ~ "Political parties"
      ),
      value2plot = round(value2plot*100,1)
    ) %>%
    mutate(
      order_var = case_when(
        category == "q2a"     ~ 1 ,
        category == "q2c"     ~ 2,
        category == "q2b"     ~ 3,
        category == "q2g"     ~ 1,
        category == "q2e"     ~ 2,
        category == "q2f"     ~ 4,
        category == "q2d"     ~ 3, 
        category == "CAR_q6h" ~ 5,
        category == "CAR_q6i" ~ 2,
        category == "CAR_q6j" ~ 1 ,
        category == "CAR_q6k" ~ 4,
        category == "CAR_q6l" ~ 6, 
        category == "CAR_q6m" ~ 7, 
        category == "CAR_q6n" ~ 5, 
        category == "CAR_q6o" ~ 3, 
        category == "CAR_q6p" ~ 2, 
        category == "CAR_q6q" ~ 1
      )
    )
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Defining color palette
  colors4plot <- countryPalette
  
  # Defining opacity vector
  opacities4plot <- c(1, rep(0.5, length(countrySet)-1))
  names(opacities4plot) <- countrySet
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Media", 
         "B" = "Government", 
         "C" = "Public Admin",
         "D" = "Judiciary"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]]) %>%
           arrange(order_var)
         
         # Applying plotting function
         chart <- LAC_dotsChart(data         = data2plot,
                                target_var   = "value2plot",
                                grouping_var = "country",
                                labels_var   = "labels",
                                colors       = colors4plot,
                                diffOpac     = T,
                                order_var    = order_var,
                                opacities    = opacities4plot)
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 2) {
           h = 21.43905
         }
         if (length(vars4plot[[varSet]]) == 3) {
           h = 32.68576
         }
         if (length(vars4plot[[varSet]]) == 5) {
           h = 49.90729
         }
         if (length(vars4plot[[varSet]]) == 7) {
           h = 65.72298
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 189.7883,
                   h      = h)
         
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 9                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure09.fn <- function(nchart = 9){
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Offered"   = c("CAR_q2c"),
    "Requested" = c("CAR_q2b", "CAR_q2f", "CAR_q2g"),
    "Nepotism"  = c("CAR_q2a", "CAR_q2d", "CAR_q2e")
  )
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country %in% countrySet & year == latestYear) %>%
    select(country, unlist(vars4plot, 
                           use.names = F)) %>%
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
    pivot_longer(!country,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(
      labels = case_when(
        category == "CAR_q2b" ~ "A public officer asking for a bribe \nto speed up administrative procedures",
        category == "CAR_q2f" ~ "A law enforcement officer (police, \ncustoms, immigration, civil guard, military police) \nasking for a bribe",
        category == "CAR_q2g" ~ "A company official asking for a bribe\nfrom a job applicant",
        category == "CAR_q2c" ~ "A private citizen offering a bribe to a\npublic official to speed up administrative procedures",
        category == "CAR_q2a" ~ "A public officer being recruited on the\nbasis of family ties and friendship networks",
        category == "CAR_q2d" ~ "An elected official taking public funds\nfor private use",
        category == "CAR_q2e" ~ "An elected official using stolen public\nfunds to assist his or her community"
      ),
      value2plot = round(value2plot*100,1)
    )
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Defining color palette
  colors4plot <- countryPalette
  
  # Defining opacity vector
  opacities4plot <- c(1, rep(0.5, length(countrySet)-1))
  names(opacities4plot) <- countrySet
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Offered", 
         "B" = "Requested", 
         "C" = "Nepotism"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]])
         
         # Applying plotting function
         chart <- LAC_dotsChart(data         = data2plot,
                                target_var   = "value2plot",
                                grouping_var = "country",
                                labels_var   = "labels",
                                colors       = colors4plot,
                                diffOpac     = T,
                                opacities    = opacities4plot)
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 1) {
           h = 21.43905
         }
         if (length(vars4plot[[varSet]]) == 3) {
           h = 52.01605
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 189.7883,
                   h      = h)
         
       })
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 10                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure10.fn <- function(nchart = 10, carib = FALSE) {
  
  # Defining variables to include in plot
  if (carib == F) {
    vars4plot <- c("q4a", "q4b", "q4c", "q4d", "q4e")
  } else {
    vars4plot <- c("CAR_q8a", "CAR_q8b", "CAR_q8d", "CAR_q8e", "CAR_q8f")
  }
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(year == latestYear & country %in% countrySet) %>%
    select(country, all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(across(!country,
                  ~if_else(.x == 99, NA_real_, as.double(.x)))) %>%
    group_by(country) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!country,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(value2plot  = value2plot*100,
           highlighted = if_else(country == mainCountry, "Highlighted", "Regular"),
           labels      = to_percentage.fn(value2plot))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% select(!highlighted) %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Defining colors
  colors4plot <- barsPalette
  names(colors4plot) <- c("Highlighted", "Regular")
  
  # Plotting each panel of Figure 5
  imap(c("A" = vars4plot[1], 
         "B" = vars4plot[2], 
         "C" = vars4plot[3], 
         "D" = vars4plot[4], 
         "E" = vars4plot[5]),
       function(tvar, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% tvar)
         
         # Applying plotting function
         chart <- LAC_barsChart(data           = data2plot,
                                target_var     = "value2plot",
                                grouping_var   = "country",
                                labels_var     = "labels",
                                colors_var     = "highlighted",
                                colors         = colors4plot,
                                direction      = "horizontal")
         
         # Defining height
         if (length(countrySet) == 3) {
           h = 24.60219
         }
         if (length(countrySet) == 4) {
           h = 30.92846
         }
         if (length(countrySet) == 6) {
           h = 43.22956
         }
         if (length(countrySet) > 6) {
           h = 55.17919
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 86.81057,
                   h      = h)
         
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 11                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure11.fn <- function(nchart = 11) {
  
  # Variables to plot
  vars4plot = list("Community"    = c("q1a"), 
                   "Police"       = c("q1d"), 
                   "Executive"    = c("q1b", "q1c"), 
                   "Judiciary"    = c("q1e", "q1f", "q1g"))
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    select(year, all_of(unlist(vars4plot))) %>%
    mutate(
      across(!year,
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      year = paste0("'", str_sub(year, start = -2))
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!year,
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(value = value*100,
           label = paste0(format(round(value, 0),
                                 nsmall = 0),
                          "%"))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Community", 
         "B" = "Police", 
         "C" = "Executive", 
         "D" = "Judiciary"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(str_detect(category, varSet))
         
         # Defining colors4plot
         colors4plot    <- glinesPalette[1:length(vars4plot[[varSet]])]
         naming_vector  <- paste0(varSet, as.character(1:length(vars4plot[[varSet]])))
         
         if (length(vars4plot[[varSet]]) == 1) {
           naming_vector <- str_remove(naming_vector, "1")
         }
         
         names(naming_vector)
         
         # Applying plotting function
         chart <- LAC_lineChart(data           = data2plot,
                                target_var     = "value",
                                grouping_var   = "year",
                                ngroups        = data2plot$category, 
                                labels_var     = "label",
                                colors_var     = "category",
                                colors         = colors4plot,
                                repel          = T
         )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 91.37955,
                   h      = 76.9697)
         
       })
  
} 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 6 - PARAGUAY                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel
figure06_A_PRY.fn <- function(nchart = 6){
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    filter(year == 2021) %>%
    select(year, q1a) %>%
    mutate("Trust in Community" = if_else(q1a == 3 | q1a == 4, 1, 
                                          if_else(!is.na(q1a) & q1a != 99, 0,
                                                  NA_real_))) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    select(`Trust in Community`) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(value = round(value*100,0),
           label = paste0(format(round(value, 0),
                                 nsmall = 0),
                          "%"),
           empty_value = 100 - value) %>%
    pivot_longer(cols = !c(category, label), names_to = "group", values_to = "value") %>%
    mutate(label = if_else(group %in% "empty_value", NA_character_, label),
           x_pos = 1.15)
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart, "A"),
             append    = T,
             row.names = T)
  
  figure6_a <- horizontal_edgebars(data2plot    = data2plot,
                           y_value      = value,
                           x_var        = category,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 0);figure6_a
  # Saving panels
  saveIT.fn(chart  = figure6_a,
            n      = nchart,
            suffix = "A",
            w      = 111.7642,
            h      = 14.40985)
}

# Lower Panel
figure06_B_PRY.fn <- function(nchart = 6){
  
  # Variables to plot
  vars4plot = list("Executive"    = c("q1b", "q1c", "q2b", "q2c"), 
                   "Judiciary"    = c("q1e", "q1f", "q1g", "q2e", "q2f", "q2g"),
                   "Other"        = c("q1d", "q2a", "q2d"))
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(all_of(unlist(vars4plot,
                         use.names = F))) %>%
    mutate(
      across(starts_with("q1"),
             ~case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ NA_real_
             )),
      across(starts_with("q2"),
             ~case_when(
               .x == 1  ~ 0,
               .x == 2  ~ 0,
               .x == 3  ~ 1,
               .x == 4  ~ 1,
               .x == 99 ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "perc") %>%
    mutate(
      perc   = round(perc*100, 0),
      status = if_else(str_detect(category, "q1"), "Positive", "Negative"),
      grp  = case_when(
        str_detect(category, "b|c")   ~ "Executive",
        str_detect(category, "e|f|g") ~ "Judiciary",
        str_detect(category, "a|d")   ~ "Other",
      ),
      perc     = if_else(status == "Positive", perc, -perc),
      label    = to_percentage.fn(abs(perc)),
      row      = str_replace(category, "q1|q2", ""),
      row      = case_when(
        row == "a" ~ "Members of the Legislative",
        row == "b" ~ "Local Government Officers",
        row == "c" ~ "National Government Officers",
        row == "d" ~ "Police Officers",
        row == "e" ~ "Criminal Prosecutors",
        row == "f" ~ "Public Defense Attorneys",
        row == "g" ~ "Judges and Magistrates",
      )
    )
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart, "B"),
             append    = T,
             row.names = T)
    
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette)
  names(colors4plot) <- c("Positive", "Negative")
  
  # Plotting each panel
  imap(c("B1" = "Executive", 
         "B2" = "Judiciary", 
         "B3" = "Other"),
       function(var4plot, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(grp == var4plot)
         
         # The height of the plot depends on the number of categories
         if (panelName == "Judiciary") {
           h = 16.51861
         } else {
           h = 23.19635
         }
         
         # Applying plotting function
         chart <- LAC_divBars(data           = data2plot,
                              target_var     = "perc",
                              grouping_var   = "row",
                              diverging_var  = "status",
                              negative_value = "Negative",
                              colors         = colors4plot,
                              labels_var     = "label")
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 141.6383,
                   h      = h)
       })
}


