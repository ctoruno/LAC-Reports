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
## This version:      March 23rd, 2023
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
                              NA_real_)))
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
                          "%")) %>%
    filter(year >= 2014)
  
  # Pulling minimum and maximum available year
  minyear <- min(data2plot %>% pull(year))
  if (minyear %% 2 != 0) {
    minyear <- minyear - 1
  }
  maxyear <- max(data2plot %>% pull(year))
  if (maxyear %% 2 != 0) {
    maxyear <- maxyear + 1
  }
  
  if (mainCountry == "Haiti") {
    x.axis.values <- c(2021, 2022)
    x.axis.labels <- c("'21", "'22")
  }
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
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
                                repel          = F,
                                custom.axis    = T,
                                x.breaks       = x.axis.values,
                                x.labels       = x.axis.labels,
                                sec.ticks      = sec.ticks
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
        category == "q2a"     ~ "Members of the \nlegislature                        ",
        category == "q2c"     ~ "National government\nofficers",
        category == "q2b"     ~ "Local government \nofficers",
        category == "q2g"     ~ "Judges and magistrates",
        category == "q2e"     ~ "Prosecutors in charge of\ncriminal investigations   ",
        category == "q2f"     ~ "Public defense attorneys",
        category == "q2d"     ~ "Police officers", 
        category == "CAR_q6h" ~ "Members of the armed \nforces",
        category == "CAR_q6i" ~ "Tax/revenue officers    ",
        category == "CAR_q6j" ~ "Customs officers",
        category == "CAR_q6k" ~ "Public utility company \nofficers",
        category == "CAR_q6l" ~ "Doctors and nurses in\npublic hospitals", 
        category == "CAR_q6m" ~ "Teachers in public \nschools", 
        category == "CAR_q6n" ~ "Land registry officers", 
        category == "CAR_q6o" ~ "Car registration agency  \nofficers", 
        category == "CAR_q6p" ~ "News media                  ", 
        category == "CAR_q6q" ~ "Political parties               "
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
  
  if (mainCountry == "United States"){
    data2plot <- data2plot %>%
      filter(labels != "Members of the armed \nforces")
  }
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
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
  
  # Defining shape vector
  shapes4plot <- c(16, rep(18, length(countrySet)-1))
  names(shapes4plot) <- countrySet
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Media", 
         "B" = "Government", 
         "C" = "Public Admin",
         "D" = "Judiciary"),
       function(varSet, panelName) {
         
         if (varSet == "Judiciary") {
           
           # Filtering data2plot to leave the variable for each panel
           data2plot <- data2plot %>%
             filter(category %in% vars4plot[[varSet]]) %>%
             arrange(order_var) %>% 
             filter(if_else(country == "Bahamas", labels != "Police officers", T, F))
           
         } 
         
         if(varSet == "Government") {
           
           # Filtering data2plot to leave the variable for each panel
           data2plot <- data2plot %>%
             filter(category %in% vars4plot[[varSet]]) %>%
             arrange(order_var) %>% 
             filter(if_else(country == "Bahamas", labels != "Members of the \nlegislature                        ", T, F))
           
         
         } else {
           
           # Filtering data2plot to leave the variable for each panel
           data2plot <- data2plot %>%
             filter(category %in% vars4plot[[varSet]]) %>%
             arrange(order_var)

         }
         
         # Applying plotting function
         chart <- LAC_dotsChart(data         = data2plot,
                                target_var   = "value2plot",
                                grouping_var = "country",
                                labels_var   = "labels",
                                colors       = colors4plot,
                                diffOpac     = T,
                                opacities    = opacities4plot,
                                diffShp      = T,
                                shapes       = shapes4plot, 
                                order_var    = "order_var")
         
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
        category == "CAR_q2b" ~ "A public officer asking for a bribe to \nspeed up administrative procedures",
        category == "CAR_q2f" ~ "A law enforcement officer (police, \ncustoms, immigration, civil guard, \nmilitary police) asking for a bribe",
        category == "CAR_q2g" ~ "A company official asking for a bribe\nfrom a job applicant",
        category == "CAR_q2c" ~ "A private citizen offering a bribe \nto a public official to speed up \nadministrative procedures                 ",
        category == "CAR_q2a" ~ "A public officer being recruited on \nthe basis of family ties and \nfriendship networks",
        category == "CAR_q2d" ~ "An elected official taking public funds\nfor private use",
        category == "CAR_q2e" ~ "An elected official using stolen public \nfunds to assist his or her community"
      ),
      value2plot = round(value2plot*100,1),
      order_var  = case_when(
        category == "CAR_q2b"  ~ 2,
        category == "CAR_q2f"  ~ 3,
        category == "CAR_q2g"  ~ 1,
        category == "CAR_q2c"  ~ 1,
        category == "CAR_q2a"  ~ 1,
        category == "CAR_q2d"  ~ 3,
        category == "CAR_q2e"  ~ 2
      )
    )
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
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
  
  # Defining shape vector
  shapes4plot <- c(16, rep(18, length(countrySet)-1))
  names(shapes4plot) <- countrySet
  
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
                                opacities    = opacities4plot,
                                diffShp      = T,
                                shapes       = shapes4plot,
                                order_var    = "order_var")
         
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
    vars4plot <- c("CAR_q8a", "CAR_q8b", "CAR_q8d", "CAR_q8e", "CAR_q8f", "CAR_q8i")
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
           highlighted = if_else(country == mainCountry, 
                                 "Highlighted", 
                                 "Regular"),
           labels      = to_percentage.fn(value2plot),
           country     = if_else(country %in% "Bahamas", 
                                 "The Bahamas", 
                                 country)) %>%
    mutate(labels = if_else(country == "Haiti" & category == "CAR_q8e", "13%", labels)) # We need to keep consistency with the corruption report, the value is 12.5000000.
  
  
  # Specifying a custom order for West Caribbean
  if (mainCountry %in% westCaribbean_and_guianas.ls) {
    c.order <- T
  } else {
    c.order <- F
  }
  if (mainCountry %in% westCaribbean_and_guianas.ls) {
    data2plot <- data2plot %>%
      mutate(
        order_var = case_when(
          country == "The Bahamas"        ~ 1,
          country == "Dominican Republic" ~ 2,
          country == "Guyana"             ~ 3,
          country == "Haiti"              ~ 4,
          country == "Jamaica"            ~ 5
        )
      )
  }
    
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% select(!highlighted) %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Defining colors
  colors4plot <- barsPalette
  names(colors4plot) <- c("Highlighted", "Regular")
  
  # Plotting each panel of Figure 5
  panelVector <- c("A" = vars4plot[1], 
                   "B" = vars4plot[2], 
                   "C" = vars4plot[3], 
                   "D" = vars4plot[4], 
                   "E" = vars4plot[5])
  
  if (carib == T) {
    panelVector <- c(panelVector, "F" = vars4plot[6])
  }
  
  imap(panelVector,
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
                                direction      = "horizontal",
                                custom_order   = c.order,
                                order_var      = "order_var")
         
         # Defining height
         if (length(countrySet) == 3 & mainCountry != "Paraguay") {
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
         if (mainCountry == "Paraguay") {
           h = 35.14598
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
  if (mainCountry %in% centralAmerica.ls & mainCountry != "Costa Rica") {
    vars4plot = list("Community"    = c("q1a", "EXP_q1a"), 
                     "Police"       = c("q1d"), 
                     "Executive"    = c("q1b", "q1c"), 
                     "Judiciary"    = c("q1e", "q1f", "q1g"))
    
  } else {
    vars4plot = list("Community"    = c("q1a"), 
                     "Police"       = c("q1d"), 
                     "Executive"    = c("q1b", "q1c"), 
                     "Judiciary"    = c("q1e", "q1f", "q1g"))
  }
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    select(year, all_of(unlist(vars4plot))) %>%
    mutate(
      across(!year,
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))
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
                          "%")) %>%
    filter(year >= 2014)
  
  # Variables to plot
  if (mainCountry %in% centralAmerica.ls & mainCountry != "Costa Rica") {
    data2plot <- data2plot %>%
      filter(!(category == "Community1" & year == 2021))
  }
  
  # Pulling minimum and maximum available year
  minyear <- min(data2plot %>% pull(year))
  if (minyear %% 2 != 0) {
    minyear <- minyear - 1
  }
  maxyear <- max(data2plot %>% pull(year))
  if (maxyear %% 2 != 0) {
    maxyear <- maxyear + 1
  }
  
  if (mainCountry == "Haiti") {
    x.axis.values <- c(2021, 2022)
    x.axis.labels <- c("'21", "'22")
  }
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
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
                                repel          = F,
                                custom.axis    = T,
                                x.breaks       = x.axis.values,
                                x.labels       = x.axis.labels,
                                sec.ticks      = sec.ticks
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
##    Figure 5 - PARAGUAY                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel
figure05_A_PRY.fn <- function(nchart = 5){
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    filter(year == latestYear) %>%
    select(year, q1a) %>%
    mutate("Trust in Community" = if_else(q1a == 1 | q1a == 2, 1, 
                                          if_else(!is.na(q1a) & q1a != 99, 0,
                                                  NA_real_))) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    select(`Trust in Community`) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "Yes") %>%
    mutate(Yes = round(Yes*100,0),
           label = paste0(format(round(Yes, 0),
                                 nsmall = 0),
                          "%"),
           No = 100 - Yes) %>%
    pivot_longer(cols = !c(category, label), names_to = "group", values_to = "value") %>%
    mutate(label = if_else(group %in% "empty_value", NA_character_, label),
           x_pos = if_else(group == "Yes", 1, 0)) %>%
    mutate(group = case_when(
      group == "Yes" ~ "58% Yes",
      group == "No"  ~ "42% No"
    ))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart, "A"),
             append    = T,
             row.names = T)
  
  figure6_a <- ggplot(data2plot, aes(fill = reorder(group, -x_pos), values = value)) +
    geom_waffle(color = "white", size = 1, n_rows = 5) + 
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_fill_manual("", 
                      values = c("58% Yes" = "#003b8a",
                                 "42% No"  = "#fa4d57")) +
    coord_equal() +
    theme_enhance_waffle() +
    #guides(fill = guide_legend(override.aes = list(shape  = 21,
    #                                               size   = 1))) +
    theme(panel.spacing = unit(0.25, "cm"),
          strip.background = element_blank(),
          strip.text = element_text(size = 11.5, 
                                    hjust = 0,
                                    family = "Lato Full",
                                    face = "italic"),
          legend.position = "left",
          plot.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(), 
          panel.border = element_blank(), 
          legend.text = element_text(hjust = 0,
                                     family = "Lato Full",
                                     size = 7),
          legend.key.width = unit(0.35,"cm"), 
          legend.key.height = unit(0.35,"cm"), 
          legend.background = element_blank(), 
          legend.box.background = element_blank(), 
          plot.margin = margin(-25,0,0,0), legend.box.just = "center");figure6_a
  
  # Saving panels
  saveIT.fn(chart  = figure6_a,
            n      = nchart,
            suffix = "A",
            w      = 111.7642,
            h      = 65.72298)
}

# Lower Panel
figure05_B_PRY.fn <- function(nchart = 5){
  
  # Variables to plot
  vars4plot = list("Trust"      = c("q1b", "q1c", "q1d", "q1e", "q1f", "q1g", "q1h", "q1i", "q1j"),
                   "Corruption" = c("q2a", "q2b", "q2c", "q2d", "q2e", "q2f", "q2g"))
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(year == latestYear) %>%
    select(country, all_of(unlist(vars4plot,
                                  use.names = F))) %>%
    mutate(
      across(starts_with("q1"),
             ~case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0
             )),
      across(starts_with("q2"),
             ~case_when(
               .x == 1  ~ 0,
               .x == 2  ~ 0,
               .x == 3  ~ 1,
               .x == 4  ~ 1
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
        str_detect(category, "q1") ~ "Trust",
        str_detect(category, "q2") ~ "Corruption"
      ),
      order_var = case_when(
        str_detect(category, "a") ~ 1,
        str_detect(category, "b") ~ 3,
        str_detect(category, "c") ~ 2,
        str_detect(category, "d") ~ 7,
        str_detect(category, "e") ~ 4,
        str_detect(category, "f") ~ 6,
        str_detect(category, "g") ~ 5,
        str_detect(category, "h") ~ 8,
        str_detect(category, "i") ~ 10,
        str_detect(category, "j") ~ 9
      ),
      labels = case_when(
        str_detect(category, "a") ~ "Members of the legislature",
        str_detect(category, "b") ~ "Local government officers",
        str_detect(category, "c") ~ "National government officers",
        str_detect(category, "d") ~ "Police officers",
        str_detect(category, "e") ~ "Prosecutors",
        str_detect(category, "f") ~ "Public defense attorneys",
        str_detect(category, "g") ~ "Judges and magistrates",
        str_detect(category, "h") ~ "Civil servants",
        str_detect(category, "i") ~ "News media",
        str_detect(category, "j") ~ "Political parties"
      )
    )
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart, "B"),
             append    = T,
             row.names = T)
  
  # Defining color palette
  colors4plot <- countryPalette
  
  # Defining opacity vector
  opacities4plot <- c(1, rep(0.5, length(countrySet)-1))
  names(opacities4plot) <- countrySet
  
  # Defining shape vector
  shapes4plot <- c(16, rep(18, length(countrySet)-1))
  names(shapes4plot) <- countrySet
  
  # Plotting each panel
  imap(c("B1" = "Trust", 
         "B2" = "Corruption"),
       function(var4plot, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(grp == var4plot) %>%
           rename(value2plot = perc)
         
         # The height of the plot depends on the number of categories
         if (var4plot == "Trust") {
           h = 70.29196
         }
         if (var4plot == "Corruption") {
           h = 52.01605
         }
         
         # Applying plotting function
         chart <- LAC_dotsChart(data         = data2plot,
                                target_var   = "value2plot",
                                grouping_var = "country",
                                labels_var   = "labels",
                                colors       = colors4plot,
                                order_var    = "order_var",
                                diffOpac     = T,
                                opacities    = opacities4plot,
                                shapes       = shapes4plot, diffShp = T)
         
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
##    Figure 18 - US                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure18_US.fn <- function(nchart = 18) {
  alpha <- 0.05
  # Variables to plot
  vars4plot <- list("Corruption" = c("q2a","q2d","q2b", "q2c","q2e", "q2f", "q2g"),
                    "Trust"      = c("q1a","q1d","q1b", "q1c","q1e", "q1f", "q1g"))
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    filter(year == latestYear) %>%
    mutate(party = case_when(
      paff3 == "The Democratic Party" ~ "Democratic Party",
      paff3 == "The Republican Party" ~ "Republican Party"
    )) %>%
    select(party, all_of(unlist(vars4plot, 
                                use.names = F))) %>%
    mutate(
      across(starts_with("q2"),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_))),
      across(starts_with("q1"),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    group_by(party) %>%
    mutate(obs = n()) %>%
    ungroup() %>%
    group_by(party) %>%
    summarise(
      across(c(all_of(unlist(vars4plot, 
                             use.names = F))),
             mean, 
             na.rm = T,
             .names = "{col}_mean"),
      across(c(all_of(unlist(vars4plot, 
                             use.names = F))),
             sd,
             na.rm = T,
             .names = "{col}_sd"),
      n_obs = mean(obs, na.rm = T),
      n_obs = as.character(n_obs)
    ) %>%
    drop_na() %>%
    pivot_longer(!c(party,n_obs),
                 names_to      = c("category", "stat"),
                 names_pattern = "(.*)_(.*)",
                 values_to     = "value") %>%
    pivot_wider(c(category,party,n_obs),
                names_from  = stat,
                values_from = value) %>%
    mutate(
      n_obs  = as.numeric(n_obs),
      labels = case_when(
        category == "q2a" ~ "Members of congress",
        category == "q2d" ~ "Police officers",
        category == "q2b" ~ "Local government officers",
        category == "q2c" ~ "National government officers",
        category == "q2e" ~ "Prosecutors",
        category == "q2f" ~ "Public defense attorneys",
        category == "q2g" ~ "Judges and magistrates",
        category == "q1a" ~ "People living in their country",
        category == "q1d" ~ "Police officers",
        category == "q1b" ~ "Local government officers",
        category == "q1c" ~ "National government officers",
        category == "q1e" ~ "Prosecutors",
        category == "q1f" ~ "Public defense attorneys",
        category == "q1g" ~ "Judges and magistrates"
        
        
      ),
      lower = mean - qt(1- alpha/2, (n() - 1))*sd/sqrt(n_obs),
      upper = mean + qt(1- alpha/2, (n() - 1))*sd/sqrt(n_obs)
    ) %>%
    rename(values = mean) %>%
    mutate(batch = if_else(str_detect(category, "q1"), "trust", "corruption")) %>%
    mutate(order_values =
             case_when(
               category %in% c("q1a", "q2a") ~ 1,
               category %in% c("q1d", "q2d") ~ 2,
               category %in% c("q1b", "q2b") ~ 3,
               category %in% c("q1c", "q2c") ~ 4,
               category %in% c("q1e", "q2e") ~ 5,
               category %in% c("q1g", "q2g") ~ 1,
               category %in% c("q1f", "q2f") ~ 6
             ))
  
  # Defining color palette
  colors4plot <- binPalette
  names(colors4plot) <- data2plot %>% distinct(party) %>% arrange(party) %>% pull(party)
  
  imap(c("A" = "corruption",
         "B" = "trust"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(batch %in% varSet)
         
         # Applying plotting function
         chart <- errorDotsChart(data2plot = data2plot,
                                 labels = "labels",
                                 group = "party",
                                 category = "category",
                                 values = values,
                                 lower = lower,
                                 upper = upper, 
                                 colors4plot = colors4plot, 
                                 custom_order = F, 
                                 order_values = order_values)
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 189.7883,
                   h      = 54.12481)
         
       })
}
