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
## This version:      November 21st, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 8                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure08.fn <- function() {
  
  nchart = 8
  
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
                      if_else(!is.na(.x), 0, NA_real_))),
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
##    Figure 9                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 10                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 11                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 12                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure12.fn <- function() {
  
  nchart = 12
  
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
                      if_else(!is.na(.x), 0, NA_real_))),
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
  
  # Plotting each panel of Figure 8
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



