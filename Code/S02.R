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

figure09.fn <- function(){
  
  nchart = 9
  
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
        category == "q2a"     ~ "Members of Parliament/Congress",
        category == "q2c"     ~ "Officers working in the\nnational government",
        category == "q2b"     ~ "Officers working in the\nlocal government",
        category == "q2g"     ~ "Judges and magistrates",
        category == "q2e"     ~ "The prosecutors in charge\nof criminal investigations",
        category == "q2f"     ~ "Public defense attorneys",
        category == "q2d"     ~ "Police officers", 
        category == "CAR_q6h" ~ "Members of the Armed Forces",
        category == "CAR_q6i" ~ "Tax/revenues officers",
        category == "CAR_q6j" ~ "Customs officers",
        category == "CAR_q6k" ~ "Public utility company officers\nand employees",
        category == "CAR_q6l" ~ "Doctors and nurses in\npublic hospitals", 
        category == "CAR_q6m" ~ "Teachers in public schools", 
        category == "CAR_q6n" ~ "Land registry officers", 
        category == "CAR_q6o" ~ "Car registration officers", 
        category == "CAR_q6p" ~ "The news media", 
        category == "CAR_q6q" ~ "Political parties"
      ),
      value2plot = round(value2plot*100,1)
    )
  
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
##    Figure 10                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 11                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure11.fn <- function(){
  
  nchart = 11
  
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
        category == "CAR_q2b" ~ "A public officer asking for a bribe to\nspeed up administrative procedures",
        category == "CAR_q2f" ~ "A law enforcement officer asking for a\nbribe",
        category == "CAR_q2g" ~ "A company official asking for a bribe\nfrom a job applicant",
        category == "CAR_q2c" ~ "A private citizen offering a bribe to a\npublic official to speed up administrative procedures",
        category == "CAR_q2a" ~ "A public officer being recruited on the\nbasis of family ties and friendship networks",
        category == "CAR_q2d" ~ "An elected official taking public funds\nfor private use",
        category == "CAR_q2e" ~ "An elected official using stolen public\nfunds to assist his or her community"
      ),
      value2plot = round(value2plot*100,1)
    )
  
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



