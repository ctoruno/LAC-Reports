## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Section I Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 18th, 2022
##
## This version:      February 3rd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 1                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure01.fn <- function(nchart = 1){
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Independent" = c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1"),
    "Judiciary"   = c("CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1"),
    "Media"       = c("CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
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
        category == "CAR_q60_G1" ~ "Censor information that comes \nfrom abroad",
        category == "CAR_q61_G1" ~ "Censor opinions from opposition \ngroups",
        category == "CAR_q60_G2" ~ "Resort to misinformation to shape \npublic opinion in their favor",
        category == "CAR_q64_G2" ~ "Attack or attempt to discredit the \nmedia and civil society organizations\nthat criticize them",
        category == "CAR_q67_G1" ~ "Attack or attempt to discredit \nopposition parties",
        category == "CAR_q67_G2" ~ "Attack or attempt to discredit the \nelectoral system and other \nsupervisory organs", 
        category == "CAR_q64_G1" ~ "Seek to limit the courts' competencies \nand freedom to interpret the law",
        category == "CAR_q66_G1" ~ "Seek to influence the promotion and \nremoval of judges",
        category == "CAR_q65_G2" ~ "Prosecute and convict journalists and \nleaders of civil society organizations     ",
        category == "CAR_q68_G1" ~ "Prosecute and convict members of\nopposition parties                                   ",
        category == "CAR_q65_G1" ~ "Refuse to comply with court rulings \nthat are not in their favor"
      ),
      value2plot = round(value2plot*100,1),
      order_value = case_when(
        category == "CAR_q60_G1" ~ 4,
        category == "CAR_q61_G1" ~ 2,
        category == "CAR_q60_G2" ~ 3,
        category == "CAR_q64_G2" ~ 2,
        category == "CAR_q67_G1" ~ 3,
        category == "CAR_q67_G2" ~ 4, 
        category == "CAR_q64_G1" ~ 1,
        category == "CAR_q66_G1" ~ 2,
        category == "CAR_q65_G2" ~ 1,
        category == "CAR_q68_G1" ~ 1,
        category == "CAR_q65_G1" ~ 3
      )
    )
  
  # Defining color palette
  colors4plot <- countryPalette

  # Defining opacity vector
  opacities4plot <- c(1, rep(0.5, length(countrySet)-1))
  names(opacities4plot) <- countrySet
  
  # Defining shape vector
  shapes4plot <- c(16, rep(18, length(countrySet)-1))
  names(shapes4plot) <- countrySet
  
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
  imap(c("A" = "Independent", 
         "B" = "Judiciary", 
         "C" = "Media"),
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
                                order_var    = "order_value")
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 3 ) {
           h = 47.44707
         }
         
         if (length(vars4plot[[varSet]]) == 4 ) {
           h = 56.23357
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
##    Figure 2                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure02.fn <- function(nchart = 2){
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Independent" = c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1"),
    "Judiciary"   = c("CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1"),
    "Media"       = c( "CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
  )
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(CAR_q59_G1, 
           CAR_q59_G2, 
           unlist(vars4plot, 
                  use.names = F)) %>%
    mutate(
      govSupp = case_when(
        !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
        CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
        CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
        CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
        is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
      ),
      across(!c(CAR_q59_G1, CAR_q59_G2, govSupp),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    group_by(govSupp) %>%
    select(-c(CAR_q59_G1, CAR_q59_G2)) %>%
    filter(!is.na(govSupp)) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!govSupp,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(
      labels = case_when(
        category == "CAR_q60_G1" ~ "Censor information that comes \nfrom abroad",
        category == "CAR_q61_G1" ~ "Censor opinions from opposition \ngroups",
        category == "CAR_q60_G2" ~ "Resort to misinformation to shape \npublic opinion in their favor",
        category == "CAR_q64_G2" ~ "Attack or attempt to discredit the \nmedia and civil society organizations\nthat criticize them",
        category == "CAR_q67_G1" ~ "Attack or attempt to discredit \nopposition parties",
        category == "CAR_q67_G2" ~ "Attack or attempt to discredit the \nelectoral system and other \nsupervisory organs", 
        category == "CAR_q64_G1" ~ "Seek to limit the courts' competencies \nand freedom to interpret the law",
        category == "CAR_q66_G1" ~ "Seek to influence the promotion and \nremoval of judges",
        category == "CAR_q65_G2" ~ "Prosecute and convict journalists and \nleaders of civil society organizations     ",
        category == "CAR_q68_G1" ~ "Prosecute and convict members of\nopposition parties                                   ",
        category == "CAR_q65_G1" ~ "Refuse to comply with court rulings \nthat are not in their favor"
      ),
      value2plot = round(value2plot*100,1),
      
      order_value = case_when(
        category == "CAR_q60_G1" ~ 4,
        category == "CAR_q61_G1" ~ 2,
        category == "CAR_q60_G2" ~ 3,
        category == "CAR_q64_G2" ~ 2,
        category == "CAR_q67_G1" ~ 3,
        category == "CAR_q67_G2" ~ 4, 
        category == "CAR_q64_G1" ~ 1,
        category == "CAR_q66_G1" ~ 2,
        category == "CAR_q65_G2" ~ 1,
        category == "CAR_q68_G1" ~ 1,
        category == "CAR_q65_G1" ~ 3
        
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
  colors4plot <- binPalette
  names(colors4plot) <- data2plot %>% distinct(govSupp) %>% arrange(desc(govSupp)) %>% pull(govSupp)
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Independent", 
         "B" = "Judiciary", 
         "C" = "Media"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]])
         
         # Applying plotting function
         chart <- LAC_dotsChart(data         = data2plot,
                                target_var   = "value2plot",
                                grouping_var = "govSupp",
                                labels_var   = "labels",
                                colors       = colors4plot,
                                order_var    = "order_value")
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 3 ) {
           h = 47.44707
         }
         
         if (length(vars4plot[[varSet]]) == 4 ) {
           h = 56.23357
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
##    Figure 3                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure03.fn <- function(nchart = 3, PAR = F) {
  
  # Defining variables to use
  if (PAR == F) {
    vars4plot <- c("q50", "q51", "q52", "CAR_q73", "CAR_q74")
  } else {
    vars4plot <- c("q50", "q51", "q52")
  }
  
  if (mainCountry %in% westCaribbean_and_guianas.ls){
    data_subset.df <- data_subset.df %>%
      filter(country != "Suriname")
  }
  if (mainCountry %in% southCone.ls){
    if(mainCountry == "Paraguay"){
      
      data_subset.df <- data_subset.df
      
    }else{
      
      data_subset.df <- data_subset.df %>%
      filter(country != "Paraguay")
    }
  }
  if (mainCountry %in% centralAmerica.ls){
    data_subset.df <- data_subset.df %>%
      filter(country != "Nicaragua")
  }
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(year == latestYear) %>%
    select(country, all_of(vars4plot)) %>%
    mutate(
      q52 = case_when(
        q52 == 1 ~ 4,
        q52 == 2 ~ 3,
        q52 == 3 ~ 2,
        q52 == 4 ~ 1,
        q52 == 5 ~ 5,
        q52 == 99 ~ 99
      ),
      across(!country,
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(all_of(vars4plot),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos"),
      across(all_of(vars4plot),
             ~if_else(.x == 5, 1, 0),
             .names = "{.col}_neither")
    ) %>%
    group_by(country) %>%
    summarise(
      across(c(ends_with("_pos"),
               ends_with("_neg"),
               ends_with("_neither")),
             sum,
             na.rm = T)
    ) %>%
    mutate(
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_pos"),
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_neg")
    ) %>%
    select(-ends_with("_neither"))
  
  # We need to dynamically generate the totals for each variable
  data2plot <- map_dfr(vars4plot,
                       function(categories) {
                         
                         data2plot %>%
                           select(country, starts_with(categories)) %>%
                           mutate(
                             "{categories}_total" := rowSums(across(starts_with(categories)))
                           ) %>%
                           rename(total = ends_with("_total")) %>%
                           pivot_longer(!c(country, total),
                                        values_to = "abs_value",
                                        names_to  = "category") %>%
                           mutate(
                             perc    = round((abs_value/total)*100, 
                                             0),
                             status  = case_when(
                               str_detect(category, "_neither") ~ "Neutral",
                               str_detect(category, "_neg")     ~ "Negative",
                               str_detect(category, "_pos")     ~ "Positive"
                             ),
                             status     = factor(status, levels = c("Negative", "Positive", "Neutral")),
                             perc       = if_else(str_detect(category, "_neg"), 
                                                  perc*-1, 
                                                  perc),
                             label      = paste0(format(abs(perc),
                                                        nsmall = 0),
                                                 "%"),
                             label      = if_else(status == "Neutral", NA_character_, label), 
                             group      = str_replace_all(category, "_pos|_neg|_neither", ""),
                             lab_status = case_when(
                               str_detect(category, "_pos") ~ "POS",
                               str_detect(category, "_neg") ~ "NEG"
                             )
                           )
                       }) %>%
    mutate(country = if_else(country == "Bahamas",
                             "The Bahamas",
                             country)
    ) %>%
    group_by(country, group, lab_status) %>%
    mutate(lab_pos = sum(perc))
  
  # Specifying a custom order for West Caribbean
  if (mainCountry %in% westCaribbean_and_guianas.ls) {
    c.order <- T
    ext     <- T
  } else {
    c.order <- F
    ext     <- F
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
  if (mainCountry %in% eastCaribbean.ls) {
    ext <- T
  }
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% select(country, group, status, perc, label) %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette, "#A6A8AA")
  names(colors4plot) <- c("Positive", "Negative", "Neutral")
  
  # The height of the plot depends on the number of countries
  if (length(countrySet) == 3) {
    h = 15.464229
  }
  if (length(countrySet) == 4) {
    h = 20.736125
  }
  if (length(countrySet) == 6) {
    h = 31.279918
  }
  if (length(countrySet) > 6) {
    h = 41.823711
  }
  
  # Defining Panel order
  if (PAR == F) {
    vars4plot <- c("C" = "q50", 
                   "D" = "q51", 
                   "E" = "q52",
                   "A" = "CAR_q73",
                   "B" = "CAR_q74")
  } else {
    vars4plot <- c("A" = "q50", 
                   "B" = "q51", 
                   "C" = "q52")
  }
  
  # Plotting each figure panel
  imap(vars4plot,
       function(var4plot, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(group %in% var4plot)
         
         # Applying plotting function
         chart <- LAC_divBars(data           = data2plot,
                              target_var     = "perc",
                              grouping_var   = "country",
                              diverging_var  = "status",
                              negative_value = "Negative",
                              colors         = colors4plot,
                              labels_var     = "label",
                              lab_pos        = "lab_pos",
                              custom_order   = c.order,
                              order_var      = "order_var",
                              extreme        = ext)
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 100.8689,
                   h      = h)
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 4                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure04.fn <- function(nchart = 4) {
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    select(year,
           q46c_G2, q46f_G2, q46g_G2, q46c_G1, q46e_G2,
           q46d_G2, q46f_G1, q46a_G2,
           q46d_G1, q46e_G1, q46h_G2) %>%
    mutate(
      across(!c(year),
             ~ case_when(
               .x == 1 | .x == 2 ~ 1,
               .x == 3 | .x == 4 ~ 0
             ))
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
    maxyear <- minyear + 1
  }
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  if (mainCountry == "Haiti") {
    x.axis.values <- c(2021, 2022)
    x.axis.labels <- c("'21", "'22")
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
  
  # Plotting each panel of Figure 5
  imap(c("A" = "q46c_G2", "B" = "q46f_G2", "C" = "q46g_G2", "D" = "q46c_G1", "E" = "q46e_G2",
         "F" = "q46d_G2", "G" = "q46f_G1", "H" = "q46a_G2",
         "I" = "q46d_G1", "J" = "q46e_G1", "K" = "q46h_G2"),
       function(var4plot, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% var4plot)
         
         # Defining colors4plot
         colors4plot <- mainCOLOR
         names(colors4plot) <- var4plot
         
         # Applying plotting function
         chart <- LAC_lineChart(data           = data2plot,
                                target_var     = "value",
                                grouping_var   = "year",
                                ngroups        = 1, 
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
                   w      = 90.67663,
                   h      = 45.68977)
         
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 5                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure05.fn <- function(nchart = 5) {
  
  # Defining variables to include in plot
  vars4plot <- list("Expression"    = c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2"),
                    "Participation" = c("q46d_G2", "q46f_G1", "q46a_G2"),
                    "Election"      = c("q46d_G1", "q46e_G1"),
                    "Religion"      = c("q46h_G2"))
  
  # Country names or country codes?
  if (length(countrySet) > 4) {
    data_subset.df <- data_subset.df %>%
      mutate(country_name = country,
             country      = country_code)
  } else {
    data_subset.df <- data_subset.df %>%
      mutate(country_name = country)
  }
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(year == latestYear) %>%
    select(country, 
           country_name, 
           all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(across(!c(country, country_name),
                  ~if_else(.x == 1 | .x == 2, 1, 
                           if_else(!is.na(.x) & .x != 99, 0, 
                                   NA_real_)))) %>%
    group_by(country, country_name) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!c(country, country_name),
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(value2plot  = value2plot*100,
           highlighted = if_else(country_name == mainCountry, "Highlighted", "Regular"),
           labels      = to_percentage.fn(value2plot))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% select(!highlighted) %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Specifying a custom order for West Caribbean
  if (mainCountry %in% c(centralAmerica.ls, eastCaribbean.ls)) {
    c.order <- T
  } else {
    c.order <- F
  }
  if (mainCountry %in% eastCaribbean.ls) {
    data2plot <- data2plot %>%
      mutate(
        order_var = case_when(
          country == "BRB" ~ 1,
          country == "DMA" ~ 2,
          country == "GRD" ~ 3,
          country == "LCA" ~ 4,
          country == "VCT" ~ 5,
          country == "TTO" ~ 6
        )
      )
  }
  if (mainCountry %in% centralAmerica.ls) {
    data2plot <- data2plot %>%
      mutate(
        order_var = case_when(
          country == "BLZ" ~ 1,
          country == "CRI" ~ 2,
          country == "SLV" ~ 3,
          country == "GUA" ~ 4,
          country == "HND" ~ 5,
          country == "NIC" ~ 6,
          country == "PAN" ~ 7
        )
      )
  }
  
  if (! mainCountry %in% c(centralAmerica.ls, eastCaribbean.ls)) {
    data2plot <- data2plot %>%
      mutate(order_var = NULL)
  }
  
  # Defining colors
  colors4plot <- barsPalette
  names(colors4plot) <- c("Highlighted", "Regular")
  
  if (mainCountry %in% c(eastCaribbean.ls, 
                         westCaribbean_and_guianas.ls, 
                         southCone.ls, 
                         centralAmerica.ls)){
    exp <- TRUE
  } else {
    exp <- FALSE
  }
  
  # Plotting each panel of Figure 5
  imap(c("A" = "q46c_G2", "B" = "q46f_G2", "C" = "q46g_G2", "D" = "q46c_G1", "E" = "q46e_G2",
         "F" = "q46d_G2", "G" = "q46f_G1", "H" = "q46a_G2",
         "I" = "q46d_G1", "J" = "q46e_G1", 
         "K" = "q46h_G2"),
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
                                direction      = "vertical",
                                expand         = exp,
                                custom_order   = c.order,
                                order_var      = "order_var"
         )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 90.67663,
                   h      = 43.58102)
         
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 6                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure06.fn <- function(nchart = 6) {
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    select(country, year, q43_G2) %>% 
    mutate(
      q43_G2 = if_else(q43_G2 == 3, 1, 
                       if_else(!is.na(q43_G2) & q43_G2 != 99, 0, 
                               NA_real_))
    ) %>%     
    group_by(year, country) %>%
    summarise(value2plot = mean(q43_G2, na.rm = T)) %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           label = if_else(country == mainCountry, label, NA_character_)) %>%
    filter(year >= 2014 & country != "Nicaragua") 
  
  # Pulling minimum and maximum available year
  minyear <- min(data2plot %>% pull(year))
  if (minyear %% 2 != 0) {
    minyear <- minyear - 1
  }
  maxyear <- max(data2plot %>% pull(year))
  if (maxyear %% 2 != 0) {
    maxyear <- minyear + 1
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
  
  # Defining colors4plot
  colors4plot <- countryPalette

  # Defining alphas4plot
  alphas4plot <- c(1, rep(0.5, length(comparison_countries.ls)))
  names(alphas4plot) <- countrySet
  
  # Applying plotting function
  chart <- LAC_lineChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "year",
                         ngroups        = data2plot$country, 
                         labels_var     = "label",
                         colors_var     = "country",
                         colors         = colors4plot,
                         repel          = T,
                         transparency   = T,
                         transparencies = alphas4plot,
                         custom.axis    = T,
                         x.breaks       = x.axis.values,
                         x.labels       = x.axis.labels,
                         sec.ticks      = sec.ticks
  )
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 189.7883,
            h      = 149.7219)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 4 - PRY                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure04_PRY.fn <- function(nchart = 4){
  
  # Defining the variables to use in the plot
  vars4plot <- c("q45a_G1", "q45b_G1", "q45c_G1")
  
  # Defining the data to be used in the plot
  data2plot <- data_subset.df %>%
    filter(year == latestYear) %>%
    select(country, all_of(unlist(vars4plot, 
                                  use.names = F))) %>%
    mutate(
      country = if_else(country == "Bahamas", "The Bahamas", country),
      across(!country,
             ~if_else(.x == 99, 
                      NA_real_, 
                      as.double(.x)))
    ) 
  
  data2plot <- lapply(vars4plot, 
                      function(target) {
                        
                        as.data.frame(table(data2plot[["country"]], data2plot[[target]])) %>%
                          group_by(Var1) %>%
                          mutate(value2plot = (Freq/sum(Freq))*100,
                                 labels     = to_percentage.fn(value2plot),
                                 group      = target,
                                 stack_y    = cumsum(value2plot) - (value2plot/2))
                        
                      }) %>%
    bind_rows(.) %>%
    rename(country  = Var1,
           category = Var2) %>%
    mutate(
      category = case_when(
        category == 1 ~ "Very Likely",
        category == 2 ~ "Likely",
        category == 3 ~ "Unlikely",
        category == 4 ~ "Very Unlikely",
      )
    ) 
  
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
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Defining colors4plot
  colors4plot <- c("Very Likely"   = "#2a2a94",
                   "Likely"        = "#a90099",
                   "Unlikely"      = "#3273ff",
                   "Very Unlikely" = "#43a9a7")
  
  # Transforming 
  data2plot$category <- factor(data2plot$category, 
                               levels = c("Very Unlikely",
                                          "Unlikely",
                                          "Likely",
                                          "Very Likely"))
  
  # Plotting each panel of Figure 5
  imap(c("A" = "q45a_G1", 
         "B" = "q45b_G1", 
         "C" = "q45c_G1"),
       function(tvar, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(group %in% tvar)
         
         # Applying plotting function
         chart <- LAC_barsChart(data           = data2plot,
                                target_var     = "value2plot",
                                grouping_var   = "country",
                                labels_var     = "labels",
                                colors_var     = "category",
                                colors         = colors4plot,
                                direction      = "horizontal",
                                stacked        = T,
                                lab_pos        = "stack_y",
                                custom_order   = c.order,
                                order_var      = "order_var"
         )
         
         # Defining height
         if (length(table(data2plot$country)) == 3) {
           h = 43.58102
         }
         
         if (length(table(data2plot$country)) == 6) {
           h = 54.12481
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 169.7883,
                   h      = h)
         
       })
  
}


