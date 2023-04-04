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
  "tidyverse"
  
))

# Loading data
master_data.df <- bind_rows(read_dta("Data/LAC - Merged.dta"),
                            read_dta("Data/LAC - Merged (with CA).dta") %>% 
                              filter(year == 2022))

# Defining set of variables
ffreedoms      <- c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2",
                    "q46d_G2", "q46f_G1", "q46a_G2",
                    "q46d_G1", "q46e_G1", "q46h_G2")
accountability <- c("q43_G2")
corruption     <- c("q2a", "q2d", "q2b", "q2c", "q2e", "q2f", "q2g")
trust          <- c("q1a", "q1d", "q1b", "q1c", "q1e", "q1f", "q1g")
security       <- c("q9")
cjustice       <- c("q49a", "q49b_G2", "q49e_G2", "q49c_G2", "q49e_G1", "q49c_G1", "q49b_G1")
effectiveness  <- c("q48f_G2", "q48h_G1", "q48g_G2")
police         <- c("q48c_G2",
                    "q48a_G2", "q48b_G2", "q48b_G1",
                    "q48a_G1", "q48c_G1", "q48d_G2",
                    "q48d_G1")
authoritarian  <- c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1", "CAR_q66_G1", 
                    "CAR_q65_G1", "CAR_q64_G1", "CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
ctolerance     <- c("CAR_q2c", "CAR_q2b", "CAR_q2f", "CAR_q2g", "CAR_q2a", "CAR_q2d", "CAR_q2e")
victimization  <- c("EXP_q24a_G1", "EXP_q24b_G1", "EXP_q24c_G1", "EXP_q24d_G1", "EXP_q24a_G2", "EXP_q24b_G2",
                    "EXP_q24c_G2", "EXP_q24d_G2", "EXP_q24f_G2", "EXP_q24g_G2", "EXP_q23f_G1")

VIP_vars <- c(ffreedoms, accountability, corruption, trust, security, cjustice, effectiveness, police)

# Subsetting for variables of interest
data_subset.df <- master_data.df %>%
  select(country, country_code, year,
         all_of(VIP_vars)) %>%
  mutate(
    across(all_of(ffreedoms),
           ~ case_when(
             .x == 1 | .x == 2 ~ 1,
             .x == 3 | .x == 4 ~ 0
           )),
    across(all_of(accountability),
           ~if_else(.x == 3, 1, 
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    across(all_of(corruption),
           ~if_else(.x == 3 | .x == 4, 1,
                    if_else(!is.na(.x)  & .x != 99, 0, 
                            NA_real_))),
    across(all_of(trust),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    across(all_of(security),
           ~case_when(
             .x == 1 | .x == 2    ~ 1,
             .x == 3 | .x == 4    ~ 0
           )),
    across(all_of(cjustice),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, NA_real_))),
    across(all_of(effectiveness),
           ~if_else(.x == 1 | .x == 2, 1,
                    if_else(!is.na(.x) & .x != 99, 0, 
                            NA_real_))),
    across(all_of(police),
           ~case_when(
             .x == 1  ~ 1,
             .x == 2  ~ 1,
             .x == 3  ~ 0,
             .x == 4  ~ 0))
  ) 

# What are the top 2 years for each country?
top2_years.df <- data_subset.df %>%
  group_by(country, year) %>%
  summarise(year = first(year)) %>%
  slice_max(year, n = 2) %>%
  mutate(n = row_number(),
         n = paste0("year", n)) %>%
  pivot_wider(country,
              names_from  = n,
              values_from = year)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## STEP 6: Perform a t-test for changes across time within WJP’s data                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining the data frame to use for the t-test across time
diffmeans.df <- data_subset.df %>%
  filter(! country %in% c("Chile", "Paraguay", "Mexico", "Uruguay",  "Venezuela")) %>%
  left_join(top2_years.df,
            by = "country") %>%
  filter(year %in% c(year1, year2)) %>%
  mutate(group = if_else(year == year1, 
                         "EndYear", 
                         "StartYear"))

# Looping across variables
lapply(VIP_vars, 
       function(target){
         
         # Looping across countries
         results_bycntry <- map_dfr(unique(diffmeans.df$country),
                                    function(cntry){
                                      
                                      print(paste("Diff. in means for variaable:",
                                                  target,
                                                  "in country:",
                                                  cntry))
                                      
                                      # Subsetting the country
                                      sset  <- diffmeans.df %>%
                                        filter(country == cntry) %>%
                                        select(country, group, all_of(target))
                                      
                                      cond1 <- mean(sset %>% filter(group == "StartYear") %>% pull(target), na.rm = T)
                                      cond2 <- mean(sset %>% filter(group == "EndYear") %>% pull(target), na.rm = T)
                                      
                                      if(!is.na(cond1) & !is.na(cond2)){
                                        
                                        # Estimating the t-test between groups
                                        obj <- t.test(sset %>% filter(group == "StartYear") %>% pull(target), 
                                                      sset %>% filter(group == "EndYear") %>% pull(target), 
                                                      var.equal = F)
                                        
                                        # Saving results as data frame
                                        df <- tribble(~country, ~mean2022,       ~meanPrevYear,   ~tstat,           ~pvalue, 
                                                      cntry,    obj$estimate[1], obj$estimate[2], obj$statistic[1], obj$p.value)
                                        
                                      } else {
                                        
                                        # Saving empty rows as a data frame
                                        df <- tribble(~country,  ~mean2022, ~meanPrevYear, ~tstat,   ~pvalue, 
                                                      cntry,     NA_real_,  NA_real_,      NA_real_, NA_real_)
                                      }
                                      
                                      return(df)
                                      
                                    })
         
         # Saving results
         write.xlsx(as.data.frame(results_bycntry), 
                    file      = file.path("Outputs",
                                          "0.DataChecks",
                                          "Step_6.xlsx",
                                          fsep = "/"), 
                    sheetName = paste0(target),
                    append    = T,
                    row.names = F)
       })
                           
                           
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## STEP 7: Identify changes in trends                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     

# Identifying trend changes
trendChanges.df <- data_subset.df %>%
  filter(! country %in% c("Chile", "Paraguay", "Mexico", "Uruguay",  "Venezuela")) %>%
  group_by(country, country_code, year) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  mutate(across(!year,
                ~ (.x - dplyr::lag(.x))*100)) %>%
  group_by(country) %>%
  slice_max(year, n = 2) %>%
  pivot_longer(!c(country, country_code, year),
               names_to  = "category",
               values_to = "pchange") %>%
  group_by(country, category) %>%
  mutate(
    cualitative = case_when(
      pchange >= 10                ~ "Very Positive",
      pchange > 0 & pchange < 10   ~ "Positive",
      pchange == 0                 ~ "No change",
      pchange < 0 & pchange > -10  ~ "Negative",
      pchange < 0 & pchange <= -10 ~ "Very Negative"
    ),
    Pos = if_else(str_detect(cualitative, "Positive"),
                  TRUE,
                  FALSE),
    Pos = sum(Pos, na.rm = T),
    Neg = if_else(str_detect(cualitative, "Negative"),
                  TRUE,
                  FALSE),
    Neg = sum(Neg, na.rm = T),
    Ext = if_else(str_detect(cualitative, "Very"),
                  TRUE,
                  FALSE),
    Ext = sum(Ext, na.rm = T),
    status = case_when(
      Pos == 1 & Neg == 1 & Ext > 0  ~ "Significant Trend Change",
      Pos == 1 & Neg == 1 & Ext == 0 ~ "Small Trend Change",
      Pos == 2 | Neg == 2            ~ "No Trend Change",
      TRUE ~ "No comparison"
    )
  ) %>%
  arrange(country, category) %>%
  filter(year == 2022) %>%
  select(country, category, status) %>%
  pivot_wider(country,
              names_from  = category,
              values_from = status)

# Saving results
write.xlsx(as.data.frame(trendChanges.df), 
           file      = file.path("Outputs",
                                 "0.DataChecks",
                                 "Step_7.xlsx",
                                 fsep = "/"), 
           sheetName = "TrendChanges",
           row.names = F)

  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## STEP 8: Identifying Outliers                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Cleaning data
outliers.df <- data_subset.df %>%
  filter(year == 2022 | country == "Paraguay") %>%
  filter(! country %in% c("Nicaragua")) %>%
  select(-year) %>%
  group_by(country, country_code) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>% 
  pivot_longer(!c(country, country_code),
               names_to  = "category",
               values_to = "value2plot") %>%
  filter(!is.na(value2plot))

# Looping across variables
imap(list("Fundamental Freedoms" = ffreedoms, 
          "Accountability"       = accountability, 
          "Corruption"           = corruption, 
          "Trust"                = trust, 
          "Security"             = security, 
          "Criminal Justice"     = cjustice, 
          "Effectiveness"        = effectiveness, 
          # "Authoritarianism"     = authoritarian, 
          # "Corruption Tolerance" = ctolerance, 
          # "Crime Victimization"  = victimization
          "Police"               = police), 
       function(vset, vset_name){
         
         # Subsetting variables
         data2plot <- outliers.df %>% 
           filter(category %in% vset) %>%
           group_by(category) %>%
           mutate(outlier = if_else(value2plot < quantile(value2plot, .25) - 1.5*IQR(value2plot) | value2plot > quantile(value2plot, .75) + 1.5*IQR(value2plot),
                                    TRUE,
                                    FALSE))
         
         # Plotting
         ggplot(data  = data2plot,
                aes(x = category,
                    y = value2plot)) +
           geom_boxplot() +
           geom_text(data = data2plot %>%
                       filter(outlier == T), 
                     aes(x     = category, 
                         y     = value2plot, 
                         label = country_code),
                     nudge_y = 0.025) +
           labs(title = paste("Variable set:", vset_name)) +
           theme_bw() + 
           theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank())
         
         # Saving plots
         ggsave(paste0("Outputs/0.DataChecks/Step_8/", vset_name,".png"))
         
       }) 

         
         
         

