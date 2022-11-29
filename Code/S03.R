## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Section III Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 22nd, 2022
##
## This version:      November 23rd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 13                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  security.universe <- function(master_data) {
    
    security.universe <- master_data %>%
      filter(country %in% mainCountry) %>%
      filter(year == 2022) %>%
      select(# All variables related with security
        starts_with("EXP_q8"), 
        # Security perception
        q9, 
        # Sociodemographics 
        COLOR, income_aux, gend, disability2, disability, Urban, age, edu,
        # Variables related to institutions perfomance
        q48b_G1, q48f_G1, q49a, CAR_q58_G1, q48f_G2, q48g_G2, 
        # Trust in institutions
        q1c, q1d, q1e, q1g, q1i, q41d) %>%
      # This variable assigns the victim condition to each observation
      mutate(victim = if_else(EXP_q8a_1 == 1 | EXP_q8a_2 == 1 | EXP_q8a_3 == 1 | EXP_q8a_4 == 1 | EXP_q8a_5 == 1 | EXP_q8a_6 == 1 | EXP_q8a_7 == 1 |
                                EXP_q8a_8 == 1 | EXP_q8a_9 == 1 | EXP_q8a_10 == 1 | EXP_q8a_11 == 1 | EXP_q8a_12 == 1 | EXP_q8a_13 == 1 | EXP_q8a_14 == 1 |
                                EXP_q8b_1 == 1 | EXP_q8b_2 == 1 | EXP_q8b_3 == 1 | EXP_q8b_4 == 1 | EXP_q8b_5 == 1, 1, 0, 0))
    
    return(security.universe)
  } # This is going to be refactoring, and this function guarantee the automatization 

figure13_1.fn <- function() {
    
  security_universe <- security.universe(master_data = data_subset.df) # This function assign the victim condition and select the main variables to security secction
  
  # Panel A
  
  data2plot <- security_universe %>%
    mutate(prop_crimes = if_else(EXP_q8a_1 == 1 |EXP_q8a_2 == 1 |EXP_q8a_3 == 1| EXP_q8a_4 == 1 
                                 |EXP_q8a_5 == 1| EXP_q8a_6 == 1 |EXP_q8a_8 == 1, 1, 0),
           life_crimes = if_else(EXP_q8a_7 == 1 |EXP_q8a_12 == 1 |EXP_q8b_1 == 1 |EXP_q8b_2 == 1
                                 |EXP_q8b_3 == 1, 1, 0),
           corr_crimes = if_else(EXP_q8a_9 == 1| EXP_q8a_10 == 1|EXP_q8a_11 == 1, 1, 0)) %>%
    summarise(prop_crimes = round(mean(prop_crimes, na.rm=T), 2),
              life_crimes = round(mean(life_crimes, na.rm=T), 2),
              corr_crimes = round(mean(corr_crimes, na.rm=T), 2))  %>%
    pivot_longer(cols=c(prop_crimes,life_crimes,corr_crimes), names_to = "category", values_to = "value2plot") %>%
    mutate(category = case_when(category == "prop_crimes" ~ "Property crimes",
                                category == "life_crimes" ~ "Crimes against life and integrity \nof individuals",
                                category == "corr_crimes" ~ "Corruption, financial, \nand commercial crimes"))
  
  crimes <- lollipop_chart(data2plot = data2plot, 
                           categories = category)
  
  saveIT.fn(chart  = crimes,
            n      = 13,
            suffix = "A",
            w      = 175.027,
            h      = 46.74415)
}

# Panel B

figure13_2.fn <- function() {
  
  security_universe <- security.universe(master_data = data_subset.df) # This function assign the victim condition and select the main variables to security secction
  
  victims <- security_universe %>%
    summarise(victim = round(mean(victim, na.rm = T),2)) %>%
    mutate(non_victim = 1 - victim)
  
  report <- security_universe %>%
    filter(EXP_q8d != 99) %>%
    mutate(EXP_q8d = if_else(EXP_q8d == 1, 1, 0)) %>%
    filter(victim == 1) %>%
    summarise(report = round(mean(EXP_q8d, na.rm = T),2)) %>%
    mutate(non_report = 1 - report)
  
  fill_report <- security_universe %>%
    filter(victim == 1) %>%
    mutate(EXP_q8d = if_else(EXP_q8d == 1, 1,
                             if_else(EXP_q8d == 1, 0, NA_real_)),
           EXP_q8f = if_else(EXP_q8f == 1, 1, 
                             if_else(EXP_q8f == 0, 0, NA_real_))) %>%
    group_by(EXP_q8d) %>%
    summarise(fill_report = round(mean(EXP_q8f, na.rm = T),2)) %>%
    mutate(non_fill_report = 1 - fill_report) %>%
    filter(EXP_q8d == 1) %>%
    select(!EXP_q8d)
  
  t1 <- sample(x = c("Victim", "Non-Victim"), size = 1000, replace = TRUE, prob = c(1,0))
  t2 <- sample(x = c("Non-Report", "Report"), size = 1000, replace = TRUE, prob = c(report$non_report, report$report))
  t3 <- sample(x = c("Non-Official", "Official"), size = 1000, replace = TRUE, prob = c(fill_report$non_fill_report,fill_report$fill_report))
  
  d <- data.frame(cbind(t1, t2))
  names(d) <- c("Victim", "Report")
  
  
  df <- d %>%
    mutate(`Official Crime Report` = if_else(Report %in% "Report", t3, " ")) 
  
  data2plot <- df %>%
    make_long(Victim, Report, `Official Crime Report`)
  
  y <- c(1, 900, -300, 600, 75)
  x <- c(0.7, 2, 2.3, 3.3, 3.3)
  label <- c(paste0("<span style='color:#003b8a;font-size:4.217518mm'>", '**',victims$victim*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> of Colombians <br>were victims <br>of a crime"),
             paste0("<span style='color:#003b8a;font-size:4.217518mm'>", '**',report$report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> reported <br> the crime"),
             paste0("<span style='color:#fa4d57;font-size:4.217518mm'>", '**',report$non_report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> did not report <br>the crime"),
             paste0("<span style='color:#003b8a;font-size:4.217518mm'>", '**',fill_report$fill_report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> filed an official <br> crime report"),
             paste0("<span style='color:#fa4d57;font-size:4.217518mm'> ", '**',fill_report$non_fill_report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> did not file an <br>official crime report"))
  df <- data.frame(label)
  
  pl <- ggplot(data = data2plot, aes(x = x, 
                                     next_x = next_x,
                                     node = node,
                                     next_node = next_node,
                                     fill = factor(node))) +
    geom_sankey(flow.alpha = 0.5,
                node.color = "white",
                show.legend = FALSE) +
    geom_richtext(data = df, aes(x = x, label = label, y = y, 
                                 next_x = NULL, node = NULL, 
                                 next_node = NULL, fill = NULL, family = "Lato Medium"), 
                  fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5) +
    scale_y_continuous(expand = expansion(mult = c(0,0.2))) +
    scale_fill_manual(values = c("Victim" = "#003b8a",
                                 'Non-Victim' = "#003b8a",
                                 "Report" = "#003b8a",
                                 "Non-Report" = "white",
                                 "Official" = "#003b8a",
                                 "Non-Official" = "#fa4d57",
                                 ' ' = "white")) +
    theme_sankey(base_size = 10, base_rect_size = 10) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(family ="Lato Regular", 
                                     size = 3.514598*.pt,
                                     color = "Black"),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank());pl
  
  saveIT.fn(chart  = pl,
            n      = 13,
            suffix = "B",
            w      = 175.027,
            h      = 94.54267)

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 14                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel
figure14_1.fn <- function() {
  
  nchart = 14
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry) %>%
    select(year, q9) %>%
    mutate(
      q9   = case_when(
        q9 == 1 | q9 == 2    ~ 1,
        q9 == 3 | q9 == 4    ~ 0,
        q9 == 99 | is.na(q9) ~ NA_real_,
      ),
      year = paste0("'", str_sub(year, start = -2))
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    mutate(value2plot = q9*100,
           label      = to_percentage.fn(value2plot),
           category   = mainCountry)
  
    # Defining colors4plot
    colors4plot <- mainCOLOR
    names(colors4plot) <- mainCountry
    
    
    # Applying plotting function
    chart <- LAC_lineChart(data           = data2plot,
                           target_var     = "value2plot",
                           grouping_var   = "year",
                           ngroups        = 1, 
                           labels_var     = "label",
                           colors_var     = "category",
                           colors         = colors4plot,
                           repel          = F
    )
    
    # Saving panels
    saveIT.fn(chart  = chart,
              n      = nchart,
              suffix = "A",
              w      = 189.7883,
              h      = 68.88612)
} 

# Lower Panel
figure14_2.fn <- function() {
  
  security_universe <- security.universe(master_data = data_subset.df) # This function assign the victim condition and select the main variables to security secction
  
  perception <- security_universe %>%
    mutate(unsafe_bin    =  if_else(q9 == 1 | q9 == 2, 1, 
                                    if_else(q9 == 3 | q9 ==4, 0, NA_real_)),
           victim        =  if_else(victim == 1, "Victim", "Non Victim"),
           white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, "White" , 
                                    if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, "No White", NA_character_)),
           young         =  if_else(age < 31, "Less than 30 years", 
                                    if_else(age > 30, "More than 30 years", NA_character_)),
           poor          =  if_else(income_aux == 1 | income_aux == 2, "Poor",
                                    if_else(income_aux == 3 | income_aux == 4 | income_aux == 5, "No Poor", NA_character_)),
           area          =  if_else(Urban == 1, "Urban", "Rural"),
           gender        =  if_else(gend == 1, "Male", "Female"),
           diploma       =  if_else(edu == 5 | edu == 6, "High Education Level", 
                                    if_else(edu < 5, "No High Education Level", NA_character_))) # We transform the variable of security perception in a dummy variable, the values 3 and 4 reference to unsafe people feeling
  
  logit_demo <- function(mainData, Yvar) {
  
  logit_data <- perception
  
  logit_data$young <- recode(logit_data$young, "More than 30 years" = "1More than 30 years")
  logit_data$gender <- recode(logit_data$gender, Male = "1male")
  
  logit_data<- logit_data %>%
    select(gender, victim, white, poor, young, area, diploma,
           unsafe_bin) # We didn't include the non answer
  
  models <- lapply(list("unsafe_bin"), 
                   function(depVar) {
                     formula  <- as.formula(paste(depVar, "~ gender + victim + white + poor + young + area + diploma"))
                     logit  <- glm(formula,  
                                   data   = logit_data, 
                                   family = "binomial")})
  margEff    <- margins_summary(models[[1]], data = models[[1]]$model)
  
  data2plot <- margEff
  
  data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Poor", "victimVictim" = "Victim",
                             "areaUrban" = "Urban", "whiteWhite" = "White", "youngLess than 30 years" = "Less than \n30 years",
                             "diplomaNo High Education Level" = "Low Education \nLevel")
  
  data2plot <- data2plot %>%
    mutate(category = "Colombia",
           order_variable = if_else(factor %in% "Female", 1,
                                    if_else(factor %in% "White", 2,
                                            if_else(factor %in% "Poor", 3,
                                                    if_else(factor %in% "Victim", 4,
                                                            if_else(factor %in% "Urban", 5, 
                                                                    if_else(factor %in% "Young", 6, 7)))))))
  }
  
  data2plot <- logit_demo(mainData = perception, Yvar = 'unsafe_bin')
  
  logit_plot <- logit_demo_panel(mainData = data2plot)
  
  saveIT.fn(chart  = logit_plot,
            n      = 14,
            suffix = "B",
            w      = 175.027,
            h      = 81.89012)
}
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 15                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure15.fn <- function() {
  
  nchart = 15
  
  # Defining which years to show in the plot: Two latest years for each country
  yrs <- data_subset.df %>%
    filter(country == mainCountry) %>%
    group_by(year) %>%
    summarise() %>%
    slice_max(order_by = year,
              n = 2) %>%
    pull(year)
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry & year %in% yrs) %>%
    select(year, q49a, q49b_G2, q49e_G2, q49c_G2, q49e_G1, q49d_G1, EXP_q23d_G1, q49c_G1, q49b_G1) %>%
    mutate(
      
      # We need to concatenate variables q49d_G1 and EXP_q23d_G1 into a single one
      q49d_G1_merge = rowSums(across(c(q49d_G1, EXP_q23d_G1)), 
                              na.rm = T),
      q49d_G1_merge = if_else(is.na(q49d_G1) & is.na(EXP_q23d_G1), NA_real_, q49d_G1_merge),
      
      # Transforming everything into binary variables
      across(!year,
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, NA_real_)))
    ) %>%
    select(!c(q49d_G1, EXP_q23d_G1)) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    rename(group = year)
  
  # Defining color palette
  colors4plot <- binPalette
  names(colors4plot) <- yrs
  
  # Definig labels - Part I: Percentages
  vals <- data2plot %>% 
    filter(group == yrs[1]) %>%
    pivot_longer(!group,
                 values_to = "values",
                 names_to  = "vars") %>%
    pull(values)
  vals <- to_percentage.fn(vals*100)
  names(vals) <- names(data2plot %>% select(!group))
  
  
  # Defining labels - Part II: Percentages + text as HTML
  applying_labels.fn <- function(text = text, color_code, value_vectors){
    case_when(
      text == 'q49a' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                             value_vectors["q49a"],
                             "</span>",
                             "<br>",
                             "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
                             "Is **effective** in bringing<br>people who commit<br>crimes to justice",
                             "</span>"),
      text == 'q49b_G2' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                                value_vectors["q49b_G2"],
                                "</span>",
                                "<br>",
                                "<span style='color:#524F4C;font-size:3.514598mm'>",
                                "Ensures **equal treatment<br>of victims** by allowing all<br>",
                                "victims to seek justice<br>regardless of who they are",
                                "</span>"),
      text == 'q49e_G2' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                                value_vectors["q49e_G2"],
                                "</span>",
                                "<br>",
                                "<span style='color:#524F4C;font-size:3.514598mm'>",
                                "Safeguards the<br>**presumption of<br>innocence** by treating<br>those",
                                "accused of<br>crimes as innocent<br>until proven guilty",
                                "</span>"),
      text == 'q49c_G2' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                                value_vectors["q49c_G2"],
                                "</span>",
                                "<br>",
                                "<span style='color:#524F4C;font-size:3.514598mm'>",
                                "Ensures **equal treatment of<br>the accused** by giving all a<br>",
                                "fair trial regardless of who<br>they are",
                                "</span>"),
      text == 'q49e_G1' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                                value_vectors["q49e_G1"],
                                "</span>",
                                "<br>",
                                "<span style='color:#524F4C;font-size:3.514598mm'>",
                                "Gives **appropriate<br>punishments** that fit<br>the crime",
                                "</span>"),
      text == 'q49d_G1_merge' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                                      value_vectors["q49d_G1_merge"],
                                      "</span>",
                                      "<br>",
                                      "<span style='color:#524F4C;font-size:3.514598mm'>",
                                      "Ensures **uniform quality** by<br>providing equal service<br>",
                                      "regardless of where<br>they live",
                                      "</span>"),
      text == 'q49c_G1' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                                value_vectors["q49c_G1"],
                                "</span>",
                                "<br>",
                                "<span style='color:#524F4C;font-size:3.514598mm'>",
                                "Ensures everyone<br>has **access** to the<br>justice system",
                                "</span>"),
      text == 'q49b_G1' ~ paste("<span style='color:", color_code, ";font-size:6.326276mm;font-weight:bold'>",  
                                value_vectors["q49b_G1"],
                                "</span>",
                                "<br>",
                                "<span style='color:#524F4C;font-size:3.514598mm'>",
                                "Ensures **timeliness**<br>by dealing with<br>cases promptly",
                                "and<br>efficiently",
                                "</span>")
    )
  }
  
  chart <- LAC_radarChart(data          = data2plot,
                          labelling_fn  = applying_labels.fn,           
                          colors        = colors4plot,
                          percentages   = vals)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 189.7883,
            h      = 183.1106)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 16                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure16.fn <- function() {
  
  nchart = 16
  
  # Variables to plot
  vars4plot = list("Trust"         = c("q1e", "q1f", "q1g"), 
                   "Corruption"    = c("q2e", "q2f", "q2g"), 
                   "Effectiveness" = c("q48f_G2", "q48h_G1", "q48g_G2"))
  
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
  
  # Plotting each panel of Figure 16
  imap(c("A" = "Trust", 
         "B" = "Corruption", 
         "C" = "Effectiveness"),
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
                   w      = 189.7883,
                   h      = 49.90729)
         
       })
} 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 17                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 18                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure18.fn <- function() {
  
  nchart = 18
  
  # Defining variables to use in rose chart
  vars4plot <- c("EXP_q24a_G1", "EXP_q24b_G1", "EXP_q24c_G1", "EXP_q24d_G1", "EXP_q24a_G2", "EXP_q24b_G2",
                 "EXP_q24c_G2", "EXP_q24d_G2", "EXP_q24f_G2", "EXP_q24g_G2", "EXP_q23f_G1")
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(all_of(vars4plot)) %>%
    mutate(
      across(everything(), 
             ~if_else(.x == 1 | .x == 2, 1, 
                      if_else(!is.na(.x) & .x != 99, 0,
                              NA_real_)))
    ) %>%
    summarise(across(everything(),
                     mean, 
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "avg") %>%
    arrange(desc(avg)) %>%
    mutate(
      percentage = to_percentage.fn(avg*100),
      label = case_when(
        category == "EXP_q24a_G1" ~ "Receive prompt and\ncourteous attention\nwhen they report a\ncrime",
        category == "EXP_q24b_G1" ~ "Are believed when\nthey report a crime",
        category == "EXP_q24c_G1" ~ "Receive effective and\ntimely medical and\npsychological care",
        category == "EXP_q24d_G1" ~ "Receive information\nand legal advice\nwhen going to the\nauthorities",
        category == "EXP_q24a_G2" ~ "Receive protection\nfrom the police if\ntheir safety is in\ndanger",
        category == "EXP_q24b_G2" ~ paste0("Receive protection\nduring criminal\nproceedings", 
                                           " to\nprevent repeat\nvictimization"),
        category == "EXP_q24c_G2" ~ "Receive adequate\ncare and protection\nas victims of sexual\ncrimes",
        category == "EXP_q24d_G2" ~ "Receive adequate\ncare and protection\nas victims of\ndomestic violence",
        category == "EXP_q24f_G2" ~ paste0("Receive a clear\nexplanation of\nthe process when\nreporting",
                                           " a crime to\nthe police"),
        category == "EXP_q24g_G2" ~ "Are addressed by\nthe police using\naccessible language",
        category == "EXP_q23f_G1" ~ "Are guaranteed\ntheir rights in\ncriminal justice\nproceedings"
      ),
      
      # Converting labels into HTML syntax
      across(label,
             function(raw_label){
               html <- paste0("<span style='color:#000000;font-size:6.326276mm;font-weight:bold'>",  
                              percentage, "</span>",
                              "<br>",
                              "<span style='color:#524F4C;font-size:3.514598mm'>",
                              str_replace_all(raw_label, "\\n", "<br>"),
                              "</span>")
               return(html)
             })
    )
  
  # Defining colors
  colors4plot        <- rosePalette
  names(colors4plot) <- data2plot %>% arrange(avg) %>% pull(category)
  
  # Applying plotting function
  chart <- LAC_roseChart(data = data2plot,
                         target_var    = "avg",
                         grouping_var  = "category",
                         alabels_var   = "label",
                         plabels_var   = "percentage",
                         colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 189.7883,
            h      = 168.7007)
  
  
}

