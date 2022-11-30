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
    scale_x_discrete(position = "top") +
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
          axis.text.x = element_text(family ="Lato Full", 
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
  
  logit_plot <- logit_demo_panel(mainData = data2plot, line_size = 1.5)
  
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

figure_17.fn <- function() {
  # Panel A: Serve the Public
  
  panelA <- data_subset.df %>%
    filter(year == 2022 & country == "Colombia") %>%
    select(q48c_G2, EXP_q22i_G2 , EXP_q22h_G2) %>%
    mutate(
      q48c_G2 = case_when(
        q48c_G2 == 1  ~ 1,
        q48c_G2 == 2  ~ 1,
        q48c_G2 == 3  ~ 0,
        q48c_G2 == 4  ~ 0,
        q48c_G2 == 99 ~ 0,
        is.na(q48c_G2) ~ NA_real_
      ),
      EXP_q22i_G2 = case_when(
        EXP_q22i_G2 == 1  ~ 1,
        EXP_q22i_G2 == 2  ~ 1,
        EXP_q22i_G2 == 3  ~ 0,
        EXP_q22i_G2 == 4  ~ 0,
        EXP_q22i_G2 == 99 ~ 0,
        is.na(EXP_q22i_G2) ~ NA_real_
      ),
      EXP_q22h_G2 = case_when(
        EXP_q22h_G2 == 1  ~ 1,
        EXP_q22h_G2 == 2  ~ 1,
        EXP_q22h_G2 == 3  ~ 0,
        EXP_q22h_G2 == 4  ~ 0,
        EXP_q22h_G2 == 99 ~ 0,
        is.na(EXP_q22h_G2) ~ NA_real_
      ),
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(
      empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q48c_G2", 1.15,
                  if_else(variable %in% "EXP_q22i_G2", 2.15,
                          if_else(variable %in% "EXP_q22h_G2", 3.15, NA_real_))),
      order_value = if_else(variable %in% "q48c_G2", 1,
                            if_else(variable %in% "EXP_q22i_G2", 2,
                                    if_else(variable %in% "EXP_q22h_G2", 3, NA_real_))),
      variable = case_when(
        variable == "q48c_G2" ~ "Are available to help when needed",
        variable == "EXP_q22i_G2" ~ "Serve the interests of the community",
        variable == "EXP_q22h_G2" ~ "Serve the interests of regular citizens"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
    );panelA
  
  a <- horizontal_edgebars(data2plot    = panelA,
                           y_value      = value,
                           x_var        = variable,
                           order_value  = order_value, 
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 10);a
  
  saveIT.fn(chart  = a,
            n      = 17,
            suffix = "a",
            w      = 82.59305,
            h      = 45.33831)
  
  # Panel B: Crime Control and Safety
  
  panelB <- data_subset.df %>%
    filter(year == 2022 & country == "Colombia") %>%
    select(EXP_q24a_G1, q48a_G2, q48b_G1, EXP_q24a_G2) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ 0,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "EXP_q24a_G1", 1.1,
                      if_else(variable %in% "q48a_G2", 2.15,
                              if_else(variable %in% "q48b_G1", 3.15, 4.15))),
      order_value = if_else(variable %in% "EXP_q24a_G1", 1,
                            if_else(variable %in% "q48a_G2", 2,
                                    if_else(variable %in% "q48b_G1", 3, 4))),
      variable = case_when(
        variable == "EXP_q24a_G1" ~ "Crime victims receive prompt and courteous <br> attention",
        variable == "q48a_G2"     ~ "Resolve security problems in  the community",
        variable == "q48b_G1"     ~ "Perform effective and lawful investigations",
        variable == "EXP_q24a_G2" ~ "Crime victims receive protection from the police",
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelB
  
  b <- horizontal_edgebars(data2plot    = panelB,
                           y_value      = value,
                           x_var        = variable,
                           order_value  = order_value,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 0);b
  
  saveIT.fn(chart  = b,
            n      = 17,
            suffix = "b",
            w      = 82.59305,
            h      = 59.74817)
  
  # Panel C: Due Process
  panelC <- data_subset.df %>%
    filter(year == 2022 & country == "Colombia") %>%
    select(q48a_G1, EXP_q22e_G1, q48c_G1, q48d_G2) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ 0,
               is.na(.x) ~ NA_real_
             )),
      EXP_q22e_G1 = if_else(EXP_q22e_G1 == 1, 0, 
                            if_else(EXP_q22e_G1 == 0, 1, NA_real_))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q48a_G1", 1.15,
                           if_else(variable %in% "EXP_q22e_G1", 2.15,
                                   if_else(variable %in% "q48c_G1", 3.15, 4.15))),
      order_value = if_else(variable %in% "q48a_G1", 1,
                            if_else(variable %in% "EXP_q22e_G1", 2,
                                    if_else(variable %in% "q48c_G1", 3, 4))),
      variable = case_when(
        variable == "q48a_G1"     ~ "Act lawfully",
        variable == "EXP_q22e_G1" ~ "Do not use excessive force",
        variable == "q48c_G1"     ~ "Respect the rights of suspects",
        variable == "q48d_G2"     ~ "Treat all people with respect",
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelC
  
  c <- horizontal_edgebars(data2plot    = panelC,
                           y_value      = value,
                           x_var        = variable,
                           order_value  = order_value,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 10);c
  
  saveIT.fn(chart  = c,
            n      = 17,
            suffix = "c",
            w      = 82.59305,
            h      = 59.74817)
  
  # Panel D: Discrimination
  
  panelD <- data_subset.df %>%
    filter(year == 2022 & country == "Colombia") %>%
    select(q18b, EXP_q17g, EXP_q17h, EXP_q17i, EXP_q17j) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 0  ~ 0,
               .x == 1  ~ 1,
               .x == 99 ~ NA_real_,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q18b", 1.15,
                      if_else(variable %in% "EXP_q17g", 2.15,
                              if_else(variable %in% "EXP_q17h", 3.15, 
                                      if_else(variable %in% "EXP_q17i", 4.15, 5.15)))),
      order_value = if_else(variable %in% "q18b", 1,
                            if_else(variable %in% "EXP_q17g", 2,
                                    if_else(variable %in% "EXP_q17h", 3, 
                                            if_else(variable %in% "EXP_q17i", 4, 5)))),
      variable = case_when(
        variable == "q18b"        ~ "Gender",
        variable == "EXP_q17g"    ~ "Skin color",
        variable == "EXP_q17h"    ~ "Indigenous identity",
        variable == "EXP_q17i"    ~ "Tattoos",
        variable == "EXP_q17j"    ~ "Age"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelD
  
  d <- horizontal_edgebars(data2plot    = panelD,
                           y_value      = value,
                           x_var        = variable,
                           order_value  = order_value,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 10);d
  
  saveIT.fn(chart  = d,
            n      = 17,
            suffix = "d",
            w      = 82.59305,
            h      = 74.86094)
  
  # Panel E: Discrimination
  
  panelE <- data_subset.df %>%
    filter(year == 2022 & country == "Colombia") %>%
    select(q2d, q48e_G2, EXP_q22k_G2, EXP_q22j_G2) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ 0,
               is.na(.x) ~ NA_real_
             )),
      EXP_q22k_G2 = if_else(EXP_q22k_G2 == 1, 0, 
                            if_else(EXP_q22k_G2 == 0, 1, NA_real_)),
      EXP_q22j_G2 = if_else(EXP_q22j_G2 == 1, 0, 
                            if_else(EXP_q22j_G2 == 0, 1, NA_real_))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q2d", 1.15,
                      if_else(variable %in% "q48e_G2", 2.15,
                              if_else(variable %in% "EXP_q22k_G2", 3.15, 
                                      if_else(variable %in% "EXP_q22j_G2", 4.15, NA_real_)))),
      order_value = if_else(variable %in% "q2d", 1,
                            if_else(variable %in% "q48e_G2", 2,
                                    if_else(variable %in% "EXP_q22k_G2", 3, 4))),
      variable = case_when(
        variable == "q2d"         ~ "Are not involved in corrupt practices",
        variable == "q48e_G2"     ~ "Investigate crimes in an independent manner",
        variable == "EXP_q22k_G2" ~ "Do not serve the interests of gangs",
        variable == "EXP_q22j_G2" ~ "Do not serve the interests of politicians"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelE
  
  e <- horizontal_edgebars(data2plot    = panelE,
                           y_value      = value,
                           x_var        = variable,
                           order_value  = order_value, 
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 10);e
  
  saveIT.fn(chart  = e,
            n      = 17,
            suffix = "e",
            w      = 82.59305,
            h      = 59.74817)
  
  # Panel F: Trust and Safety
  
  panelF <- data_subset.df %>%
    filter(year == 2022 & country == "Colombia") %>%
    select(q1d, q9, q48b_G2) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ 0,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q1d", 1.15,
                      if_else(variable %in% "q9", 2.15,
                              if_else(variable %in% "q48b_G2", 3.15, NA_real_))),
      order_value = if_else(variable %in% "q1d", 1,
                            if_else(variable %in% "q9", 2, 3)),
      variable = case_when(
        variable == "q1d"     ~ "Trust the police",
        variable == "q9"      ~ "Feel safe in their neighborhoods",
        variable == "q48b_G2" ~ "Feel the police contributes to their safety"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelF
  
  f <- horizontal_edgebars(data2plot    = panelF,
                           y_value      = value,
                           x_var        = variable,
                           order_value  = order_value,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 10);f
  
  saveIT.fn(chart  = f,
            n      = 17,
            suffix = "f",
            w      = 82.59305,
            h      = 45.33831)
  
  # Panel G: Accountability
  
  panelG <- data_subset.df %>%
    filter(year == 2022 & country == "Colombia") %>%
    select(q48d_G1, EXP_q22f_G1, EXP_q22g_G1, EXP_q22h_G1) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ 0,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q48d_G1", 1.15,
                      if_else(variable %in% "EXP_q22f_G1", 2.15,
                              if_else(variable %in% "EXP_q22g_G1", 3.15, 
                                      if_else(variable %in% "EXP_q22h_G1", 4.15, NA_real_)))),
      order_value = if_else(variable %in% "q48d_G1", 1,
                            if_else(variable %in% "EXP_q22f_G1", 2, 
                                    if_else(variable %in% "EXP_q22g_G1", 3, 4))),
      variable = case_when(
        variable == "q48d_G1"     ~ "Are held accountable for violating laws",
        variable == "EXP_q22f_G1"     ~ "Are held accountable for seeking bribes",
        variable == "EXP_q22g_G1"     ~ "Are held accountable for accepting bribes",
        variable == "EXP_q22h_G1" ~ "Are investigated for misconduct"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelG
  
  g <- horizontal_edgebars(data2plot    = panelG,
                           y_value      = value,
                           x_var        = variable,
                           order_value  = order_value,
                           group_var    = group,
                           label_var    = label,
                           x_lab_pos    = x_pos,
                           y_lab_pos    = 0,
                           bar_color    = "#2a2a94",
                           margin_top   = 10);g
  
  saveIT.fn(chart  = g,
            n      = 17,
            suffix = "g",
            w      = 82.59305,
            h      = 59.74817)
}

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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 19                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure19.fn <- function() {

legal_assigment <- function(data_subset) {
  
  target_variables <- c(
    # Advice
    "q25_1", "q25_2", "q25_3", "q25_4", "q25_5", "q25_6", "q25_7", "q25_8", "q25_9",
    # Representation
    "q24", 
    # Claim to court (A2J)
    "q28", 
    # Persistance of the problem
    "q30", 
    # Fair Process
    "q36a", 
    # Slow Process
    "q36b", 
    # Expensive Process
    "q36c", 
    # Time of the process
    "q37b", 
    # Economic Issues
    "q37d",
    # Satisfaction with the outcome
    "q38",
    # Sources of help
    "q41a", "q41b", "q41c",
    # Confidence in the system
    "q41d",
    # Hardships: Stress-related illness, injuries, or physical ill healt
    "q42a",
    # Hardships: Relationship breakdown or damage to a family relationship
    "q42b",
    # Hardships: Loss of income, loss of employment
    "q42c",
    # Hardships: Problem with alcohol or drugs
    "q42d",
    # Sociodemographics
    "area", "income_bin", "skin_color","skin_color_bin", "gender", "income_aux")
  
  a <- data_subset %>%
    mutate(a2j_consumer = ifelse(q19_A1 == 1 | q19_A2 == 1 | q19_A3 == 1, 1, 0),
           a2j_land = ifelse(q19_B1 == 1 | q19_B2 == 1 | q19_B3 == 1, 1, 0),
           a2j_housing = ifelse(q19_C1 == 1 | q19_C2 == 1 | q19_C3 == 1, 1, 0),
           a2j_family = ifelse(q19_D1 == 1 | q19_D2 == 1 | q19_D3 == 1 | q19_D4 == 1 | q19_D5 == 1 |q19_D6 == 1, 1, 0),
           a2j_education = ifelse(q19_E1 == 1 | q19_E2 == 1 | q19_E3 == 1, 1, 0),
           a2j_accidental = ifelse(q19_F1 == 1 | q19_F2 == 1 , 1, 0),
           a2j_employment = ifelse(q19_G1 == 1, q19_G2 == 1, q19_G3 == 1),
           a2j_public = ifelse(q19_H1 == 1 | q19_H2 == 1 | q19_H3 == 1, 1, 0),
           a2j_law = ifelse(q19_I1 == 1, 1, 0),
           a2j_id = ifelse(q19_J1 == 1 | q19_J2 == 1 | q19_J3 == 1 | q19_J4 == 1, 1, 0),
           a2j_money =  ifelse(q19_K1 == 1 | q19_K2 == 1 | q19_K3 == 1, 1, 0),
           a2j_community = ifelse(q19_L1 == 1 | q19_L2 == 1, 1, 0)) %>%
    mutate(legal = ifelse(a2j_consumer == 1 | a2j_land == 1 | a2j_housing == 1 | a2j_family == 1 | a2j_education == 1 | a2j_accidental == 1 |
                            a2j_employment == 1 | a2j_public == 1 | a2j_law == 1 | a2j_id == 1 | a2j_money == 1 | a2j_community == 1, 1, 0)) %>%
    group_by(country) %>%
    filter(year == 2022) %>%
    ungroup() %>%
    select(country, year, ends_with(target_variables), starts_with("a2j_"), legal) %>%
    filter(country %in% mainCountry)
}

aes_function <- function(mainData) {
  
  data2plot <- mainData %>%
  mutate(empty_value = 1 - value) %>%
  pivot_longer(!problem,
               names_to = "group",
               values_to = "value") %>%
  mutate(x_pos = c(4.1, 4.1, 3.1, 3.1, 2.1, 2.1, 1.1, 1.1),
         order_value = c(1,1,2,2,3,3,4,4),
         empty_value = 1 - value,
         label = paste(round(value*100,0), "%"),
         multiplier = if_else(group == "empty_value", 0, 1),
         label = if_else(multiplier == 0, NA_character_, label))
  return(data2plot)
}

# Figure 19a

a2j <- legal_assigment(data_subset = data_subset.df)

a2j_a <- a2j %>%
  select(starts_with("a2j_")) %>%  
  rename("Accidental Illness & <br> Injury" = "a2j_accidental",
    "Citizenship & ID" = "a2j_id",
    "Community & Natural <br> Resources" = "a2j_community",
    "Consumer" = "a2j_consumer",
    "Employment" = "a2j_employment",
    "Education"  = "a2j_education",
    "Family" =  "a2j_family",
    "Housing" = "a2j_housing",
    "Land" = "a2j_land",
    "Law Enforcement" = "a2j_law",
    "Money & Debt" = "a2j_money",
    "Public Services" = "a2j_public"
    ) %>%
  summarise(across(everything(),mean, na.rm = T)) %>%
  pivot_longer(cols = everything(),names_to = "problem", values_to = "value") %>%
  arrange(-value)

first_panel <- a2j_a %>%
  top_n(n = 4) %>%
  aes_function(.)

a2j_p1 <- horizontal_edgebars(data2plot    = first_panel,
                         y_value      = value,
                         x_var        = problem,
                         order_value  = order_value,
                         group_var    = group,
                         label_var    = label,
                         x_lab_pos    = x_pos,
                         y_lab_pos    = 0,
                         bar_color    = "#2a2a94")

second_panel <- a2j_a[5:8,] %>%
  aes_function(.)

a2j_p2 <- horizontal_edgebars(data2plot    = second_panel,
                              y_value      = value,
                              x_var        = problem,
                              order_value  = order_value,
                              group_var    = group,
                              label_var    = label,
                              x_lab_pos    = x_pos,
                              y_lab_pos    = 0,
                              bar_color    = "#2a2a94")

third_panel <- a2j_a[9:12,] %>%
  aes_function(.)

a2j_p3 <- horizontal_edgebars(data2plot    = third_panel,
                              y_value      = value,
                              x_var        = problem,
                              order_value  = order_value,
                              group_var    = group,
                              label_var    = label,
                              x_lab_pos    = x_pos,
                              y_lab_pos    = 0,
                              bar_color    = "#2a2a94")

figures_problems<- list()
figures_problems[["Panel A"]] <- a2j_p1
figures_problems[["Panel B"]] <- a2j_p2
figures_problems[["Panel C"]] <- a2j_p3

figure19a <- figures_problems[["Panel A"]] + figures_problems[["Panel B"]] + figures_problems[["Panel C"]] +
  plot_layout(ncol = 3,
              nrow = 1,
              widths = unit(42, "mm"),
              heights = unit(52, "mm"))
  
# Saving Patchwork
saveIT.fn(chart  = figure19a,
          n      = 19,
          suffix = "a",
          w      = 131.7974,
          h      = 53.42189)

# Figure 19B

aes_function_v2 <- function(mainData) {
  
  data2plot <- mainData %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!help,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(x_pos = c(3.1, 3.1, 2.1, 2.1, 1.1, 1.1),
           order_value = c(3,3,2,2,1,1),
           empty_value = 1 - value,
           label = paste(round(value*100,0), "%"),
           multiplier = if_else(group == "empty_value", 0, 1),
           label = if_else(multiplier == 0, NA_character_, label))
  return(data2plot)
}

a2j_b <- a2j %>%
  filter(legal == 1) %>%
  summarise("Friend or Family" = mean(q25_1, na.rm = T),
            "Lawyer or Professional <br> Advice Service" = mean(q25_2, na.rm = T),
            "Government Legal Aid <br> Office" = mean(q25_3, na.rm = T),
            "Court or Government <br> Body or Police" = mean(q25_4, na.rm = T),
            "Health or Welfare <br> Professional" = mean(q25_5, na.rm = T),
            "Trade Union or Employer" = mean(q25_6, na.rm = T),
            "Religious or Community <br> Leader" = mean(q25_7, na.rm = T),
            "Civil Society <br> Organization or Charity" = mean(q25_8, na.rm = T),
            "Other Organization" = mean(q25_9, na.rm = T)) %>%
  pivot_longer(cols = everything(),names_to = "help", values_to = "value") %>%
  arrange(-value)
  
first_panel <- a2j_b[1:3,] %>%
  aes_function_v2(.)

a2j_p1 <- horizontal_edgebars(data2plot    = first_panel,
                              y_value      = value,
                              x_var        = help,
                              order_value  = order_value,
                              group_var    = group,
                              label_var    = label,
                              x_lab_pos    = x_pos,
                              y_lab_pos    = 0,
                              bar_color    = "#2a2a94")

second_panel <- a2j_b[4:6,] %>%
  aes_function_v2(.)

a2j_p2 <- horizontal_edgebars(data2plot    = second_panel,
                              y_value      = value,
                              x_var        = help,
                              order_value  = order_value,
                              group_var    = group,
                              label_var    = label,
                              x_lab_pos    = x_pos,
                              y_lab_pos    = 0,
                              bar_color    = "#2a2a94")

third_panel <- a2j_b[7:9,] %>%
  aes_function_v2(.)

a2j_p3 <- horizontal_edgebars(data2plot    = third_panel,
                              y_value      = value,
                              x_var        = help,
                              order_value  = order_value,
                              group_var    = group,
                              label_var    = label,
                              x_lab_pos    = x_pos,
                              y_lab_pos    = 0,
                              bar_color    = "#2a2a94")

figures_problems<- list()
figures_problems[["Panel A"]] <- a2j_p1
figures_problems[["Panel B"]] <- a2j_p2
figures_problems[["Panel C"]] <- a2j_p3

figure19b <- figures_problems[["Panel A"]] + figures_problems[["Panel B"]] + figures_problems[["Panel C"]] +
  plot_layout(ncol = 3,
              nrow = 1,
              widths = unit(42, "mm"),
              heights = unit(45, "mm"))

saveIT.fn(chart  = figure19b,
          n      = 19,
          suffix = "b",
          w      = 131.7974,
          h      = 53.42189)
}
