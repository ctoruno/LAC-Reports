## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Section VI Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     April 24th, 2023
##
## This version:      April 25th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 16 - United States: Discrimination                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel

figure16A_US.fn <- function(nchart = 16){

  data2plot <- data_subset.df %>%
    filter(country %in% mainCountry) %>%
    filter(year == 2021) %>%
    select(q16a, q16b, q16c, q16d, q16e) %>%
    mutate(across(everything(),
                  ~if_else(.x < 4, 1, 
                           if_else(.x == 4 | .x == 5 | .x == 6, 0, NA_real_)))) %>%
    rowwise() %>%
    mutate(discrimination = sum(q16a, q16b, q16c, q16d, q16e, na.rm = T),
           discrimination = if_else(discrimination > 0, 1, 0, NA_real_)) %>%
    ungroup() %>%
    summarise(discrimination = mean(discrimination, na.rm = T))  %>%
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
      group == "Yes" ~ "40% Yes",
      group == "No"  ~ "60% No"
    ))
  
  parts=c("Yes"= (data2plot$value[[1]]), "No"= (data2plot$value[[2]]))
  
  names(parts) = paste0(parts,"%", " ",names(parts))
  plot <- waffle(parts, 
                 rows = 5, colors = c("#a90099", "#EBEBEB"),
                 legend_pos="left",
                 use_glyph = 'child', 
                 glyph_size = 2.5, size = 1) +
    theme_enhance_waffle() +
    theme(panel.spacing = unit(0.25, "cm"),
          strip.background = element_blank(),
          strip.text = element_text(size = 11, 
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
          plot.margin = margin(-50,0,0,0), legend.box.just = "center");plot
  
  # Saving data points
  
  write.xlsx(as.data.frame(data2plot %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart, "A"),
             append    = T,
             row.names = T)
  
  # Saving panels
  
  saveIT.fn(chart  = plot,
            n      = nchart,
            suffix = "A",
            w      = 111.7642,
            h      = 65.72298)
}

# Lower Panel

figure16B_US.fn <- function(nchart = 16){

  discrimination <- data_subset.df %>%
    filter(country %in% mainCountry) %>%
    filter(year == 2021) %>%
    mutate(respect     = if_else(q16a < 4, 1, 
                                  if_else(q16a == 4 | q16a == 5 | q16a == 6, 0, NA_real_)),
           poor_service  =  if_else(q16b < 4, 1, 
                                    if_else(q16b == 4 | q16b == 5 | q16b == 6, 0, NA_real_)),
           no_smart      = if_else(q16c < 4, 1, 
                                    if_else(q16c == 4 | q16c == 5 | q16c == 6, 0, NA_real_)),
           afraid        =  if_else(q16d < 4, 1, 
                                    if_else(q16d == 4 | q16d == 5 | q16d == 6, 0, NA_real_)),
           harassed      =  if_else(q16e < 4, 1, 
                                    if_else(q16e == 4 | q16e == 5 | q16e == 6, 0, NA_real_)),
           young         =  if_else(age < 30, "Less than 30 years", 
                                    if_else(age > 29, "More than 30 years", NA_character_)),
           poor          =  if_else(fin == 1 | fin == 2, "Poor",
                                    if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
           area          =  if_else(Urban == 1, "Urban", "Rural"),
           gender        =  if_else(gend == 1, "Male", "Female"),
           diploma       =  if_else(edu == 5 | edu == 6| edu == 7, "High Education Level", 
                                    if_else(edu < 5, "No High Education Level", NA_character_)),
           color         =  if_else(ethni == "White", "White", "No white", NA_character_)
    )
  
  condition <- discrimination %>%
    select(respect, young, poor, area, gender, diploma, color) %>%
    mutate(counter = 1)
  
  age     <- condition_categories(main_data = condition, group_var = young, name_var = "young")
  income  <- condition_categories(main_data = condition, group_var = poor, name_var = "poor")
  area    <- condition_categories(main_data = condition, group_var = area, name_var = "area")
  gender  <- condition_categories(main_data = condition, group_var = gender, name_var = "gender")
  diploma <- condition_categories(main_data = condition, group_var = diploma, name_var = "diploma")
  color   <- condition_categories(main_data = condition, group_var = color, name_var = "color")
  
  selectables <- rbind(age, income, area, gender, diploma, color) %>%
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
             values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values),
             values = if_else(categories %in% "color" & values %in% "White", "1White", values)) %>%
      pivot_wider(id_cols = c(Yvar, id), names_from = categories, values_from = values)
    
    logit_data <- logit_data %>%
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
    
    summaryreg <- bind_rows(as.data.frame(coef(summary(models[[1]]))))
    
    margEff    <- margins_summary(models[[1]], data = models[[1]]$model)
    
    data2plot <- margEff
    
    data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \ninsecure",
                               "areaUrban" = "Urban", "youngLess than 30 years" = "Younger than 30",
                               "diplomaNo High Education Level" = "No Bachelor's degree", "colorNo white" = "Non-white")
    
    data2plot <- data2plot %>%
      mutate(category = mainCountry,
             order_variable = if_else(factor %in% "Female", 1,
                                      if_else(factor %in% "White", 2,
                                              if_else(factor %in% "Poor", 3,
                                                      if_else(factor %in% "Urban", 4, 
                                                              if_else(factor %in% "Young", 5, 6))))))
    return(list(data2plot, summaryreg))
  }
  
  data2plot_P1 <- logit_demo(mainData = discrimination, Yvar = respect)
  logit_plotP1 <- logit_demo_panel(mainData = data2plot_P1[[1]], 
                                 line_size = 1.25, 
                                 point_color = "#a90099", 
                                 line_color  = "#a90099") +
    scale_y_continuous(limits = c(-0.40, 0.40),
                       breaks = seq(-0.40, 0.40, by = 0.20),
                       expand = expansion(mult = 0.075), position = "right",
                       labels = c("-40", "-20", "0", "+20","+40")) +
    labs(y = "Less likely                   More likely");logit_plotP1
  
  saveIT.fn(chart  = logit_plotP1,
            n      = nchart,
            suffix = "B",
            w      = 87.16203,
            h      = 65.72298)
  
  data2plot_P2 <- logit_demo(mainData = discrimination, Yvar = afraid)
  logit_plotP2<- logit_demo_panel(mainData = data2plot_P2[[1]], 
                                  line_size = 1.25, 
                                  point_color = "#a90099", 
                                  line_color  = "#a90099") +
    scale_y_continuous(limits = c(-0.40, 0.40),
                       breaks = seq(-0.40, 0.40, by = 0.20),
                       expand = expansion(mult = 0.075), position = "right",
                       labels = c("-40", "-20", "0", "+20","+40")) +
    labs(y = "Less likely                   More likely");logit_plotP2
  
  saveIT.fn(chart  = logit_plotP2,
            n      = nchart,
            suffix = "C",
            w      = 87.16203,
            h      = 65.72298)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 17 - United States: Discrimination Bias                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure17_US.fn <- function(nchart = 17){
  
  vars4plot <- c("q17_1", "q17_2", "q17_3", "q17_4", "q17_5", "q17_6", "q17_7", "q17_8", 
                  "q17_9", "q17_10", "q17_11", "q17_12", "q17_13", "q17_14", "q17_15", "q17_99")
  
  data2plot <- data_subset.df %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(all_of(vars4plot)) %>%
    mutate(across(everything(),
      ~ case_when(
        .x == 1  ~ 1,
        .x == 0  ~ 0,
        .x == 99 ~ NA_real_
      ) 
    )) %>%
    rowwise() %>%
    mutate(
      total_experiences = sum(
        q17_1, q17_2, q17_3, q17_4, q17_5, q17_6, q17_7, q17_8, 
        q17_9, q17_10, q17_11, q17_12, q17_13, q17_14, q17_15,
        na.rm = T)
    ) %>%
    ungroup() %>%
    filter(total_experiences > 0 & q17_99 != 1) %>%
    summarise(across(!c(total_experiences, q17_99),
                     mean, 
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "avg") %>%
    arrange(-avg) %>%
    mutate(
      label = case_when(
        category == "q17_1" ~ "Ancestry or national \norigin",
        category == "q17_2" ~ "Gender",
        category == "q17_3" ~ "Race",
        category == "q17_4" ~ "Age",
        category == "q17_5" ~ "Religion",
        category == "q17_6" ~ "Height",
        category == "q17_7" ~ "Weight",
        category == "q17_8" ~ "Physical\nappearence",
        category == "q17_9" ~ "Physical or mental \ndisability",
        category == "q17_10" ~ "Sexual orientation",
        category == "q17_11" ~ "Education or\nincome level",
        category == "q17_12" ~ "Nationality or inmigration \nstatus",
        category == "q17_13" ~ "Skin color",
        category == "q17_14" ~ "Tribe",
        category == "q17_15" ~ "Clothing or\nhairstyle",
      )
    ) %>%
    top_n(n = 10, wt = avg) %>%
    mutate(
      percentage = to_percentage.fn(avg*100),
      # Converting labels into HTML syntax
      across(label,
             function(raw_label){
               html <- paste0("<span style='color:#000000;font-size:6.326276mm;font-weight:bold'>",  
                              percentage, "</span>",
                              "<br>",
                              "<span style='color:#524F4C;font-size:4.514598mm;font-weight:bold'>",
                              str_replace_all(raw_label, "\\n", "<br>"),
                              "</span>")
               return(html)
             }),
      order_var = seq_along(along.with = avg)
    )
  
  # Defining colors
  colors4plot        <- rosePalette
  names(colors4plot) <- data2plot %>% pull(category)
  
  # Applying plotting function
  chart <- LAC_roseChart(data = data2plot,
                         target_var    = "avg",
                         grouping_var  = "category",
                         alabels_var   = "label",
                         plabels_var   = "percentage",
                         order_var     = "order_var",
                         colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 189.7883,
            h      = 168.7007)

}
