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
      COLOR, income_aux, gender, disability2, disability, Urban, age, edu,
      # Variables related to institutions perfomance
      q48b_G1, q48f_G1, q49a, CAR_q58_G1, q48f_G2, q48g_G2, 
      # Trust in institutions
      q1c, q1d, q1e, q1g, q1i, q41d) %>%
    # This variable assigns the victim condition to each observation
    mutate(victim = if_else(EXP_q8a_1 == 1 | EXP_q8a_2 == 1 | EXP_q8a_3 == 1 | EXP_q8a_4 == 1 | EXP_q8a_5 == 1 | EXP_q8a_6 == 1 | EXP_q8a_7 == 1 |
                              EXP_q8a_8 == 1 | EXP_q8a_9 == 1 | EXP_q8a_10 == 1 | EXP_q8a_11 == 1 | EXP_q8a_12 == 1 | EXP_q8a_13 == 1 | EXP_q8a_14 == 1 |
                              EXP_q8b_1 == 1 | EXP_q8b_2 == 1 | EXP_q8b_3 == 1 | EXP_q8b_4 == 1 | EXP_q8b_5 == 1, 1, 0, 0))
  
  return(security.universe)
} # This is going to be refactoring

security_universe <- security.universe(master_data = data_subset.df) # This function assign the victim condition and select the main variables to security secction

victims <- security_universe %>%
  summarise(victim = round(mean(victim, na.rm = T),2)) %>%
  mutate(non_victim = 1 - victim)

report <- security_universe %>%
  mutate(EXP_q8d = if_else(EXP_q8d == 1, 1, 0)) %>%
  filter(victim == 1) %>%
  summarise(report = round(mean(EXP_q8d, na.rm = T),2)) %>%
  mutate(non_report = 1 - report)

fill_report <- security_universe %>%
  filter(victim == 1) %>%
  mutate(EXP_q8d = if_else(EXP_q8d == 1, 1, 0),
         EXP_q8f = if_else(EXP_q8f == 1, 1, 0)) %>%
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

y <- c(1, 1000, -300, 600, 75)
x <- c(0.6, 2, 2.3, 3.3, 3.3)
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
                               next_node = NULL, fill = NULL), 
                fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0.225,0.225))) +
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
        axis.text.x = element_text(family ="Lato Black", 
                                   size = 3.514598*.pt, 
                                   hjust = -0.0,
                                   color = "Black"),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank());pl
pl

saveIT.fn(chart  = pl,
          n      = 13,
          suffix = "B",
          w      = 175.027,
          h      = 94.54267)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 14                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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
                      if_else(!is.na(.x), 0, NA_real_)))
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


