# 2022 LAC-GPP Country Reports

## Description
The present repository contains the R code used by the DAU to produce all the visualizations used by the 2022 LAC-GPP Country Reports. In order to prepare these VIZs, the DAU followed the specifications sent by the communications and design team (see the Inpits/full_specs.pdf file).

## Files description
In this repository you will find three main folders:

1. Code
2. Data
3. Outputs

The `Code` directory is where all the R scripts are stored with exception of the RunMe.R file. The `Data` directory contains the data to be used for the reports and the `Outputs` directory is where the VIZs outcomes are saved.

The data to be used in this project is a subset from the merged.dta file managed by the organization. The VIZs are stored in SVG format following this filing system: `COUNTRY_NAME/FIGURE_NUMBER/FILE_NAME.svg`

Due to the privacy discloures followed by the DAU (see the [DAU R Coding Handbook](https://ctoruno.quarto.pub/wjp-r-handbook/)), the contents of the `Data` and `Outputs` directories are limited on the GitHub repository and their full content can only be accessed through the WJP SharePoint.

## Code logic

The codde is divided in three levels:

1. Upper level: A single file controls all the program and it is the `RunMe.R` script. This is a single-click code and it is designed to just write down the name of the country we would like to generate the plots for and then run the entire script.

2. Defining Functions: The RunMe file will call several defining functions. One per figure or VIZ to generate. For example, to generate the SVG file for Figure 01 in the report, RunMe will call out the `figure01.fn()` function. These functions can be found in the `Code` directory inside their respective SECTION. For example, given that Figure 1 is in section 1 in the report. The defining function `figure01.fn()` can be found in the `Code/S01.R` file.

3. Plotting functions: The role of the defining functions is to define the data to be used in a specific figure. However, the plotting is performed by a plotting function. There is one plotting function per chart type. For example, all line charts in this report are plotted by `the LAC_lineChart()` function. Therefore, the same plotting function can be called by several different defining functions in order to generate the same type of chart. All the plotting functions are publicly available in this [GitHub repository](https://github.com/ctoruno/WJP-Data-Viz/tree/main/LAC).

Finally, there is an auxiliary file called `setting.R`. As its name could suggest, inside this file are all the pre-settings needed by the `RunMe.R` to perform smoothly. All the fonts, the ggplot theme, color palettes, and other related settings can be found here.

![Code Logic](Inputs/code_structure.png)

## Contact
For inqueries please contact _Carlos Toruño_ (ctoruno@worldjusticeproject.org) or _Santiago Pardo_ (spardo@worldjusticeproject.org). For general information, please contact _Ana María Montoya_ (amontoya@worldjusticeproject.org).
