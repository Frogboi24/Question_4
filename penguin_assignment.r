## ---------------------------
##
## Script name: penguin_assignment.r
##
## Purpose of script: 
##      Loads penguin data, cleans it, runs a linear regression of body mass against 
##      culmen length, plots the linear regression and data
##      and saves the plot to a file.
##
## Author: 1053660
##
## Date Created: 2022-12-08
##
##
## Notes:
##   
##
## ---------------------------

# This first section is just to give some background on the code used, 
# you don't need to run any of the code until the "Load the libraries and the functions" section. 

#the versions of the packages used to do this analysis and produce this plot can be found in the "package_versions" folder

# load the libraries 
library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(ragg)
library(svglite)

# the code above is stored in "libraries.r" in the "functions" folder so the libraries can also be loaded by
# running the code below:
source("functions/libraries.r")

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

# ---------------------------
# Define Functions
# ---------------------------

# ---- Cleaning --------------

# Clean column names, remove empty rows, remove columns called comment and delta
cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

# Subset the data to only include Adelie penguins that are not NA for the culmen length and body mass
remove_empty_cul_bod_adelie <- function(data_clean){
  data_clean %>%
    filter(!is.na(culmen_length_mm), !is.na(body_mass_g), species == "Adelie Penguin (Pygoscelis adeliae)") %>%
    select(species, culmen_length_mm, body_mass_g)
}
# The cleaning functions have been saved in "cleaning.r" in the "functions" folder. 
# they can be loaded into this script by running the code below:
source("functions/cleaning.r")

# ---- Plotting --------------

# Plot the culmen length against body mass data with the linear regression overlaid
plot_culbod_figure <- function(adelie_culbod){
  adelie_culbod %>% 
    ggplot(aes(x = body_mass_g, y = culmen_length_mm)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(x = "Body Mass (g)",
         y = "Culmen Length (mm)", title = "Linear regression of Culmen Length against Body Mass for Adelie penguins") +
    theme_bw()
}

# ---- Saving --------------

# Save the plot as a svg and define the size and scaling
save_culbod_figure_svg <- function(adelie_culbod, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  culbod_figure <- plot_culbod_figure(adelie_culbod)
  print(culbod_figure)
  dev.off()
}
# the plotting and saving functions are saved in "plotting.R" in the "functions" folder. 
# they can be loaded into this script using the code below:
source("functions/plotting.r")


# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

# run the code below to perform the analysis and produce the plot

# Set working directory 

# setwd("path to the folder you are working in")

# ---------------------------
# Load the libraries and the functions
# ---------------------------

source("functions/libraries.r")
source("functions/cleaning.r")
source("functions/plotting.r")

# ---------------------------
# Load the data
# ---------------------------

penguins_raw <- read.csv("data_raw/penguins_raw.csv")

# ---------------------------
# Clean the data
# ---------------------------

# Fix the column names, remove empty rows, remove columns called comment and delta
penguins_clean <- cleaning(penguins_raw)

# Save the cleaned data
write.csv(penguins_clean, "data_clean/penguins_clean.csv")

# Subset the data to only include Adelie penguins that are not NA for the culmen length and body mass
adelie_culbod <- remove_empty_cul_bod_adelie(penguins_clean)

# -------------------------------------------------
# perform a linear regression on the data 
# -------------------------------------------------

# here is the structure of the lm function for doing a linear regression: 
# mod_name <- lm(response_variable ~ explanatory_variable, data)
culbod_mod <- lm(culmen_length_mm ~ body_mass_g, adelie_culbod)

# create a qqplot to test the assumption that the residuals are normally distributed
plot(culbod_mod, which = 2)

#create a residuals vs fitted graph to test the equal variance assumption
plot(culbod_mod, which = 1)

# looking at the two graphs we can see that the assumptions are met because:
# in the qqplot, the points fall mainly along the line. 
# in the residuals vs fitted plot, no residuals stand out from the rest; 
# residuals form a horizontal band around 0; 
# and the residuals bounce randomly around the line. 

#view the linear regression summary table
summary(culbod_mod)

#and save it to the "out" folder
sink(file = "out/summary_table.txt")
summary(culbod_mod)
sink()

# ---------------------------
# Plot the data
# ---------------------------

# Plot the culmen length against body mass data with the linear regression overlaid
culbod_figure <- plot_culbod_figure(adelie_culbod)

# ---------------------------
# Save the Figure
# ---------------------------

# Save the plot as a vector (no resolution needed), define the size and scaling and save it to the "out" folder
save_culbod_figure_svg(adelie_culbod, "out/culbod_figure_vector.svg", 
                      size = 15, scaling = 0.85)
