# Plot the culmen length against body mass data with the linear regression overlaid
plot_culbod_figure <- function(adelie_culbod){
  adelie_culbod %>% 
    ggplot(aes(x = body_mass_g, y = culmen_length_mm)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(x = "Body Mass (g)",
         y = "Culmen Length (mm)", title = "Linear regression of Culmen Length against Body Mass for Adelie penguins") +
    theme_bw()
}

# Save the plot as a svg and define the size and scaling
save_culbod_figure_svg <- function(adelie_culbod, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  culbod_figure <- plot_culbod_figure(adelie_culbod)
  print(culbod_figure)
  dev.off()
}
