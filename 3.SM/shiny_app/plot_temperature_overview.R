# Function for plotting generation by fuel type for a specific week
plot_temperature_overview <- function(df, input_date){
  
  df$Acorn_grouped = factor(df$Acorn_grouped, levels = c("Affluent", 
                                                         "Comfortable",
                                                         "Adversity"))
  
  plot_theme <- theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, colour = "#485785"),
    axis.title.x = element_text(colour = "#485785"),
    axis.title.y = element_text(colour = "#485785"),
    rect = element_rect(fill = "transparent"),
    legend.title =  element_blank(),
    legend.text = element_text(size=16, colour = "#485785"),
    legend.direction = "horizontal",
    legend.box.just = "centre",
    legend.position = "top",
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent"),
    legend.key.height = unit(2,"cm"),
    legend.key.width = unit(2.5, "cm"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
  )
  
  # Filter 
  df <- df %>% filter(
    date >= input_date[1] & date <= input_date[2]
  ) %>% drop_na() 

  plot <- ggplot(df, aes(x = date)) + 
    geom_ribbon(aes(ymin = tmin, ymax = tmax), fill = "blue", alpha = 0.4) + 
    geom_line(aes(y = tavg, colour = "Average temperature")) + 
    labs(x = "Date",
         y = "Temperature (\u00B0C)") + 
    scale_color_manual(values = c("Average temperature" = "black")) + 
    plot_theme  
  return(plot)
}