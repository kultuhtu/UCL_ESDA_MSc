# Function for plotting generation by fuel type for a specific week
plot_electricity_consumption <- function(df, 
                                  input_filter,
                                  input_group){
  
  df$Acorn_grouped <- factor(df$Acorn_grouped, levels = c("Affluent", 
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
  if (input_filter == "Day of Month") {
    df <- df %>% filter(Acorn_grouped %in% input_group) %>% 
      group_by(day, Acorn_grouped) %>% 
      summarise(kwh_per_hh_sum = mean(kwh_per_hh_sum, na.rm = TRUE),
                tavg = mean(tavg, na.rm = TRUE)) %>% 
      ungroup()
    df <- df %>% drop_na()
    plot <- ggplot(df, aes(x = day, y = kwh_per_hh_sum, color = Acorn_grouped)) + 
      geom_line() + 
      scale_x_continuous(breaks = seq(1, 30, 2), lim = c(1, 30)) + 
      scale_color_manual(values = c("#41C25E", "#79AAFC", "#F28B87")) + 
      labs(x = "Day of Month",
           y = "Average Electricity Consumption (kWh)", 
           color = "Group") + plot_theme
    
  } else if (input_filter == "Week") {
    df <- df %>% filter(Acorn_grouped %in% input_group) %>% 
      group_by(week, Acorn_grouped) %>% 
      summarise(kwh_per_hh_sum = sum(kwh_per_hh_sum, na.rm = TRUE),
                tavg = mean(tavg, na.rm = TRUE)) %>% 
      ungroup()
    df <- df %>% drop_na()
    plot <- ggplot(df, aes(x = week, y = kwh_per_hh_sum, 
                           color = Acorn_grouped)) + 
      geom_line() + 
      scale_x_continuous(breaks = seq(1, 52, 5), lim = c(1, 52)) + 
      scale_color_manual(values = c("#41C25E", "#79AAFC", "#F28B87")) + 
      labs(x = "Week",  
           y = "Average Electricity Consumption (kWh)",
           color = "Group") + plot_theme
  } else if (input_filter == "Month") {
    df <- df %>% filter(Acorn_grouped %in% input_group) %>% 
      group_by(month, Acorn_grouped) %>% 
      summarise(kwh_per_hh_sum = sum(kwh_per_hh_sum, na.rm = TRUE),
                tavg = mean(tavg, na.rm = TRUE)) %>% 
      ungroup() 
    df <- df %>% drop_na()
    plot <- ggplot(df, aes(x = month, y = kwh_per_hh_sum, 
                           color = Acorn_grouped)) + 
      geom_line() + 
      scale_x_continuous(breaks = seq(1, 12, 1), lim = c(1, 12)) + 
      scale_color_manual(values = c("#41C25E", "#79AAFC", "#F28B87")) + 
      labs(x = "Month",  
           y = "Average Electricity Consumption (kWh)",
           color = "Group") + plot_theme
  }
  return(plot)
}