  # Function for plotting generation by fuel type for a specific week
  plot_confusion_matrix <- function(df, 
                                    input_classes, 
                                    input_model){
    
    df$Actual <- factor(df$Actual, levels = c("Adversity", "Comfortable", 
                                              "Affluent"))
    df$Prediction <- factor(df$Prediction, levels = c("Adversity", 
                                                      "Comfortable", 
                                                      "Affluent"))
    confusion_matrix_plot_theme <- theme(
      text = element_text(size = 18),
      plot.title = element_text(hjust = 0.5, colour = "#485785"),
      rect = element_rect(fill = "transparent"),
      axis.title.x = element_text(colour = "#485785"),
      axis.title.y = element_text(colour = "#485785"),
      plot.margin=unit(c(0.5,0.5,1.5,0.5), "cm"))
    
    # Filter 
    df_without_temperature <- df %>% 
      filter(classes == input_classes) %>% 
      filter(model == input_model) %>%
      filter(with_or_without_temperature == "Without Temperature")
    
    df_with_temperature <- df %>% 
      filter(classes == input_classes) %>% 
      filter(model == input_model) %>%
      filter(with_or_without_temperature == "With Temperature")
    
    # Create the plot
    plot_without_temperature <- ggplot(df_without_temperature, 
                                       aes(x = Actual, 
                                           y = Prediction, 
                                           fill = Freq)) + 
      geom_tile() + 
      coord_equal() +
      scale_fill_gradient() + 
      guides(fill = "none") + 
      geom_text(aes(label = Freq), color = "white", size = 6) + 
      labs(title = paste0("Without temperature \n Accuracy: ", 
                          round(df_without_temperature$accuracy, 3))) + 
      theme_minimal() + confusion_matrix_plot_theme
  
    plot_with_temperature <- ggplot(df_with_temperature, 
                                    aes(x = Actual, 
                                        y = Prediction, 
                                        fill = Freq)) + 
      geom_tile() + 
      coord_equal() +
      scale_fill_gradient() + 
      guides(fill = "none") + 
      geom_text(aes(label = Freq), color = "white", size = 6) + 
      labs(title = paste0("With temperature \n Accuracy: ", 
                          round(df_with_temperature$accuracy, 3)))+
      theme_minimal() + confusion_matrix_plot_theme
    
    figure <- grid.arrange(plot_without_temperature, 
                        plot_with_temperature,
                        ncol = 2)
  
    return(figure)
  }