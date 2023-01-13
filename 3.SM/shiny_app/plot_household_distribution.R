plot_household_distribution <- function(df, input_group) {
  df <- df %>% filter(Group %in% input_group)
  PieDonut(df, aes(Group, Tariff), ratioByGroup = FALSE, 
           labelposition = 1,
           pieLabelSize = 4,
           donutLabelSize = 4
           )
}