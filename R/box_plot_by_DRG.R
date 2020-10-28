## Description is in the box_plot_by_DRG.Rd file.
## function
box_plot_by_DRG <-
  function(df,
           ## set values for vary
           vary = c('Average.Medicare.Payments',
                    'Average.Total.Payments',
                    'Average.Covered.Charges')) {
    library(tidyverse)
    df %>%
      ## extract DRG code from DRG Definition
      mutate(DRG_code = substring(DRG.Definition, 1, 3)) %>%
      ## set the x, y, and color for the plot
      ggplot(aes(x = DRG_code, y = get(vary), color = DRG_code)) +
      ## make a box plot
      geom_boxplot() +
      ## delete the legend
      theme(legend.position = 'none') +
      ## change the angle of text
      theme(axis.text.x = element_text(angle = -45))+
      ## set label for x-axis
      xlab("DRG Code") +
      ## set label for y-axis
      ylab(gsub('\\.', ' ', as.character(vary))) +
      ## add a title for the plot
      ggtitle(paste0(
        "Box Plot of ",
        gsub('\\.', ' ', as.character(vary)),
        "\nGrouped By DRG Code"
      ))
  }

