## Description is in the stat_of_avg_med_payment.Rd file.
## function
stat_of_avg_med_payment <-
  function(df,
           ## set values for stat
           stat = c('mean',
                    'median',
                    'sd')) {
    library(tidyverse)
    df_new <- df %>%
      ## extract DRG code from DRG Definition
      mutate(DRG_code = substring(DRG.Definition, 1, 3)) %>%
      ## select the columns
      select(DRG_code, Average.Medicare.Payments) %>%
      ## group the rows by DRG Code
      group_by(DRG_code) %>%
      ## compute mean, median, and sd of AMP
      summarise(
        mean = mean(Average.Medicare.Payments),
        median = median(Average.Medicare.Payments),
        sd = sd(Average.Medicare.Payments)
      )
    df_new %>%
      ## select column by the input stat
      select(DRG_code, as.character(stat)) %>%
      ## make a table for the data
      kableExtra::kable(
        ## set column names
        col.names = c("DRG Code", stringr::str_to_title(stat)),
        ## set position of the data: 'c':center, 'r': right
        align = 'cr',
        ## add a title for the table
        caption = paste0(stringr::str_to_title(stat),
                         " of Average Medicare Payments")) %>%
      ## change the format of the table
      kableExtra::kable_styling("striped", "float_left", full_width = F)
  }
