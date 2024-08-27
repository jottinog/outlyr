library(tidyverse)

outlyr <- function(x, y, group = NULL, outlier = c('z', 'iqr', 'mad'), threshold = 3, treat = c('replace', 'win', 'trim')) {
  # Test if provided vars are numeric. If not, return an error message.
  if (isFALSE(all(sapply(x %>% select(matches(y)), function(x) is.numeric(x))))) {
    stop('No numeric variables provided')
  } else {
    # If group is defined then scale within-group (z/iqr/mad) and specify treat method (win/replace/trim)
    if (!is.null(group)) {
      x <- x %>% group_by(!!!rlang::syms(group))
    }
    
    # Select variables that match the pattern in 'y'
    selected_vars <- x %>% select(matches(y))
    
    # Detect outliers based on the specified method
    if (outlier == 'z') {
      selected_vars <- selected_vars %>%
        mutate_if(is.numeric, ~ifelse(scale(.) >= threshold, 999,
                                      ifelse(scale(.) <= -threshold, -999, .)))
    } else if (outlier == 'iqr') {
      selected_vars <- selected_vars %>%
        mutate_if(is.numeric, ~ifelse(. >= boxplot.stats(.)$stats[5], 999,
                                      ifelse(. <= boxplot.stats(.)$stats[1], -999, .)))
    } else if (outlier == 'mad') {
      median_val <- selected_vars %>% summarise(across(everything(), median, na.rm = TRUE))
      mad_val <- selected_vars %>% summarise(across(everything(), mad, na.rm = TRUE))
      selected_vars <- selected_vars %>%
        mutate(across(everything(), ~ifelse(abs(. - median_val) > threshold * mad_val, 999, .)))
    }
    
    # Apply the selected treatment method
    if (treat == 'win') {
      selected_vars <- selected_vars %>%
        mutate_if(is.numeric, ~ifelse(. == 999, NA, .)) %>%
        mutate_if(is.numeric, ~ifelse(is.na(.), max(., na.rm = TRUE), .)) %>%
        mutate_if(is.numeric, ~ifelse(. == -999, NA, .)) %>%
        mutate_if(is.numeric, ~ifelse(is.na(.), min(., na.rm = TRUE), .))
    } else if (treat == 'replace') {
      selected_vars <- selected_vars %>%
        mutate_if(is.numeric, ~ifelse(. == 999 | . == -999, NA, .)) %>%
        mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))
    } else if (treat == 'trim') {
      selected_vars <- selected_vars %>%
        mutate_if(is.numeric, ~ifelse(. == 999 | . == -999, NA, .))
    }
    
    # Recombine the treated variables with the original data
    x <- x %>% select(-matches(y)) %>% bind_cols(selected_vars)
    
    return(x)
  }
}
