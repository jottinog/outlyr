outlyr <- function(x, y, group = NULL, outlier = c('z', 'iqr'), treat = c('replace', 'win', 'trim'), threshold = NULL) {
  # Set default thresholds --> if Z default is 3, if IQR default is 1.5
  outlier <- match.arg(outlier)
  if (is.null(threshold)) {
    threshold <- if (outlier == 'z') 3 else 1.5
  }
  
  # Test if provided vars are numeric. If not, return an error message.
  if (isFALSE(all(sapply(x %>% select(matches(y)), is.numeric)))) {
    stop('no numeric variables provided')
  } else {
    if (!is.null(group) & outlier == 'z') {
      x %>%
        group_by(!!!rlang::syms(group)) %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(scale(.) >= threshold, 999,
                                            ifelse(scale(.) <= -threshold, -999, .))) -> tmp
    } else if (!is.null(group) & outlier == 'iqr') {
      x %>%
        group_by(!!!rlang::syms(group)) %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(. >= quantile(., 0.75) + threshold * IQR(.), 999,
                                            ifelse(. <= quantile(., 0.25) - threshold * IQR(.), -999, .))) -> tmp
    } else if (is.null(group) & outlier == 'z') {
      x %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(scale(.) >= threshold, 999,
                                            ifelse(scale(.) <= -threshold, -999, .))) -> tmp
    } else if (is.null(group) & outlier == 'iqr') {
      x %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(. >= quantile(., 0.75) + threshold * IQR(.), 999,
                                            ifelse(. <= quantile(., 0.25) - threshold * IQR(.), -999, .))) -> tmp
    }

    # Apply the selected treatment method
    if (treat == 'win') {
      tmp %>%
        mutate_if(is.numeric, ~ifelse(. == 999, NA, .)) %>%
        mutate_if(is.numeric, ~ifelse(is.na(.), max(., na.rm = TRUE), .)) %>%
        mutate_if(is.numeric, ~ifelse(. == -999, NA, .)) %>%
        mutate_if(is.numeric, ~ifelse(is.na(.), min(., na.rm = TRUE), .)) -> tmp
    } else if (treat == 'replace') {
      tmp %>%
        mutate_if(is.numeric, ~ifelse(. == 999 | . == -999, NA, .)) %>%
        mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) -> tmp
    } else {
      tmp %>%
        mutate_if(is.numeric, ~ifelse(. == 999 | . == -999, NA, .)) -> tmp
    }
    
    # Return the modified dataframe
    return(tmp)
  }
}
