library(tidyverse)

outlyr <- function(x, y, group = NULL, outlier = c('z', 'iqr'), treat = c('replace', 'win', 'trim')) {
  # Test if provided vars are numeric. If not return error message.
  if (isFALSE(all(sapply(x %>% select(matches(y)), function(x) is.numeric(x))))) {
    stop('no numeric variables provided') }
  # If group is defined then scale within-group (z/iqr) and specify treat method (win/replace/trim)
  else {
    if (!is.null(group) & outlier == 'z') {
      x %>%
        group_by(!!!rlang::syms(group)) %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(scale(.) >= 3.28, 999,
                                            ifelse(scale(.) <= -3.28, -999, .))) -> tmp
      if (treat == 'win') {
        tmp %>%
          group_by(!!!rlang::syms(group)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), max(., na.rm = T), .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), min(., na.rm = T), .))
      }
      else if (treat == 'replace') {
        tmp %>%
          group_by(!!!rlang::syms(group)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), mean(., na.rm = T), .))
      }
      else {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .))
      }
    }
    else if (!is.null(group) & outlier == 'iqr') {
      x %>%
        group_by(!!!rlang::syms(group)) %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(. >= boxplot.stats(.)$stats[5], 999,
                                            ifelse(. <= boxplot.stats(.)$stats[1], -999, .))) -> tmp
      if (treat == 'win') {
        tmp %>%
          group_by(!!!rlang::syms(group)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), max(., na.rm = T), .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), min(., na.rm = T), .))
      }
      else if (treat == 'replace') {
        tmp %>%
          group_by(!!!rlang::syms(group)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), mean(., na.rm = T), .))
      }
      else {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .))
      }
    }
    else if (is.null(group) & outlier == 'z') {
      x %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(scale(.) >= 3.28, 999,
                                            ifelse(scale(.) <= -3.28, -999, .))) -> tmp
      if (treat == 'win') {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), max(., na.rm = T), .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), min(., na.rm = T), .))
      }
      else if (treat == 'replace') {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), mean(., na.rm = T), .))
      }
      else {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .))
      }
    }
    else if (is.null(group) & outlier == 'iqr') {
      x %>%
        select(matches(y)) %>%
        mutate_at(vars(matches(y)), ~ifelse(. >= boxplot.stats(.)$stats[5], 999,
                                            ifelse(. <= boxplot.stats(.)$stats[1], -999, .))) -> tmp
      if (treat == 'win') {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), max(., na.rm = T), .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(. == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), min(., na.rm = T), .))
      }
      else if (treat == 'replace') {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .)) %>%
          mutate_if(~is.numeric(.), ~ifelse(is.na(.), mean(., na.rm = T), .))
      }
      else {
        tmp %>%
          mutate_if(~is.numeric(.), ~ifelse(. == 999 | . == -999, NA, .))
      }
    }
  }
}
