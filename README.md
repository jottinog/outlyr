# **OUTLYR – Detect and treat outliers in R** :rocket:

## What is it

This is an R function heavily rooted on **dplyr** grammar. It is intended to be used to check outliers' influence in your analysis. It returns a copy of your dataset with outliers treated (i.e., trimmed, replaced, or winsorized). Re-run your test using this new dataset to assess how influential outliers were. It handles multiple variables at the time (as many as listed in 'vars'). Operations are done column-wise. If the 'group' argument is defined, these will be done on each group separately. 

**\*PLEASE NOTE\*** Neither removing nor changing outliers should be your first option. This is not a front-end step to *clean* your data. There are reasons to believe that doing so is actually counter-producent. Check Dr. Nick Holmes (@TheHandLab) thread to learn more about it: https://twitter.com/TheHandLab/status/1434097550840246279

Robust (**robustbase**) or non-parametric tests, or even data transformations should be prioritized. 

Check **LambertW** and **bestNormalize** for more aggresive data transforms approaches if classical methods fail.

## Usage

outlyr(x, y, group, method, treat) 

 x – your full dataset. 
 
 y – character vector with names of variables from the dataset to examine.
 
 group – (Optional) string indicating name of the grouping variable. If filled all further steps are done within-group.
 
 outlier – how flag and treat outliers. 'z' flags values outside ± 3.28 SD range. 'iqr' flags values outside 1.5 * IQR range.
 
 treat – how treat outliers. 'trim' set them to NA. 'win' replace them by max/min. 'replace' does mean-replacement.
 
 ### Example:

    library(tidyverse)
    
    data(iris)

    vars <- c('Petal.Width', 'Petal.Length')  # List the variables you want to look up for.

    new_iris <- outlyr(iris, vars, group = 'Species', outlier = 'iqr', treat = 'win')  # Within-group ('Species' defined in group argument). Outliers defined by IQR method and winsorized.
    