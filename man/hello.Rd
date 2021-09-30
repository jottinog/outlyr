\name{Description}
\alias{@jottino}
\title{Outlyr}
\usage{
outlyr(x, y, group, method, treat)

x - dataframe object
y - string vector w/variable names in dataframe
group - name of group variable in dataframe (optional)
method - flag outliers based on 'iqr' (1.5*IQR) or 'z' (±3.28 SD)
treat - 'trim' (NA), 'replace' (by mean) or 'win' (winsorize) outliers
}
\description{
Returns a dataframe with outliers treated for the specified variables.

If group argument is defined, outliers will be further treated group-wise.

*PLEASE NOTE* removing outliers can be counter-producent (see https://twitter.com/TheHandLab/status/1434097550840246279).
}
\examples{
library(tidyverse)

data(iris)

vars <- c('Petal.Width', 'Petal.Length')

new_iris <- outlyr(iris, vars, group = 'Species', method = 'iqr', treat = 'win')
}

