% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viewing_intention.R
\encoding{UTF-8}
\name{create_coefficient_plot}
\alias{create_coefficient_plot}
\title{Generate Coefficient Plot}
\usage{
create_coefficient_plot(analysis_results, lang = "ja")
}
\arguments{
\item{analysis_results}{Result list from analyze_viewing_intention}

\item{lang}{Language setting ("ja": Japanese, "en": English)}
}
\value{
ggplot object (coefficient plot)
}
\description{
Generates a visualization plot of standardized coefficients from the regression analysis
results. The plot displays coefficient magnitudes and confidence intervals to facilitate
comparison of factor influences.
}
\details{
Plot features:
\itemize{
\item Point plot of standardized coefficients
\item Display of 95\% confidence intervals
\item Translation-enabled factor names
}
}
\concept{visualization}
