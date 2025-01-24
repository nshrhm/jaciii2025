% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translation_config.R
\encoding{UTF-8}
\name{set_language}
\alias{set_language}
\title{Change Language Settings}
\usage{
set_language(lang = "ja")
}
\arguments{
\item{lang}{Language code ("ja" for Japanese or "en" for English)}
}
\description{
Changes the display language for the entire system with comprehensive
internationalization support.
}
\details{
Language System Features:
\itemize{
\item Global language configuration
\item Runtime language switching
\item Cascading settings application
\item Fallback handling for missing translations
}

Implementation Details:
\enumerate{
\item Sets global option 'movie_analysis.lang'
\item Updates active translation mappings
\item Refreshes display components
\item Validates language selection
}

Supported Languages:
\itemize{
\item Japanese (ja): Default language
\item English (en): Alternative language
\item Fallback to English for unsupported languages
}

Configuration Impact:
\itemize{
\item Plot labels and titles
\item Error messages
\item Statistical output
\item Documentation strings
}
}
\examples{
# Set to Japanese
set_language("ja")

# Set to English
set_language("en")

}
\seealso{
\itemize{
\item \code{\link{get_translation}} - For retrieving translated strings
\item \code{\link{get_error_message}} - For localized error messages
}
}
\concept{internationalization}
\concept{configuration}
