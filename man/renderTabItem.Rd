% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generator.R
\name{renderTabItem}
\alias{renderTabItem}
\title{Render a tab item.}
\usage{
renderTabItem(tabName, module, moduleUIParameters)
}
\arguments{
\item{tabName}{Name of the tab that this item corresponds to.}

\item{module}{\code{tibble} with module metadata.
\code{tibble} format: \code{type}, \code{name}, \code{id}, \code{parameters}.}

\item{moduleUIParameters}{Module UI parameters.}
}
\value{
Rendered tab item.
}
\description{
Render a tab item.
}
\author{
Damian Rodziewicz
}
