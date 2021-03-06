% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizePolygons.R
\name{visualizePolygonsUI}
\alias{visualizePolygonsUI}
\alias{visualizePolygons}
\title{Visualize \code{SpatialPolygons*} (shiny module)}
\usage{
visualizePolygonsUI(id, ...)

visualizePolygons(input, output, session, poly, proxy = NULL)
}
\arguments{
\item{id}{An id string that corresponds with the id used to call the module's UI function.}

\item{...}{Additional parameters passed to \code{link[leaflet]{leafletOutput}}.}

\item{input}{shiny server input object}

\item{output}{shiny server output object}

\item{session}{shiny server session object}

\item{poly}{Reactive value with polygon to visualize.}

\item{proxy}{Proxy to a leaflet map on which polygons should be displayed.
See \code{\link[leaflet]{leafletProxy}}}
}
\value{
None. Invoked for the side-effect of generating UI for a leaflet map.

None. Invoked for the side-effect of rendering leaflet map.
}
\description{
Display a \code{SpatialPolygons*} objects on a leaflet map.
}
