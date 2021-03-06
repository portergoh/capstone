% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{ggplot_basic}
\alias{ggplot_basic}
\title{Wrapper for ggplot to include the basic attributes of a plot}
\usage{
ggplot_basic(data, var_x, var_y, var_fill = NULL, labs)
}
\arguments{
\item{var_x}{aes mapping for x axis}

\item{var_y}{aes mapping for y axis}

\item{var_fill}{aes mapping for fill attribute}

\item{labs}{tibble object that contains the ggplot attributes}

\item{df}{a data.frame (or list) from which the variables
in formula should be taken.}
}
\value{
ggplot object
}
\description{
Wrapper for ggplot to include the basic attributes of a plot
}
