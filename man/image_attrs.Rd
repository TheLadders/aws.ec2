% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Images_attrs.R
\name{get_image_attr}
\alias{get_image_attr}
\alias{reset_image_attr}
\alias{set_image_attr}
\title{AMI Attributes}
\usage{
get_image_attr(image, attribute, ...)

set_image_attr(image, attribute, value, description, operationtype,
  launchpermission, usergroup, userid, ...)

reset_image_attr(image, attribute, ...)
}
\arguments{
\item{image}{A character string containing an Amazon Machine Image (AMI) image ID or an object of class \dQuote{ec2_image}.}

\item{attribute}{A character string specifying one of: \dQuote{description}, \dQuote{kernel}, \dQuote{ramdisk}, \dQuote{launchPermission}, \dQuote{productCodes}, \dQuote{blockDeviceMapping}, \dQuote{sriovNetSupport}}

\item{...}{Additional arguments passed to \code{\link{ec2HTTP}}.}

\item{value}{\dots}

\item{description}{\dots}

\item{operationtype}{\dots}

\item{launchpermission}{\dots}

\item{usergroup}{\dots}

\item{userid}{\dots}
}
\value{
A list
}
\description{
Get, set, and reset AMI attributes
}
\examples{
\dontrun{
# RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
get_image_attr("ami-7f9dc615", "description")
}
}
\keyword{images}

