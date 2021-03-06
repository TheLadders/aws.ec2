% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EBS.r
\name{create_volume}
\alias{create_volume}
\title{Create volume}
\usage{
create_volume(availability_zone, volume_type, size, snapshot, encrypted, iops,
  ...)
}
\arguments{
\item{availability_zone}{the zone in which to create the volume}

\item{volume_type}{the type of volume (ie gp2, io1)}

\item{size}{optionally, the size of the new volume in GB (when missing, defaults to size of snapshot)}

\item{snapshot}{optionally, a snapshot-id from which the volume will be instantiated}

\item{encrypted}{whether or not to encrypt the volume}

\item{iops}{the number of iops to be provisioned to the volume}
}
\value{
A list containing the results of the API call
}
\description{
Create a new EBS-backed volume
}

