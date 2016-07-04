#' @title Create volume
#' @description Create a new EBS-backed volume
#' @param availability_zone the zone in which to create the volume
#' @param volume_type the type of volume (ie gp2, io1)
#' @param size optionally, the size of the new volume in GB (when missing, defaults to size of snapshot)
#' @param snapshot optionally, a snapshot-id from which the volume will be instantiated
#' @param encrypted whether or not to encrypt the volume
#' @param iops the number of iops to be provisioned to the volume
#' @return A list containing the results of the API call
create_volume <- function(availability_zone, volume_type, size,
                          snapshot, encrypted, iops, ...) {
  query <- list(Action = "CreateVolume",
                AvailabilityZone = availability_zone,
                VolumeType = volume_type)
  if (!missing(snapshot)) query$SnapshotId <- snapshot
  if (!missing(iops)) query$Iops <- iops
  if (!missing(encrypted)) query$Encrypted <- encrypted
  if (!missing(size)) query$Size <- size
  r <- ec2HTTP(query = query, ...)
  structure(flatten_list(r), class = "ec2_volume")
}

delete_volume <- function() {}

#' @title Attach volume
#' @description Attach a volume to an instance on the given device
#' @return A list containing the results of the API call
#' @export
attach_volume <- function(device, instance, volume, ...) {
  query <- list(Action = "AttachVolume",
                VolumeId = get_volumeid(volume),
                Device = device,
                InstanceId = get_instanceid(instance))
  
  r <- ec2HTTP(query = query, ...)
  
  structure(flatten_list(r), class = "ec2_volume")
}

#' @title Detach volume
#' @description Detach a volume to an instance on the given device
#' @return A list containing the results of the API call
#' @export
detach_volume <- function(device, instance, volume,
                          force, ...) {
  query <- list(Action = "DetachVolume",
                VolumeId = get_volumeid(volume),
                Device = device,
                InstanceId = get_instanceid(instance))
  if (!missing(force))  query$Force <- force
  r <- ec2HTTP(query = query, ...)
  
  structure(flatten_list(r), class = "ec2_volume")   
}

enable_volume_io <- function() {}

get_volume_attr <- function() {}

set_volume_attr <- function() {}

#' @title Describe volumes
#' @description Search/Describe volumes
#' @template filter
#' @param n \dots
#' @param page \dots
#' @template dots
#' @return A list of lists, each list containing API returned data on matching snapshots
#' @export
describe_volumes <- function(filter, n, page, ...) {
  query <- list(Action = "DescribeVolumes")
  if (!missing(filter)) {
    filter <- as.list(filter)
    query <- c(query, .makelist(filter, type = "Filter"))
  }
  if (!missing(n)) {
    if(n > 1000) {
      warning("'n' coerced to 1000 (the maximum)")
      n <- 1000
    }
    query$MaxResults <- n
  }
  if (!missing(page)) {
    query$NextToken <- page
  }
  r <- ec2HTTP(query = query, ...)
  structure(flatten_list(r[[2]]), class = "ec2_volume")
}

# NOT COMPLETE
# volume_status <- function(volumes, ...) {
#   query <- list(Action = "DescribeVolumeStatus")
#   if (!missing(filter)) {
#     filter <- as.list(filter)
#     query <- c(query, .makelist(filter, type = "Filter"))
#   }
#   r <- ec2HTTP(query = query, ...)
#   structure(flatten_list(r[[2]]), class = "ec2_volume")
# }




copy_snapshot <- function() {}

#' @title Create snapshot
#' @description Create a snapshot of an EBS volume
#' @param volume the volume-id of the volume to be snapshotted
#' @param description a description (metadata) for the volume
#' @return A list containing the results of the API call
#' @export
create_snapshot <- function(volume, description, ...) {
  query <- list(Action = "CreateSnapshot",
                VolumeId = get_volumeid(volume))
  if (!missing(description)) {
    query$Description = description
  }
  r <- ec2HTTP(query = query, ...)
  structure(flatten_list(r), class = "ec2_snapshot")
}

#' @title Delete snapshot
#' @description Delete a given shapshot
#' @param snapshot the snapshot-id to be deleted
#' @return A list containing the results of the API call
#' @export
delete_snapshot <- function(snapshot, ...) {
  query <- list(Action = "DeleteSnapshot",
                SnapshotId = get_snapshotid(snapshot))
  r <- ec2HTTP(query = query, ...)
  
  structure(flatten_list(r), class = "ec2_snapshot")
}
get_snapshot_attr <- function() {}

set_snapshot_attr <- function() {}

reset_snapshot_attr <- function() {}

#' @title Describe snapshots
#' @description Search/Describe snapshots
#' @template filter
#' @param n \dots
#' @param page \dots
#' @template dots
#' @return A list of lists, each list containing API returned data on matching snapshots
#' @export
describe_snapshots <- function(filter, n, page, ...) {
  query <- list(Action = "DescribeSnapshots")
  if (!missing(filter)) {
    filter <- as.list(filter)
    query <- c(query, .makelist(filter, type = "Filter"))
  }
  if (!missing(n)) {
    if(n > 1000) {
      warning("'n' coerced to 1000 (the maximum)")
      n <- 1000
    }
    query$MaxResults <- n
  }
  if (!missing(page)) {
    query$NextToken <- page
  }
  r <- ec2HTTP(query = query, ...)
  structure(flatten_list(r[[2]]), class = "ec2_snapshot")
}

