#' Rsurveygizmo: A package for downloading survey and campaign data from Survey Gizmo.
#'
#' \pkg{Rsurveygizmo} allows you to access the Survey Gizmo (SG) REST API
#' directly from R
#' By specifying your API key and a survey project ID you are able to download
#' current SG survey responses and/or campaign data into an R \code{\link{data.frame}}.
#'
#' The two functions currently provided by \pkg{Rsurveygizmo} are
#' \code{\link{pullsg}} and \code{\link{pullsg_campaign}}. These functions retrieve response and campaign data from SG and includes a
#' variety of parameters to control the types of columns included within the returned object. \code{\link{pullsg}} optionally
#' downloads campaign data (email addresses, etc.) as part of its execution using the \code{\link{pullsg_campaign}} function, merging the returned sets.
#' @docType package
#' @name Rsurveygizmo
NULL
