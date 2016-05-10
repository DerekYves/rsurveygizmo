#' Rsurveygizmo: a method for downloading survey response data from the Survey Gizmo AWS servers
#'
#' \pkg{Rsurveygizmo} allows you to access the Survey Gizmo (SG) REST API
#' directly from the R console.
#' By specifying your API key and a survey project ID you are able to download
#' current SG survey responses into an R \code{\link{data.frame}}.
#'
#' The only function currently provided by \pkg{Rsurveygizmo} is
#' \code{\link{pullsg}}, which pulls data from the SG servers and includes a
#' variety of parameters to control the format of the returned
#' object (see the \code{\link{pullsg}} help file for details).
#' @docType package
#' @name Rsurveygizmo
NULL
