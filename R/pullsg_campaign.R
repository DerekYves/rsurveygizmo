#' Download survey campaign data from Survey Gizmo, storing an R dataframe.
#'
#' This function downloads campaign data from Survey Gizmo (SG), saving
#' the returned email/response data as an R \code{\link{data.frame}}. Because SG limits the size of JSON data pulls via the API (currently, the limit is 250),
#' it calculates the number of pulls needed to download the entire response set and binds the returned frames. The "contactid" field joins email address and other campaign data with the survey response objects (see \code{\link{pullsg}}).
#' Note that this function only downloads contact object data that is accessible through the API, specifically, contact objects associated with the subtype "email" where status is "Active."
#'
#' @param surveyid The survey's unique SG ID number (in V4 of the API, the portion of the \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-sub-object}{surveyresponse} call URL which follows "id/", e.g.: "...build/id/1234567"
#' @param api The user's private API key for Survey Gizmo
#' @param verbose When true (the default), download progress is printed to standard output.
#' \strong{NOTE:} When set to false, pullsg will change the stub of SG system variables to \emph{sys_*}
#' @param reset_row_names When true (the default), resets row names to 1, 2,..N in the returned dataframe
#' @param customfields When true (the default), SG's "custom fields" associated with the contact are preserved in the returned data.
#' @param contactinfo When true (the default), SG's phone and physical address fields associated with the contact are preserved in the returned data.
#' @param small When true, the only fields returned are contactid and email address.

#' @importFrom jsonlite fromJSON
#' @export


pullsg_campaign <- function(surveyid, api, reset_row_names=TRUE, customfields=TRUE, contactinfo=TRUE, small=FALSE, verbose=TRUE){

	options(stringsAsFactors=F)

	# Set hard-coded URL parameters
	token <- paste0('?api_token=', api) # Must be in the first trailing URL position
	url      <- 'https://api.alchemer-ca.com/v5/survey/'
	camp <- "/surveycampaign/"
	cont <- "/contact/"

	pages     <- "&page="
	results10  <- "&resultsperpage=10"
	results100  <- "&resultsperpage=100"

	# Build local parameters
	cam_base    <- paste0(url, surveyid, camp, token, results10)
	cam_ret     <- jsonlite::fromJSON(txt=cam_base) # get count of "surveyresponse" object
	cam_size    <- as.integer(cam_ret["total_count"])
	cam_respnum <- ceiling(cam_size/10)
	message("\n  Retrieving global campaign parameters:")
	progb       <- txtProgressBar(min = 0, max = length(cam_respnum), style = 3)

	# Retrieve all link/campaign ids using the "surveyresponse" call
	cam_call  <- paste0(url, surveyid, camp, token, results10, pages)
	for(i in 1:cam_respnum){
		sg_return_url  <- paste0(cam_call, i)
		cam_return_data <- jsonlite::fromJSON(txt=sg_return_url)
		cam_return_data <- as.data.frame(cam_return_data$data)
		assign(paste0("cam_returns", i), cam_return_data)
		setTxtProgressBar(progb, i)
		rm(cam_return_data)
	}
	close(progb)

	#Bind the frame returned by JSONlite
	cam_fullset <- do.call("rbind", mget(ls(pattern="cam_returns")))

	# The "contact" sub-object will only pull data where campaign subtype=email & status=Active
	cam_dat   <- cam_fullset[ , c("id", "inviteid", "_type", "_subtype", "status", "name", "datecreated", "uri")]
	email_ids <- cam_dat[cam_dat$`_subtype`=="email" & cam_dat$status=="Active", "id"]
	camp_name <- cam_dat[cam_dat$`_subtype`=="email" & cam_dat$status=="Active", "name"]

	if(length(email_ids)!=0) {

		message('\n  Retrieving campaign contact data:')
		progb <- txtProgressBar(min = 0, max = length(email_ids), style = 3)

		# Get contact data using the "surveyresponse/*/contact/" call
		for(i in 1:length(email_ids)){
			contact_call     <- paste0(url, surveyid, camp, email_ids[i], cont, token, results100, pages)
			contact_base     <- paste0(url, surveyid, camp, email_ids[i], cont, token, results10)
			contact_ids      <- jsonlite::fromJSON(txt=contact_base)
			contact_id_cnt   <- as.integer(contact_ids[['total_count']])
			contact_respnum  <- ceiling(contact_id_cnt/10)
			setTxtProgressBar(progb, i)

			# Retrieve all contact data for each campaign ("id" maps to "contactid" in the response object)
			for(j in 1:cam_respnum){
				sg_return_url  <- paste0(contact_call, j)
				contact_return_data <- jsonlite::fromJSON(txt=sg_return_url)
				contact_return_data <- as.data.frame(contact_return_data$data)
				assign(paste0("contact_returns_", i, "_", j), contact_return_data)
				rm(contact_return_data)
			}
		}
		close(progb)

	if(verbose) message('\nFinished downloading campaign data.')

	# Bind the contact frames returned by JSONlite
	contact_fullset <- do.call("rbind", mget(ls(pattern="contact_returns_")))
	names(contact_fullset)[names(contact_fullset)=="id"] <- "contactid"

	# Convert to POSIX date format
	contact_fullset$dlastsent <- as.POSIXct(contact_fullset$dlastsent)

	if(sum(duplicated(contact_fullset$contactid)))
		message("\nWarning: duplicated contactids found within the campaign data!\n",
				"--> Recommendation: Examine rows where 'contactid' is duplicated prior to merging with survey responses.")


		if(reset_row_names) rownames(contact_fullset) <- NULL
		if(customfields==FALSE) contact_fullset <- contact_fullset[ , -grep("^scustomfield", names(contact_fullset))]
		if(contactinfo==FALSE)  contact_fullset <- contact_fullset[ , -grep("phone$|^smailingaddress", names(contact_fullset))]
		if(small) contact_fullset <- contact_fullset[ , c("contactid", "semailaddress")]

		return(contact_fullset)

	if(verbose) message('\nCampaign data retrieval is complete!\n')

	} else {
		message('There is no email campaign(s) associated with survey "', surveyid, '", aborting campaign download!')
	}

}









