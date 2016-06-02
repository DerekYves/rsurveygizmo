#' Download survey campaign data from Survey Gizmo, storing an R dataframe.
#'
#' This function downloads campaign data from Survey Gizmo (SG), saving
#' the returned data as an R \code{\link{data.frame}}. Because SG limits the size of JSON data pulls via the API (currently, the limit is 250),
#' it calculates the number of pulls needed to download the entire response set and binds the returned frames.
#'
#' @param surveyid The survey's unique SG ID number (in V4 of the API, the portion of the \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-sub-object}{surveyresponse} call URL which follows "id/", e.g.: "...build/id/1234567"
#' @param api The user's private API key for Survey Gizmo
#' @param delete_sys_vars When true, deletes SG system variables (i.e., fields
#' of the form [[variable(...)]] that do \emph{not} contain question responses.
#' \strong{NOTE:} When set to false, pullsg will change the stub of SG system variables to \emph{sys_*}
#' See \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-returned-fields}{this link}
#' for more information about fields returned by SG via the API).
#' @param reset_row_names When true (the default), resets row names to 1, 2,..N in the returned dataframe
#' @param clean This option performs three transformations to the returned data.frame:
#' \enumerate{
#'   \item attempts to coerce vectors to numeric if all values are numbers or "" (uses \code{\link{type.convert}})
#'   \item sets 'delete_sys_vars' to true
#'   \item removes other non-survey-question variables returned by the Survey Gizmo API, including: "contactid", "istestdata", "sessionid", "language", "ilinkid", and "sresponsecomment" (as of V4 of the SG API)
#' }
#' @importFrom jsonlite fromJSON
#' @export
pullsg_campaign <- function(surveyid, api, delete_sys_vars=F, clean=F, reset_row_names=T) {

	options(stringsAsFactors=F)

	#Set hard-coded parameters
	token <- paste0('?api_token=', "myapikey") #Must be in the URL's first position
	url      <- 'https://restapi.surveygizmo.com/v4/survey/'
	response <- "/surveyresponse/"
	question <- "/surveyquestion/"
	pages    <- "&page="
	results  <- "&resultsperpage=100"

	#Build local parameters
	filturl  <- paste0("&filter[field][0]=status&filter[operator][0]==",
					   "&filter[value][0]=Complete")
	if (completes_only == T) {
		filt = filturl
	} else {
		filt <- ""
	}

	lc_base  <- paste0(url, "surveyid", response, token, "filt")
	lc_furl  <- paste0(url, surveyid, response, token, results, filt, pages)
	lc_qurl  <- paste0(url, surveyid, question, token)

	# Get base response parameters of the survey and extract N
	lc_base     <- fromJSON(txt=lc_base)
	lc_samp_sz  <- as.integer(lc_base[['total_count']])

	# Calculate page number (starting with 1) based on 100 responses per call
	lc_respnum  <- ceiling(lc_samp_sz/100)

	# Retrieve the question list from the "/surveyquestion/" call
	lc_qs   <- fromJSON(txt=lc_qurl)

	lc_qs   <- as.data.frame(lc_qs$data)

	# Extract question text for questions without a defined SG alias, stripping
	# html tags, punctuation, and spaces,  keeping the first 35 characters and
	# appending the question id.
	lc_qs$qtext <- gsub("<.*?>", "", lc_qs$title$English)
	lc_qs$qtext <- gsub("[[:punct:]]", "", lc_qs$qtext)
	lc_qs$qtext <- paste0(substr(gsub("[[:space:]]", ".", lc_qs$qtext), 1, 35),
						  "_ID", lc_qs$id)
	lc_qs$shortname <- ifelse(is.na(lc_qs$shortname) | lc_qs$shortname=="",
							  lc_qs$qtext, lc_qs$shortname)

	# Drop instructional messages and subset the frame, keeping shortname and id.
	lc_qs <- lc_qs[lc_qs$`_type` !="SurveyDecorative", c('id', 'shortname')]

	# Retrieve the response data with the "/surveyresponse/" call
	for(i in 1:lc_respnum){
		sg_return_url  <- paste0(lc_furl, i)
		#sg_return_name <- paste0("lc_survey_page", i)
		message("Retrieving page ", i)
		sg_return_data <- fromJSON(txt=sg_return_url)
		sg_return_data <- as.data.frame(sg_return_data$data)
		assign(paste0("lc_survey_page", i), sg_return_data)
	}

	#Bind the returned frames
	lc_fullset <- do.call("rbind", mget(ls(pattern="lc_survey_page")))

	# Extract the column names of the responsedata returned from SG
	lc_names <- names(lc_fullset)

	# Define patterns to drop from the vector of column names
	patterns = "[[:punct:]]|[[:space:]]|question|shown|STANDARD"

	# Hidden values generally take form of: 'question(76), option(0)'
	# If a hidden value is assigned to more than one field (e.g., in
	# an Exact Target Pull action, then an '...option(1)... option(N)...'
	# format is used. SG password actions also have a qid with two option
	# fields. Here, we extract the "option 0" parameter from the questions
	# so they will merge correctly with the array of names.
	lc_names <- lapply(lc_names, gsub, patt= ", option(0)",
					   rep="", fixed=T)

	# Now remove the patterns saved above.
	lc_names <- lapply(lc_names, gsub, patt= patterns, rep="")

	# Save geographiv variables based on respondent IP
	if(keep_geo_vars) {
		lc_names <- lapply(lc_names, gsub, patt="^variableLONG$",       rep="rsp_lng")
		lc_names <- lapply(lc_names, gsub, patt="^variableLAT$",        rep="rsp_lat")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOCOUNTRY$", rep="rsp_cntry")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOCITY$",    rep="rsp_city")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOREGION$",  rep="rsp_region")
		lc_names <- lapply(lc_names, gsub, patt="^variableGEOPOSTAL$",  rep="rsp_post")
	}


	# Now rename SG system fields, subtituting "sys_" for "variable"
	lc_names <- lapply(lc_names, gsub, patt= "variable",
					   rep="sys_")

	# Now lower case the names and convert to data.frame
	lc_names <- data.frame(id=tolower(lc_names))

	# Index to preserve the inital column sorting
	lc_names$index <- seq_len(nrow(lc_names))

	# Left join of the cleaned name table
	lc_names <- merge(lc_names, lc_qs, by="id", all.x=T)
	lc_names <- lc_names[order(lc_names$index),]
	rownames(lc_names) <- NULL # reset the row.names attribute

	# Replace variable name with shortname wher available
	lc_names$id   <- ifelse(is.na(lc_names$shortname),
							lc_names$id, lc_names$shortname)

	# Subset the vector of cleaned names and replace the full_set names
	lc_names <- lc_names[,'id']
	colnames(lc_fullset) <- lc_names

	# Removes 'sys_*' vars that end in a digit
	# Removes residual number-only vars as they will falsely align in rbind
	drops <- "^[[:digit:]]+$|sys_[[:digit:]]+"
	set <- lc_fullset[, -grep(drops, names(lc_fullset), perl=T)]

	if(delete_sys_vars | clean) set <- set[, -grep("^sys_", names(set),
												   ignore.case=T)]

	# Other variables to drop per the "clean" parameter (V4 API)
	otherdrops <- paste0("contactid|istestdata|sessionid|",
						 "language|ilinkid|sresponsecomment")

	if(clean) set <- set[, -grep(otherdrops, names(set), ignore.case=T)]
	if(clean) set <- as.data.frame(lapply(set, type.convert, na.strings = "", as.is=T))
	if(reset_row_names) row.names(set) <- NULL

	# Format Survey Gizmo date fields
	set[, c('datestarted', 'datesubmitted')] <- lapply(set[, c('datestarted', 'datesubmitted')],
													   as.POSIXct, format="%Y-%m-%d")

	return(set)
}




