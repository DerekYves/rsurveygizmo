#' Pull the raw survey response data from Survey Gizmo, storing an unmodified dataframe.
#'
#' This function downloads a survey's response data from Survey Gizmo (SG), saving
#' the returned data as a \code{\link{data.frame}}. Because SG limits the size of JSON data pulls via the API (currently, the limit is 250),
#' it calculates the number of pulls needed to download the entire response set and binds the returned frames.
#' To ensure that variable names are interpretable in the returned data frame, it is strongly recommended
#' that users first assign question \href{https://help.surveygizmo.com/help/article/link/using-question-aliases}{aliases} to each question prior to utilizing this function.
#'
#' @param sg_surveyid The survey's unique SG ID number (in V4 of the API, the portion of the \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-sub-object}{surveyresponse}call URL which follows "id/", e.g.: "...build/id/1234567"
#' @param api The user's private API key for Survey Gizmo
#' @param completes Should the download drop partial responses (Default)?
#' @param delete_sys_vars Delete all SG system variables (i.e., non-response fields of the form [[variable(...)]] from the returned data.frame (see \href{https://apihelp.surveygizmo.com/help/article/link/surveyresponse-returned-fields}{this link} for documentation)?
#' @param reset_row_names Reset row.names to 1, 2,..N in the returned dataframe (Default)?
#' @param clean This option performs three transformations to the returned data.frame:
#' \enumerate{
#'   \item attempts to coerce vectors to numeric if all values are numbers or "" (uses \code{\link{type.convert}})
#'   \item deletes SG system variables (i.e., non-question responses)
#'   \item removes other non-survey-question variables returned by the Survey Gizmo API, including: "contactid", "istestdata", "sessionid", "language", "ilinkid", and "sresponsecomment" (as of V4 of the SG API)
#' }
#' @importFrom jsonlite fromJSON
#' @export
pullsg <- function(sg_surveyid, api, completes=T, delete_sys_vars=F, clean=F, reset_row_names=T) {
	options(stringsAsFactors=F)

	#Set hard-coded parameters
	token <- paste0('?api_token=', api) #Must be in the URL's first position
	url      <- 'https://restapi.surveygizmo.com/v4/survey/'
	response <- "/surveyresponse/"
	question <- "/surveyquestion/"
	pages    <- "&page="
	results  <- "&resultsperpage=100"

	#Build local parameters
	surveyid <- sg_surveyid
	filturl  <- paste0("&filter[field][0]=status&filter[operator][0]==",
					   "&filter[value][0]=Complete")
	if (completes==T) filt=filturl
	else filt <- ""

	lc_base  <- paste0(url, surveyid, response, token, filt)
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

	# Recode question names that do not have an alias defined
	lc_qs$shortname   <- ifelse(is.na(lc_qs$shortname),
								lc_qs$title$English, lc_qs$shortname)

	# Subset the frame, keeping shortname and id
	lc_qs <- lc_qs[, c('id', 'shortname')]

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
	lc_tobind  <- ls(pattern="lc_survey_page")
	lc_fullset <- do.call("rbind", mget(lc_tobind))

	# Process the column names
	lc_names <- names(lc_fullset)

	# Defined patterns to drop
	patterns = "[[:punct:]]|[[:space:]]|question|shown|STANDARD"

	# Hidden values generally take form of: 'question(76), option(0)'
	# If hidden value assigned to more than one ET field, then
	# ...option(1)... option(N)... format is used. Password actions
	# also have a qid with two option fields with five digit numbers
	# Here, we extract the "option 0" parameter from questions so
	# they will merge correctly with the array of names.
	lc_names <- lapply(lc_names, gsub, patt= ", option(0)",
					   rep="", fixed=T)

	# Now remove the patterns saved above.
	lc_names <- lapply(lc_names, gsub, patt= patterns,
					   rep="")
	# Now rename SG system fields, subtituting "sys_" for "variable"
	lc_names <- lapply(lc_names, gsub, patt= "variable",
					   rep="sys_")
	# Now lower case the names
	lc_names  <- as.data.frame(tolower(lc_names))

	colnames(lc_names) <- "id"

	# Index to preserve inital sort of question names
	lc_names$index <- seq_len(nrow(lc_names))

	# Left join of the cleaned name table
	lc_names <- merge(lc_names, lc_qs, all.x=T)
	lc_names <- lc_names[order(lc_names$index),]
	rownames(lc_names) <- NULL # reset the row.names attribute

	# Recode hidden values, which do not accept shortnames/aliases in SG
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

	# Other variables to drop per the "clean" parameter (V3 API)
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




