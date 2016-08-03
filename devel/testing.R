Hi Derek,

It look like RSurveyGizmo gives duplicates. I make a small script to you to see the problem and attached the csv export for surveygizmo.

Sincerely,
Alex



# RSurveyGizmo Connection to IT Survey
api    <- "EB250A459E67401399E31FCC02189478" # define CPG’s private api key
surveyid <- 2748601

it_raw <- pullsg(2748601, key, mergecampaign=T, small=F)



issue <- duplicated(it_raw$responseid)
table(issue)

#Loading CSV of SurveyGizmo Export
gizmo <- read.csv(paste0(folder, "/testing_issue.csv"))
gizmo <- duplicated(gizmo$Response.ID)
table(gizmo)


camp$dlastsent

camp <- pullsg_campaign(2748601, key)

test <- camp[with(camp, order(contactid, -as.numeric(dlastsent))), ]

test1 <- camp[order(camp[,"contactid"], -as.numeric(camp[,"dlastsent"])), ]

all.equal(test,test1)
test1 <- test1[!duplicated(test1$contactid), ]

test <- camp[order(camp$contactid, decreasing=TRUE), ]

			 base =    z[order(z$mean),]

# There are duplicates on contact ID
resp <- pullsg(2748601, key, mergecampaign=F, small=F)

table(duplicated(resp$contactid))

# Response Data
test <- data.frame(dups=duplicated(resp$contactid), resp)

# Campaign Data
campt <- data.frame(dups=duplicated(camp$contactid), camp)

api    <- "EB250A459E67401399E31FCC02189478"
surveyid <- 2748601
verbose=TRUE


pullsg_campaign <- function(surveyid, api, reset_row_names=TRUE, customfields=TRUE, contactinfo=TRUE, small=FALSE, verbose=TRUE){

	options(stringsAsFactors=F)

	# Set hard-coded URL parameters
	token <- paste0('?api_token=', api) # Must be in the first trailing URL position
	url      <- 'https://restapi.surveygizmo.com/v4/survey/'
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
	progb       <- txtProgressBar(min = 0, max = length(cam_respnum), style = 3)
	message("Retrieving campaign parameters:\n")

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

		message('Retrieving contact data for ', length(email_ids), ' email campaigns:\n')

		# Get contact data using the "surveyresponse/*/contact/" call
		progb <- txtProgressBar(min = 0, max = length(email_ids), style = 3)

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

		if(verbose) message('Finished downloading campaign data.')

		# Bind the contact frames returned by JSONlite
		contact_fullset <- do.call("rbind", mget(ls(pattern="contact_returns_")))
		names(contact_fullset)[names(contact_fullset)=="id"] <- "contactid"

		# Convert to POSIX date format
		contact_fullset$dlastsent <- as.POSIXct(contact_fullset$dlastsent)

		if(sum(duplicated(contact_fullset$contactid))){
			message("Warning: duplicated contact ids found within the campaign data!\n",
					"We recommended that you examine the campaign data duplicates\n",
					"prior to merging with survey responses.")
			as.POSIXct(contact_fullset$dlastsent)
			contact_fullset <- contact_fullset[-order(contact_fullset$dlastsent), ]

		if(reset_row_names) rownames(contact_fullset) <- NULL
		if(customfields==FALSE) contact_fullset <- contact_fullset[ , -grep("^scustomfield", names(contact_fullset))]
		if(contactinfo==FALSE)  contact_fullset <- contact_fullset[ , -grep("phone$|^smailingaddress", names(contact_fullset))]
		if(small) contact_fullset <- contact_fullset[ , c("contactid", "semailaddress")]

		return(contact_fullset)

