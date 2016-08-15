# Setup connection to MySQL event database
mysqlSetup <- function(user=NULL, pw=NULL, host="152.3.32.85") {
	if (is.null(user)) {
		user <- switch(Sys.info()["user"], ab428="ab428", andybega="ab428", 
			mw160="mw160", max.gallop = "maxg")
	}

	# check for pw
  	if (is.null(pw)) { stop("Fill in password") }
  
  # Try MySQL
  library(RMySQL)
  tryCatch(conn <<- dbConnect(MySQL(), user=user, password=pw, 
  	dbname="event_data", host=host), 
    error=function(e) warning("MySQL connection does not work")
    )
}
