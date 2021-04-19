#' @title Access UCDP GED
#' @name ucdp.ged
#' @description Access and manage the application programming
#' interface (API) of the [Uppsala Conflict Data Program (UCDP)](https://www.ucdp.uu.se/) to retrieve 
#' geo-coded event data (GED).
#' The function _`ucdp.ged()`_ makes it easy to retrieve a user-defined sample (or all of the
#' available data) of UCDP GED. 
#' @param country character vector. Supply one or more country names to narrow down which events should be retrieved. See the details
#' below for information on how the arguments "country" and "region" interact.
#' @details If very large data call, it may take a long time and progress bar will appear that shows progress by which data is downloaded.
#' @export
#'
acled.api <- function(
  email.address = Sys.getenv("EMAIL_ADDRESS"),
  access.key = Sys.getenv("ACCESS_KEY"),
  country = NULL,
  region = NULL,
  start.date = NULL,
  end.date = NULL,
  add.variables = NULL,
  all.variables = FALSE,
  dyadic = FALSE,
  interaction = NULL,
  other.query = NULL){


  # access key
  if ( (is.null(access.key) | !is.character(access.key) | access.key=="") == TRUE ) {
    stop('ACLED requires an access key, which needs to be supplied to the argument "access.key" as a character string.
    You can request an access key by registering on https://developer.acleddata.com/.', call. = FALSE)
  }
  if( (is.character(access.key) & access.key!="") == TRUE){
  access.key1 <- paste0("key=", access.key)}

  # email address
  if ( (is.null(email.address) | !is.character(email.address) | email.address=="") == TRUE ) {
    stop('ACLED requires an email address for access, which needs to be supplied to the argument "email.address" as a character string.
    Use the email address you provided when registering on https://developer.acleddata.com/.', call. = FALSE)
  }
  if( (is.character(email.address) & email.address!="") == TRUE){
  email.address1 <- paste0("&email=", email.address)}


  # country argument
  if ( (!is.null(country) & !is.character(country)) == TRUE ) {
    stop('If you wish to specify country names, these need to be supplied as a character vector.
    Usage example: \n
         acled.api(country = c("Kenya", "Togo"), start.date = "2004-08-20", end.date = "2005-05-15")', call. = FALSE)
  }
  if(is.character(country) == TRUE){
    country1 <- paste0("&country=",
                       paste( gsub("\\s{1}", "%20", country), collapse = "|")) }
  if(is.null(country) == TRUE){
    country1 <- ""
  }

  # region argument
  if ( (!is.null(region) & !is.numeric(region) & !is.character(region)) == TRUE ) {
    stop('If you wish to specify regions, these need to be supplied as a numeric vector (region codes) or character
    vector (region names). See the ACLED API guide for exact region names and codes.
    Usage example: \n
         acled.api(region = c(1,2), start.date = "2004-08-20", end.date = "2005-05-15") or \n
         acled.api(region = c("Western Africa", "Middle Africa"), start.date = "2004-08-20", end.date = "2005-05-15")', call. = FALSE)
  }
  if(is.numeric(region) == TRUE){
    region1 <- paste0("&region=", paste(region, collapse = "|") )
  }
  region.data.frame <- get.api.regions()[[1]]
  if(is.character(region) == TRUE){
    char.region <- region.data.frame$code[which(region.data.frame$region%in%region)]
    region1 <- paste0("&region=", paste(char.region, collapse = "|") )
        if(length(region) != length(char.region)){
          warning('At least one of the region names supplied in argument "region = " does not match the original
              ACLED region names. Check your spelling, or see the ACLED API Guide for the correct names.', call. = FALSE)
        }
  }
  if(is.null(region) == TRUE){
    region1 <- ""
  }

  # start.date & end.date arguments
  if ( (is.null(start.date) & is.null(end.date)) == FALSE ){
    if( (is.null(start.date) | is.null(end.date)) == TRUE){
    stop("You need to supply either no start date and no end date, in which case all available dates are requested, or both
    a start date and an end date.
    Usage example: \n
         acled.api(region = c(1), start.date = '1995-01-15', end.date = '2005-12-15') or
         acled.api(region = c(1), start.date = NULL, end.date = NULL)", call. = FALSE)
    }
    if ( start.date>end.date ) {
      stop("The start date cannot be larger than the end date.", call. = FALSE)
    }
  }
  time.frame1 <- paste0("&event_date=", paste(start.date, end.date, sep = "|"), "&event_date_where=BETWEEN")

  # add.variables and all.variables argument
  if( is.logical(all.variables)==TRUE ){
    if( all.variables==FALSE ){
        if( is.null(add.variables)==TRUE ){
          variables <- "&fields=region|country|year|event_date|source|admin1|admin2|admin3|location|event_type|sub_event_type|interaction|fatalities|timestamp"
          }else{
            variables <- paste0("&fields=region|country|year|event_date|source|admin1|admin2|admin3|location|event_type|sub_event_type|interaction|fatalities|timestamp",
                        "|", paste(add.variables, collapse = "|") )
            }
      }else{
          variables <- ""
          }
    }else{
      stop("The argument 'all.variables' requires a logical value.", call. = FALSE)
  }

  # dyadic argument
  if( is.logical(dyadic)==TRUE ){
      dyadic1 <- ifelse(dyadic==FALSE, "&?export_type=monadic", "")}else{
        stop("The argument 'dyadic' requires a logical value.", call. = FALSE)
  }

  # interaction argument
  if (!(is.numeric(interaction) | is.null(interaction))) {
    stop("The 'interaction' argument requires a numeric value.")
  } else if (!all(interaction %in% c(10:18, 20, 22:28, 30, 33:38, 40, 44:48, 50, 55:58, 60, 66, 68, 78, 80))) {
    stop(paste0("At least one of the interaction codes supplied to the argument ",
                "'interaction' does not match the original ACLED interaction codes.\n",
                "Check the ACLED codebook for the correct codes."))
  }
  interaction1 <- ifelse(is.null(interaction)==TRUE, "",
                         paste0("&", paste0("interaction=", interaction, collapse = ":OR:")))

  # other.query argument
  other.query1 <- ifelse( is.null(other.query)==TRUE, "", paste0("&", paste(other.query, collapse = "&")) )


  # ACLED ping
  acled.url.ping <- tryCatch(
    httr::GET("https://api.acleddata.com/"),
    error = function(e) e )
  if( any(class(acled.url.ping) == "error") ) {
    message("The resource api.acleddata.com cannot be reached. \n
    1. Please check your internet connection.
    2. If the internet connection is reliable, the server may be temporarily unavailable; in this case please try again later.
    3. If the problem persists, please contact the package maintainer as the resource may have moved.")
    return(NULL)
  }

  
  
  
  country <- countrycode("India", origin = "country.name", destination = "gwn", warn = TRUE)
  version <- "20.1"
  
  base.url <- paste0("https://ucdpapi.pcr.uu.se/api/gedevents/", version, "?pagesize=1000&page=")
  filter.url <- paste0("&Country=", country, "&StartDate=2000-01-01&EndDate=2007-10-12")
  
  
  # GET call
  url <- paste0(base.url, 0, filter.url)
  response <- httr::GET(url)
  json.content <- jsonlite::fromJSON( httr::content(response, "text", encoding = 'UTF-8'),
                                      simplifyVector = FALSE)
  json.data <- json.content$Result
  ucdp.ged.matrix <- matrix( unlist(json.data),
                             byrow = T,
                             nrow = length(json.data) )
  ucdp.ged.data <- data.frame(ucdp.ged.matrix, stringsAsFactors = FALSE)
  names(ucdp.ged.data) <- names(json.content$Result[[1L]])
  ucdp.ged.data
  
  if(json.content$TotalPages>1){
    if(json.content$TotalPages>10){ pb <- txtProgressBar(min = 1, max = (json.content$TotalPages-1), style = 3) } 
  for(i in 1:(json.content$TotalPages-1)){
    url <- paste0(base.url, i, filter.url)
    response <- httr::GET(url)
    json.content <- jsonlite::fromJSON( httr::content(response, "text", encoding = 'UTF-8'),
                                        simplifyVector = FALSE)
    json.data <- json.content$Result
    ucdp.ged.matrix <- matrix( unlist(json.data),
                               byrow = T,
                               nrow = length(json.data) )
    ucdp.ged.data.bind <- data.frame(ucdp.ged.matrix, stringsAsFactors = FALSE)
    names(ucdp.ged.data.bind) <- names(json.content$Result[[1L]])
    ucdp.ged.data <- rbind(ucdp.ged.data, ucdp.ged.data.bind)
    if(json.content$TotalPages>10){ setTxtProgressBar(pb,i) } 
  } 
  # possibly use close here for progress bar: if(json.content$TotalPages>10){ close(pb) }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  # GET call
  url <- paste0("https://ucdpapi.pcr.uu.se/api/<resource>/<version>?<pagesize=x>&<page=x>&country_id=",
                access.key1, email.address1, "&limit=0", dyadic1, time.frame1, variables, country1, region1, interaction1, other.query1)

  response <- httr::GET(url)
  if ( exists("response")==FALSE ) {
    message("GET request was unsuccessful. Please check your internet connection. If the problem persists despite a reliable internet connection,
    the server may be temporarily not available; in this case try again later.",
    call. = FALSE)
    return(NULL)
  }
  if (httr::http_type(response) != "application/json") {
    message(paste0("GET request was unsuccessful: the API did not return a JSON file, giving the status code ",
                response$status_code, "."), call. = FALSE)
    return(NULL)
  }

  # JSON
  json.content <- jsonlite::fromJSON( httr::content(response, "text", encoding = 'UTF-8'),
                            simplifyVector = FALSE)
  if(!json.content$success){
    message(paste0("GET request wasn't successful. The API returned status ", json.content$error$status, ": ", json.content$error$message, "."))
    return(NULL)
  }
  json.content <- json.content$data

  if( length(json.content)==0L ){
    message("No data found for this area and time period.
            Or did you supply both countries and (other) regions? These cannot be combined.")
    return(NULL)
  }

  # data prep
  acled.matrix <- matrix( unlist(json.content),
                          byrow = T,
                          nrow = length(json.content) )

  acled.data <- data.frame(acled.matrix, stringsAsFactors = FALSE)
  names(acled.data) <- names(json.content[[1L]])

  for(i in 1:ncol(acled.data)){
    if( all(grepl("^[0-9]+$", acled.data[,i], perl = T))==TRUE ){
      acled.data[,i] <- as.numeric(acled.data[,i])}
  }

  message(paste0("Your ACLED data request was successful. \nEvents were retrieved for the period starting ",
                    range(acled.data$event_date)[1], " until ", range(acled.data$event_date)[2], "."))



  # return the final data frame
  acled.data

}
