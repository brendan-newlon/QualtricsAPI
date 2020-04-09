#' QualtricsAPI
#' 
#' @import tidyverse 
#' @import keyring
#' @import svDialogs
#' @import jsonlite
#' @import httr
#' 
#' 
#' 




#' QualtricsAPI
#'
#' @description
#' Use R to interact directly with Qualtrics via the API.
#'
#' @details
#' This package takes API endpoints as arguments to give complete flexibility in how you interact with the API, rather than providing more "R-like" functions and arguments.
#'
#' 
#' @param Request The part of the URL that follows after .../API/v3/
#' @param QualBaseUrl The part of the URL up to and including .../API/v3/. It's convenient to set this as a global variable.
#' @param Clear_Token Gives the option to delete saved credentials to make a correction or remove them from your system's key store.
#' @param Method Select from GET, POST, PUT, and DELETE.
#' @param Body_Data The data to send with POST and PUT calls.
#'
#' Body_Data should be formatted like this: 
#' 
#' Body_Data = '
#' "firstName" = "John",
#' "lastName" = "Doe",
#' "email" = "jdoe@email.com",
#' "unsubscribed" = FALSE
#' '
#'
#' @export
#'
#' @return The function returns a data.frame but also outputs a list named QualtricsAPI_Response_Raw which is helpful for debugging and may preserve returned data even if the main function stops with an error.
#'
#'
#' @examples
#' 
#' Eg. to retrieve data on all surveys in the account:
#' 
#' QualtricsAPI("surveys")
#' 
#' 
#' Eg. To create a new mailing list (using POST and sending key value pairs wrapped in single quotes):
#' 
#' QualtricsAPI(
#'   "mailinglists",
#'    Method = "POST",
#'    Body_Data = '
#'      "category" = "Clients",
#'      "libraryId" = "[enter your library id here]",
#'      "name" = "my_new_mailing_list"
#'               '
#'             )
#'                       
#' 
#'

##################################################### Main function: QualtricsAPI()
QualtricsAPI <- function(
  Request,       # The endpoint at the end of the API URL eg. "surveys" to call a list of surveys
  QualBaseUrl,          # Can be set as a global variable.  
  Qualtrics_Username,  # Can be set as a global variable. 
  Clear_Token = FALSE,
  Method = c("GET","POST", "PUT", "DELETE"),
  Body_Data = NULL
) {
  
  # Handle QualBaseUrl, Username, Token, and http Header
  handle_Qualtrics_vars(Request)
  
  # Default to Method = "GET"
  if(is.null(Method) || Method == "" || Method == c("GET","POST", "PUT", "DELETE")) {Method = "GET"}
  #_______________________________________________________________ Option to clear token from system or re-enter the token if changed
  Qualtrics_clear_token <- function() {
    if (Qualtrics_TokenName %in% key_list(Qualtrics_TokenName)$service) key_delete(Qualtrics_TokenName)
  }
  #_______________________________________________________________
  if (isTRUE(Clear_Token)) Qualtrics_clear_token()
  
  # Handle body data for POST and PUT requests
  if (!is.null(Body_Data)){
    body = Body_Data %>% gsub("\n    ","",.)
    body = paste0("body = list(",body,"), encode = 'json'") 
  }
  ##################################################################################!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Send the request____________________
  resp = QualtricsAPI_Request(url, body, Headers, Method, Body_Data)
  
  if (!is.null(resp$result$elements)) {
    # Get elements if they exist, or get result
    resp_prev = resp$result$elements     
    # %>% rectangle_json()
  } else {
    resp_prev = resp$result %>% rectangle_json()      
  }
  
  # If nextPage != null, keep calling
  while (!is.null(resp$result$nextPage)) {
    resp <- QualtricsAPI_Request(resp$result$nextPage, body, Headers, Method, Body_Data)
    
    # Get elements if they exist, or get result
    if (!is.null(resp$result$elements)) {
      resp_new = resp$result$elements 
      # %>% rectangle_json()
    } else {
      resp_new = resp$result %>% rectangle_json()
    }
    
    # Append results
    resp_prev = bind_rows(resp_prev, resp_new)
  }
  return(resp_prev)
} # End Function QualtricsAPI

#eg. 
# QualtricsAPI("surveys")


#____________________________________


#' ask_QualBaseUrl
#'
#' @export
#'

# Get QualBaseUrl if stored as a global variable to make the function simpler to use:
ask_QualBaseUrl = function(){
  if ("QualBaseUrl" %in% ls(envir = .GlobalEnv)) {
    QualBaseUrl = get("QualBaseUrl", envir = parent.frame()) # parent value
  } else  if ("QualBaseUrl" %in% ls(envir = .GlobalEnv)) {
    QualBaseUrl = get("QualBaseUrl", envir = .GlobalEnv) # get the value from Global env, not the local NULL value
  } else {
    QualBaseUrl <- svDialogs::dlg_input(message = "Enter Qualtrics Base URL: " )$res   
  }
  return(QualBaseUrl)
} # end function ask_QualBaseUrl

#______________________

#' ask_Qualrics_Username
#'
#' @export
#'

# Get Qualtrics_Username if stored as a global variable to make the function simpler to use:
ask_Qualtrics_Username = function(){
  if ("Qualtrics_Username" %in% ls(envir = parent.frame())) {
    Qualtrics_Username = get("Qualtrics_Username", envir = parent.frame()) # parent value
  } else  if ("Qualtrics_Username" %in% ls(envir = .GlobalEnv)) {
    Qualtrics_Username = get("Qualtrics_Username", envir = .GlobalEnv) # get the value from Global env, not the local NULL value
    
  } else {
    Qualtrics_Username <- svDialogs::dlg_input(message = "Enter Qualtrics Username: " )$res   
  }
  return(Qualtrics_Username)
} # end function ask_Qualtrics_Username

#########################################################

#' handle_Qualtrics_vars
#'
#' @export
#'

handle_Qualtrics_vars = function(Request) {
  # Ask for Base Url if not set in global environment
  if(!exists("QualBaseUrl") || is_empty(QualBaseUrl) || is.null(QualBaseUrl)){
    QualBaseUrl = ask_QualBaseUrl()
    if (!length(QualBaseUrl)) stop("Please try again with a Qualtrics Base URL")  # The user clicked the ’cancel’ button
  }
  # Combine to full api url
  url = paste0(QualBaseUrl, Request)
  
  # Ask for username if not set in global environment
  if(!exists("Qualtrics_Username") || is_empty(Qualtrics_Username) || is.null(Qualtrics_Username)){
    Qualtrics_Username = ask_Qualtrics_Username()
    if (!length(Qualtrics_Username)) stop("Please try again with a Qualtrics Username.")  # The user clicked the ’cancel’ button
  }
  Qualtrics_TokenName = paste0("`QualtricsToken_", Qualtrics_Username,"`")
  
  # More secure Token handling (as far as R is)
  if (Qualtrics_TokenName %in% key_list(Qualtrics_TokenName)$service) {
    QualToken = key_get(Qualtrics_TokenName)
  } else { # ask for the token and set it in user's system keystore for later use
    QualToken <- rstudioapi::askForSecret(paste0("Enter Qualtrics API token for ", Qualtrics_Username))
    key_set_with_value(Qualtrics_TokenName, password = QualToken)
  }
  Headers = paste0("add_headers('X-API-TOKEN' = '",QualToken,"')")
  
  # Report out the variables:
  assign("url", url, envir = parent.frame())
  assign("Headers", Headers, envir = parent.frame())
  assign("QualBaseUrl", QualBaseUrl, envir = parent.frame())
  assign("Qualtrics_Username", Qualtrics_Username, envir = parent.frame())
}

#############################################################################

#' QualtricsAPI_Request
#'
#' @export
#'

#___________________________________________________________inner workhorse function_______________ QualtricsAPI_Request()
QualtricsAPI_Request <- function(url, body, Headers, Method, Body_Data) {
  if(!is.null(Body_Data)){ # If there's Body_Data to send...
    request = parse(text= paste0(
      "httr::",Method,"('",url,"', ",body,", ",Headers,")"
    ))
  } else { 
    # If there's no Body_Data to send...
    request = parse(text= paste0(               
      "httr::",Method,"('",url,"', ",Headers,")"
    ))
  }
  
  resp = eval(request) %>% content("text") %>% fromJSON   
  if(!str_detect(resp[["meta"]][["httpStatus"]], "200")) {
    stop(paste("Request failed with code",resp$meta$httpStatus)) # API Error messages
  } else {cat("Request Successful. Status Code 200")}  
  assign("QualtricsAPI_Response_Raw", resp, envir = .GlobalEnv) # Handy for debugging & failsafe -- and relied upon in get_contacts functions...
  return(resp)
} # End Inner Function QualtricsAPI_Request



#______________ auto_unnest()
#' auto_unnest
#'
#' @export
#'

auto_unnest = function(your_df){
  
  # unnest_wider any list columns
  while("list" %in% lapply(your_df,class)){
    for(col in seq_along(your_df)){
      if(class(your_df[col][[1]]) == "list") {
        do_this = parse(text = paste0("your_df = your_df %>% unnest_wider(",names(your_df[col]),", names_sep = '.')") )
        eval(do_this)
      } # end if
    } # end for loop
    your_df = as.df(your_df)
  } #end while loop
  return(your_df)
} # end auto_unnest()



#_____________________________________________ rectangle_json() is the successor to super_flatten()

#' rectangle_json
#'
#' @export
#'

rectangle_json = function(your_list){
  your_df <- your_list %>%
    # make json, then make list
    toJSON() %>%
    fromJSON()    %>%
    # remove classification level
    purrr::flatten() %>%
    # turn nested lists into dataframes
    map_if(is_list, as_tibble)   %>%
    # bind_cols needs tibbles to be in lists
    map_if(is_tibble, list)    %>%
    # creates nested dataframe
    bind_cols()
  
  your_df = auto_unnest(your_df)
  
  return(your_df)
} # end function


#_____________________________________________________________________________
#' get_contacts
#'
#' @export
#' @description
#' Function to simplify getting contacts from a mailing list.
#'
#' 
#' @param mailing_list_id 
#' 
#' 
#' @return The function returns a data.frame

#################################################### get_contacts()
# Get contacts for a mailing list, and simplify the nested columns
get_contacts = function(mailing_list_id){
  x = QualtricsAPI(paste0("mailinglists/",mailing_list_id,"/contacts"), Method = "GET") 
  contacts = x %>% select(-embeddedData) 
  embedded = x$embeddedData %>% auto_unnest()
  contacts = bind_cols(contacts, embedded)
  return(contacts)
}
#################################################### aggregate_account_contacts()
#' aggregate_account_contacts
#'
#' @export
#'
#' @description
#' Function to aggregate contacts from all mailing lists in your account.
#'
#' 
#' @param name_pattern_regex By default, the function returns contacts from any mailing list with a name matching ".*" ie. all lists. Enter a regex string to match if you want to only aggregate contacts from certain lists.  
#' 
#' 
#' @return The function returns a data.frame
#' 

# Get an aggregated df of all contacts of a Qualtrics account where the mailing list name matches a pattern. 
aggregate_account_contacts = function(
  name_pattern_regex = ".*" # Default matches all lists
){
  
  # Get a list of all mailing lists -- Use this to find the id of the mailing list to update.
  mailing_lists = QualtricsAPI("mailinglists", Method = "GET")
  # matching_mailing_lists = QualtricsAPI("mailinglists", Method = "GET") %>% filter(str_detect(matching_mailing_lists$name, name_pattern_regex))
  matching_mailing_lists = mailing_lists %>% filter(str_detect(mailing_lists$name, name_pattern_regex))
  
  # Get contacts for every matching mailing list
  i = 1
  mailing_list_id = matching_mailing_lists$id[i]
  all_contacts = get_contacts(mailing_list_id) 
  # %>% auto_unnest()# get_contacts()
  for(i in 2:length(matching_mailing_lists$id)){
    mailing_list_id = matching_mailing_lists$id[i]
    new_contacts = get_contacts(mailing_list_id) 
    # %>% auto_unnest()# get_contacts()
    all_contacts = bind_rows(all_contacts, new_contacts) 
  }
  a = all_contacts %>% select(c(-responseHistory,-emailHistory))
  b = all_contacts %>% select(c(responseHistory,emailHistory)) %>% mutate_all(as.character)
  all_contacts = bind_cols(a,b)
  return(all_contacts)
}
