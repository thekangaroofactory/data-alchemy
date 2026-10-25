
#' Gold API Connector
#'
#' @param endpoint the name of the API endpoint.
#' @param symbol the asset symbol.
#' @param currency ISO 4217 alphabetic code for the currency.
#' @param start a numeric value for the starting timestamp (ohlc).
#' @param end a numeric value for the ending timestamp (ohlc).
#' 
#' @details
#' The default behavior is to get the current price from the API.
#' 
#' When the `endpoint` is "price", then `start` and `end` are ignored.
#'
#' @returns a data.frame (or NULL).
#' @export
#'
#' @examples
#' \dontrun{
#' gold_api()
#' }

gold_api <- function(endpoint = c("price", "ohlc"), symbol = "XAU", currency = "USD", start = NULL, end = NULL){
  
  # -- check
  endpoint = match.arg(endpoint)
  if(endpoint == "ohlc"){
    stopifnot("start must be a numeric" = is.numeric(start))
    stopifnot("end must be a numeric" = is.numeric(end))}
  
  # -- build url
  base_url <- "https://api.gold-api.com"
  if(endpoint == "price"){
    
    header <- list()
    
    # pattern:
    # https://api.gold-api.com/price/{symbol}/{?currency}
    url <- paste(base_url, endpoint, symbol, currency, sep = "/")
  
  } else if(endpoint == "ohlc"){
    
    header <- list("x-api-key" = Sys.getenv("GOLD_API_KEY"))
    
    # pattern:
    # https://api.gold-api.com/ohlc/XAU?startTimestamp=1771053764&endTimestamp=1771485764
    query <- paste0("startTimestamp=", start, "&endTimestamp=", end)
    url <- paste(paste(base_url, endpoint, symbol, sep = "/"), query, sep = "?")}
  
  # -- call API
  res <- iker::call_api(url = url, header = header)
  
  # -- check
  if(res$status_code != 200){
    warning("API response status_code = ", res$status_code)
    return(NULL)}
  
  # return
  as.data.frame(jsonlite::fromJSON(res$content))
  
}
