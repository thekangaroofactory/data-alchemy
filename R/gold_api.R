
#' Gold API Connector
#'
#' @param endpoint the name of the API endpoint.
#' @param symbol the asset symbol.
#' @param currency ISO 4217 alphabetic code for the currency.
#'
#' @returns a data.frame (or NULL).
#' @export
#'
#' @examples
#' \dontrun{
#' gold_api()
#' }

gold_api <- function(endpoint = "price", symbol = "XAU", currency = "USD"){
  
  # pattern:
  # https://api.gold-api.com/price/{symbol}/{?currency}
  
  base_url <- "https://api.gold-api.com"

  # -- build url
  url <- paste(base_url, endpoint, symbol, currency, sep = "/")
  
  # -- call API
  res <- iker::call_api(url = url, header = list())
  
  # -- check
  if(res$status_code != 200){
    warning("API response status_code = ", res$status_code)
    return(NULL)}
  
  # return
  as.data.frame(jsonlite::fromJSON(res$content))
  
}
