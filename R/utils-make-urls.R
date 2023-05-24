#' Make urls for EHA secure
#'
#' Create urls for items stored on EHA secure sites
#'
#'@param url_domain String. Resource name on eha secure
#'@param remote_path String. What is the path to the resource on remote site. For
#' AWS this will be the item key.
#'
#' @return string. URL for resource
#' @export
#'
#' @examples
#'
#'
#'
make_eha_secure_url <- function(url_domain = Sys.getenv("URL_PREFIX"),
                                remote_path
                                ){
  out_url <- sprintf("%s/%s",url_domain,remote_path)

  return(out_url)
}
