#' Get html report files
#'
#' @param tar_obj a tar_render object or other target object that contains file paths
#' @param ... additional tar_render or other target objects
#' @param pattern string. regex pattern to match desired file paths
#'
#' @return a list of file paths
#' @export get_file_paths
#'
get_file_paths <- function(tar_obj,pattern,...){

  list_tars <- list(tar_obj,...)

  flat_reports <- unlist(list_tars, recursive = TRUE)

  files <- purrr::keep(flat_reports,function(x) {grepl(pattern,x)})

  if(length(files) == 0){
    stop("No files found")
  }

  return(files)
}
