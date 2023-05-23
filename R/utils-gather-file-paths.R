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
  } else {
    message(files)
  }

  return(files)
}

#' Create git prefix for file paths
#'
#' Adds information about the git branch to the file path.
#'
#' @param branch String. Current branch of repo.
#' @param path String. Folder where file should be uploaded. If an empty string
#' is provided (default), the path argument will be ignored.
#' @param ignore_if_main Logical. No prefixed added to files on main branch.
#'
#' @return String. Prefix for the file path.
#' @export
#'
#' @examples
#'
#' # create_git_prefix(branch = gert::git_branch(),
#' # path= "example",
#' # ignore_if_main=TRUE)
#' # "refs/head/current/branch/example"
#'
create_git_prefix<- function(branch = gert::git_branch(),
                             path = "",
                             ignore_if_main=TRUE){
  if(ignore_if_main & (gert::git_branch() == "main")){
    git_prefix = ""
    return(git_prefix)
  }

  if(!is.character(path)){
    stop("path must be class character")
  }

  if(!is.character(branch)){
    stop("branch must be class character")
  }

  git_prefix <- sprintf("refs/heads/%s/", branch)

  if(path != ""){
    git_prefix <- sprintf("%s%s/",git_prefix,path)
  }
  # remove extra backslashes
  git_prefix <- gsub("/{2,}", "/", git_prefix)

  print(glue::glue("using git_prefix: {git_prefix}"))

  return(git_prefix)
}

