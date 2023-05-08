#' download files or folders to AWS
#'
#' Providing a key that is a folder and path that is a folder will result in
#' the whole folder being copied to the path location. If you supply "" to key,
#' the whole bucket will be downloaded.
#'
#' @param path String. Path to download location. Could be a folder or specific file(s)
#' @param bucket The name of the bucket to be downloaded from
#' @param key The key or name for the file or folder to be download from the bucket.
#'   Should end with "/" for folders. Use "" to download whole bucket.
#' @param check Whether to check if the exact file already exists in the download location
#'   and skip downloading. Defaults to TRUE
#' @param error Whether error out if the file is missing, folder is empty, or
#'   system environment variables are missing. Otherwise a message will print
#'   but an empty list will be returned.
#'
#' @return A list, each element  having the key, etag (hash), and path of downloaded
#'   files
#'
#' @export aws_s3_download
#'
aws_s3_download <- function(path, bucket, key,
                          check = TRUE, error = FALSE) {

  # check env vars
  if (any(Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION")) == "")) {
    msg <- paste(
      "AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY`, `AWS_REGION` environment
       variables must all be set to download to AWS, probably in the .env file"
    )
    if (error) {
      stop(msg)
    } else {
      warning(msg)
      return(list())
    }
  }

  # check the key provided

  key_check <- check_for_file_extension(key)

  # check if key is mix of files and folders
  # if NOT all files and any files

  check_for_mixed_path_types(check_vec = key_check,
                             param_checked = "key",
                             error = error)

  svc <- paws::s3()

  if(all(!key_check)){
    # expand folders to base files
    key <- svc$list_objects_v2(Bucket = Sys.getenv("AWS_BUCKET"), Prefix = key)$Contents |>
      purrr::map(~ .x$Key) |>
      unlist(FALSE)

    key_check <- check_for_file_extension(key)

    # re check for fold/file mixed keys
    check_for_mixed_path_types(check_vec = key_check, param_checked = "key",error = error)

  }


  # if there is a more than one file, mapply over the paths to make it
  # a single download

  if (length(key) > 1 & all(key_check)) {

    # if path is file names, should have same length as key; if dir, should
    # have length 1

    path_check <- check_for_file_extension(path)

    ## if you supplied a key for a folder and directory, copy structure
    ## from s3 to local folder
    if(length(path_check) == 1 & !all(path_check)){
      path <- sprintf("%s/%s",path,key)
      path_check <- check_for_file_extension(path)
    }


    stopifnot( (all(path_check) & length(path) == length(key)) |
                (!path_check & length(path) == 1))

    out <- mapply(aws_s3_download,
                  path = path,
                  key = key,
                  MoreArgs = list(bucket = bucket,
                                  check = check,
                                  error = error),
                  SIMPLIFY = FALSE)
    return(Reduce(c, unname(out), list()))

  }

  # single path workflow

  # if the file exists and the dir does not,

    out <- list(aws_s3_download_single(path, key, bucket, check, svc))

    return(out)
}

aws_s3_download_single <- function(path, key, bucket,
                                 check = TRUE, svc = paws::s3()) {

  # path and key should be length 1

  if(length(path) != 1){
    stop("path should be a vector of length 1")
  }

  if(length(key) != 1){
    stop("key should be a vector of length 1")
  }

  # check if the dir exists
  if(check_for_file_extension(path)){
    dir_name <- dirname(path)
  } else {
    dir_name <- path
  }

  if(!dir.exists(dir_name)){
    dir.create(dir_name,recursive = TRUE)
  }

  # check if path has a file name
  if(!check_for_file_extension(path)){
    # create file name from key
    path <- sprintf("%s/%s",path,basename(key))
  }

  if (check) {
    local_hash <- paste0('"', tools::md5sum(path), '"')
    s3_obj <- svc$list_objects_v2(Bucket = bucket, Prefix = key)$Contents |>
      purrr::keep(~ .x$Key == key) |>
      unlist(FALSE)

    if (!is.null(s3_obj) && s3_obj$ETag == local_hash) {

      return(list(key = s3_obj$Key, etag = s3_obj$ETag, path = path))
    }

    resp <- svc$download_file(
      Filename = path,
      Bucket = bucket,
      Key = key
    )
    return(list(key = key,path = path, etag = resp$ETag))
  }
}


check_for_file_extension <- function(path){
  file_name <- basename(path)
  check <- grepl(".+\\.[[:alnum:]]+$",file_name)
  return(check)
}

check_for_mixed_path_types <- function(check_vec,param_checked,error ){
  if(!all(check_vec) & any(check_vec)){
    msg <- sprintf("%s variable is a mix of files and folders in aws. Please
    provide files or folders. Error may also be triggered by unconventional
    folder or file naming conventions e.g. folder/sub.folder/file.ext or
                   folder/Bucket_test (file with no extension) ",
                   param_checked
    )
    if (error) {
      stop(msg)
    } else {
      warning(msg)
      return(list())
    }
  }
}
