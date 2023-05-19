#' Upload files or folders to AWS
#'
#'  When uploading folders, the subdirectory structure will be preserved. To
#'  upload files from a folder without preserving the directory structure,
#'  pass a vector of file paths to the path arugment.
#'
#'
#' @param path String. The path to the file(s) or folder(s) to be uploaded
#' @param bucket String. The name of the bucket to be uploaded to
#' @param key String. The key or name for the file or folder to take in the bucket.
#'   Should end with "/" for folders. Use "" (empty string) to upload files in folder without
#'   top-level folder.
#' @param prefix String. A prefix to prepend to the file or folder keys. Generally
#'   should end with "/"
#' @param check Logical. Whether to check if the exact file already exists in the bucket
#'   and skip uploading. Defaults to TRUE
#' @param error Logical. Whether error out if the file is missing, folder is empty, or
#'   system environment variables are missing. Otherwise a message will print
#'   but an empty list will be returned.
#' @param file_type String. Provide a file type from [mime::mimemap()] (e.g. "html","csv")
#'  or provide "guess"to call [mime::guess_type()].
#'
#' @return A list, each element being having the key and etag (hash) of uploaded
#'   files
#'
#' @export aws_s3_upload
#'
aws_s3_upload <- function(path, bucket, key = basename(path), prefix = "",
                          check = TRUE, error = FALSE, file_type = "guess") {

  if (any(Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION")) == "")) {
    msg <- paste(
      "AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY`, `AWS_REGION` environment
       variables must all be set to upload to AWS, probably in the .env file"
    )
    if (error) {
      stop(msg)
    } else {
      warning(msg)
      return(list())
    }
  }

  # if there is a more than one file, mapply over the paths to make it
  # a single upload
  if (length(path) > 1) {
    stopifnot(length(path) == length(key))
    out <- mapply(aws_s3_upload,
                  path = path,
                  key = key,
                  MoreArgs = list(bucket = bucket,
                                  prefix = prefix,
                                  check = check,
                                  error = error,
                                  file_type = file_type),
                  SIMPLIFY = FALSE)
    return(Reduce(c, unname(out), list()))

  }


  # Single path (directory or file) workflow

  file_check <- utils::file_test(op = "-f",path)
  dir_check <- utils::file_test(op = "-d",path)

  # if neither the file nor the directory exist, break or warn
  if (!file_check & !dir_check) {
    if (error) {
      err_msg <- glue::glue("Neither File nor Directory not found. Argument supplied
                            to path does not appear to exist. {path}")
      stop(err_msg)
    } else {
      msg <- glue::glue("Neither File nor Directory not found.  Argument supplied
                            to path does not appear to exist. {path}

                            Returning an empty list.")
      message(msg)
      return(list())
    }
  }



  # single path workflow
  svc <- paws::s3()

  # if path is a single file
  if (file_check) {
    out <- list(aws_s3_upload_single(path = path,
                                     key =  paste0(prefix, key),
                                     bucket =  bucket,
                                     check =  check,
                                     svc =  svc,
                                     file_type =  file_type))
  }
  # if path is a directory
  if(dir_check){
    files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)

    if (!length(files)) {
      if (error) {
        stop("Directory empty.")
      } else {
        message("Directory empty. No upload, returning empty list")
        return(list())
      }
    }

    # Create prefixed records (archive or branches)
    #keys <- paste0(prefix, gsub(paste0("^", basename(path)), key, files))

    file_paths <- gsub(paste0("^", basename(path)), "", files)

    keys <- sprintf("%s/%s/%s",prefix,key,file_paths)
    keys <- gsub("/{2,}", "/", keys) ## correcting multiple slashes in key

    out <- mapply(aws_s3_upload_single,
                  path = files,
                  key = keys,
                  MoreArgs = list(bucket = bucket,
                                  check = check,
                                  svc = svc,
                                  file_type = file_type),
                  SIMPLIFY = FALSE
    ) |>
      unname()
  }

  out
}

aws_s3_upload_single <- function(path, key = basename(path), bucket,
                                 check = TRUE, svc = paws::s3(), file_type = "guess") {
  if (check) {
    local_hash <- paste0('"', tools::md5sum(path), '"')
    s3_obj <- svc$list_objects_v2(Bucket = bucket, Prefix = key)$Contents |>
      purrr::keep(~ .x$Key == key) |>
      unlist(FALSE)

    if (!is.null(s3_obj) && s3_obj$ETag == local_hash) {
      return(list(key = s3_obj$Key, etag = s3_obj$ETag))
    }

    if(file_type == "guess"){
      content_type <- mime::guess_type(path)
    } else {
      content_type <- mime::mimemap[file_type]
    }

    resp <- svc$put_object(
      Body = path,
      Bucket = bucket,
      Key = key,
      ContentType = content_type
    )
    return(list(key = key, etag = resp$ETag, content_type = content_type))
  }
}
