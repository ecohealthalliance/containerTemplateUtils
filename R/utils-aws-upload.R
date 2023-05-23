#' Upload files or folders to AWS
#'
#'  When uploading folders, the subdirectory structure will be preserved. To
#'  upload files from a folder without preserving the directory structure,
#'  pass a vector of file paths to the path argument.
#'
#'  If you would like the change the directory structure, pass in a vector of
#'  file paths and a corresponding vector of keys.
#'
#'
#' @param path String. The path to the file(s) or folder(s) to be uploaded
#' @param bucket String. The name of the bucket to be uploaded to
#' @param key String. The "path" of the file(s) or folder(s) in the AWS bucket.
#'   Should end with "/" for folders. Use "" (an empty string) to upload files in folder without
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
#' @examples
#' \dontrun{
#'
#' # Upload a single file to a specific location in the bucket.
#' # this will take the readme.md file and place in the exact location
#' # specified by the key and prefix. Notice the key ends in a file
#' # extension.
#'
#' containerTemplateUtils::aws_s3_upload(path = "README.md",
#' key = "test/key/param/readme.md",
#' error = TRUE,
#' bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # A vector of paths with a matching vector of keys
#' # will also result in exact placement.
#'
#' paths <- list.files("R",full.names = TRUE )
#'
#' file_names <- basename(paths)
#'
#' keys <- sprintf("%s/%s","example_dir",file_names)
#'
#' containerTemplateUtils::aws_s3_upload(path = paths,
#' key = keys,
#' error = TRUE,
#' bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws keys will be "example_dir/<file_name>"
#'
#' # Supplying a single file path and key with no file extension will
#' # result in the key being treated as a directory and the file being placed
#' # in that directory.
#'
#' containerTemplateUtils::aws_s3_upload(path = "R/utils-aws-upload.R",
#'                                       key = "test/key/param",
#'                                       error = TRUE,
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws key will be "test/key/param/utils-aws-upload.R"
#'
#' # Supplying a single file path and no key argument will result in the file
#' # being uploaded to the top level directory of the bucket.
#'
#' containerTemplateUtils::aws_s3_upload(path = "R/utils-aws-upload.R",
#'                                       error = TRUE,
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws key will be "./utils-aws-upload.R"
#'
#' # If the path argument is a folder, the key argument should also be a folder.
#' # Files from the folder will be uploaded into that directory.
#'
#' containerTemplateUtils::aws_s3_upload(path = "R/",
#'                                       key = "test/upload_folder/",
#'                                       error = TRUE,
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws keys will be "test/upload_nested_folder/<files from R/>"
#'
#' # If the path argument is a folder with sub-directories, the structure of
#' # the sub-directories will be preserved.
#'
#' dir.create("example_with_sub_dirs")
#' dir.create("example_with_sub_dirs/sub_dir")
#' file.create("example_with_sub_dirs/sub_dir/test.txt")
#'
#' containerTemplateUtils::aws_s3_upload(path = "example_with_sub_dirs/",
#'                                       key = "test/upload_nested_folder/",
#'                                       error = TRUE,
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws key will be "test/upload_nested_folder/example_with_sub_dirs/sub_dir/test.txt"
#'
#' # If the path argument is a folder and no key argument is supplied,
#' # the local directory structure will be copied to the S3 bucket.
#'
#' containerTemplateUtils::aws_s3_upload(path = "example_with_sub_dirs/",
#'                                       error = TRUE,
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws keys will be "R/<files from R/>"
#'
#' # If the path argument is a folder and key is an empty string, then only
#' # the files from the folder will be uploaded.
#'
#' containerTemplateUtils::aws_s3_upload(path = "R/",
#'                                       key = "",
#'                                       error = TRUE,
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws keys will be "./<files from R/>"
#'
#' # The prefix argument can be used to add a directory to the beginning of
#' # a path in the AWS bucket.This can be used with files or folders.
#'
#' containerTemplateUtils::aws_s3_upload(path = "R/",
#'                                       key = "example_r_scripts",
#'                                       error = TRUE,
#'                                       prefix = "my_example_prefix",
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws keys will be "my_example_prefix/example_r_scripts/<files from R/>"
#'
#' # This can be useful if you're using version control
#' # systems like git and would like to organize files by branch
#'
#' library(gert)
#' git_prefix <- gert::git_branch()
#'
#' containerTemplateUtils::aws_s3_upload(path = "R/",
#'                                       key = "",
#'                                       error = TRUE,
#'                                       prefix = git_prefix,
#'                                       bucket =Sys.getenv("AWS_BUCKET"))
#'
#' # aws keys will be "<current GIT branch>/<files from R/>"
#'
#'
#'
#' }
#'
#'
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

    # check that the key has a file extension

    key_ext_check <- check_for_file_extension(key)

    if(!key_ext_check){

      key_1 <- sprintf("%s/%s/%s",prefix,key, path)
      key <- gsub("/{2,}", "/", key_1) ## correcting multiple slashes in key

      wrn_msg <- glue::glue(
      "Path is a single file and key does not have a file extension.
      Treating key as a subdirectory.
      Path in AWS is:
      {key}")

      warning(wrn_msg)

    }

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

    ## drop the path argument from the key precursor
    file_paths <- gsub(paste0("^", path), "", files)

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
