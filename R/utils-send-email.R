#' Send email alerts to updated automation reports
#'
#' @param to A vector of email addresses serving as primary recipients for the
#'   message.
#' @param from The email address of the sender.
#' @param attach Logical. Should reports be attached to email? Default is
#'   FALSE. If TRUE, all HTML reports found in *"outputs"* folder will be
#'   attached to email.
#' @param test Logical. Is this an email alert for testing automation reports?
#'   Default is FALSE. If TRUE, subject includes test and upload path will include
#'   the current git branch in the file path (e.g. https://project.secure.eha.io/refs/fix/missing_documentation/file.txt)
#' @param project_name String. Name of the project to use in email subject and body text.
#' @param path String. Name of folder or file path for attachment items
#' @param pattern String. Regex pattern to select specific files in path.
#'
#' @seealso `browseVignettes("blastula")`
#'
#' @return Invisible. Update email sent to list of recipients in `to`
#'
#' @export send_email_update

send_email_update <- function(to,
                              from = "rsconnect@ecohealthalliance.org",
                              project_name,
                              attach = FALSE,
                              test = FALSE,
                              path = "outputs",
                              pattern= "\\.html") {
  ## Set SMTP server credentials
  email_credentials <- blastula::creds_envvar(
    user = from,
    host = "smtp.gmail.com",
    port = 587,
    use_ssl = TRUE
  )

  ## Create human readable data and time
  readable_date_time <- blastula::add_readable_time()

  ## List out reports found in "outputs" folder
  reports <- list.files(path, pattern = pattern)

  ## Create links to HTML reports and create email
  if (test) {
    report_links <- sprintf("%s/%s/outputs/%s\n",
                            Sys.getenv("URL_PREFIX"),
                            Sys.getenv("GITHUB_REF"),
                            reports)


    subject <- glue::glue(
      "Testing {project_name} Automated Reports for ", {readable_date_time}
    )

    report_links_collapse <- glue::glue_collapse(report_links,sep = ", ",last = "and ")

    email <- blastula::compose_email(
      body = glue::glue(
        "The test automation reports can be viewed here: \n\n",
        {report_links_collapse}, "\n\n",
        "A copy/copies of the {project_name} automated report/s is/are also attached. \n\n"
      ) |>
        blastula::md()
    )
  } else {
    report_links <- paste0(
      Sys.getenv("URL_PREFIX"), "/", reports, "\n"
    )

    subject <- glue::glue(
      "{project_name} Automated Reports for ", {readable_date_time}
    )

    report_links_collapse <- glue::glue_collapse(report_links,sep = ", ",last = "and ")

    email <- blastula::compose_email(
      body = glue::glue(
        "The {project_name} automated reports can be viewed here: \n\n",
        {report_links_collapse}, "\n\n",
        "A copy/copies of the {project_name} automated report/s is/are also attached. \n\n"
      ) |>
        blastula::md()
    )
  }

  ## Add attachements
  if (attach) {
    for (i in list.files(path = path, pattern = pattern, full.names = TRUE)) {
      email <- email |>
        blastula::add_attachment(file = i)
    }
  } else {
    email
  }

  ## Send email
  blastula::smtp_send(
    email = email,
    to = to,
    from = from,
    subject = subject,
    credentials = email_credentials
  )
}


#' Send email alerts to updated automation reports
#'
#' @param to A vector of email addresses serving as primary recipients for the
#'   message.
#' @param from The email address of the sender.
#' @param attach Logical. Should reports be attached to email? Default is
#'   FALSE. If TRUE, all HTML reports found in *"outputs"* folder will be
#'   attached to email.
#' @param test Logical. Is this an email alert for testing automation reports?
#'   Default is FALSE. If TRUE, subject includes test and upload path will include
#'   the current git branch in the file path (e.g. https://project.secure.eha.io/refs/fix/missing_documentation/file.txt)
#' @param project_name String. Name of the project to use in email subject and body text.
#' @param pattern String. Regex pattern to select specific files in path.
#' @param target Targets object. List of file paths.
#' @param use_hyperlinks Logical. If TRUE, a hyperlink using the file name is provided instead of the
#' the full url of the report.
#'
#' @seealso `browseVignettes("blastula")`
#'
#' @return Invisible. Update email sent to list of recipients in `to`
#'
#' @export send_email_update_tar

send_email_update_tar <- function(to,
                              from = "rsconnect@ecohealthalliance.org",
                              project_name,
                              use_hyperlinks = FALSE,
                              attach = FALSE,
                              test = FALSE,
                              target,
                              pattern= "\\.html") {
  ## Set SMTP server credentials
  email_credentials <- blastula::creds_envvar(
    user = from,
    host = "smtp.gmail.com",
    port = 587,
    use_ssl = TRUE
  )

  ## Create human readable data and time
  readable_date_time <- blastula::add_readable_time()

  ## List out reports found in target object
  reports <- target[grepl(pattern = pattern,x = target)]

  ## Create links to HTML reports and create email ----
  if (test) {
    report_links <- sprintf("%s/%s/outputs/%s\n",
                            Sys.getenv("URL_PREFIX"),
                            Sys.getenv("GITHUB_REF"),
                            basename(reports) )

    if(use_hyperlinks){
      report_links <- sprintf("[Current %s](%s)",basename(reports),report_links )
    }
    subject <- glue::glue(
      "Testing {project_name} Automated Reports for ", {readable_date_time}
    )

    report_links_collapse <- glue::glue_collapse(report_links,sep = ", ",last = "and ")

    email <- blastula::compose_email(
      body = glue::glue(
        "The test automation reports can be viewed here: \n\n",
        {report_links_collapse}, "\n\n",
        "A copy/copies of the {project_name} automated report/s is/are also attached.
        For the best viewing experience, download the report before opening.\n\n"
      ) |>
        blastula::md()
    )
  } else {
    report_links <- paste0(
      Sys.getenv("URL_PREFIX"), "/", basename(reports), "\n"
    )

    if(use_hyperlinks){
      report_links <- sprintf("[Current %s](%s)",basename(reports),report_links )
    }

    subject <- glue::glue(
      "{project_name} Automated Reports for ", {readable_date_time}
    )

    report_links_collapse <- glue::glue_collapse(report_links,sep = ", ",last = "and ")

    email <- blastula::compose_email(
      body = glue::glue(
        "The {project_name} automated reports can be viewed here: \n\n",
        {report_links_collapse}, "\n\n",
        "A copy/copies of the {project_name} automated report/s is/are also attached.
        For the best viewing experience, download the report before opening. \n\n"
      ) |>
        blastula::md()
    )
  }

  ## Add attachements
  if (attach) {
    for (i in reports) {
      email <- email |>
        blastula::add_attachment(file = i)
    }
  } else {
    email
  }

  ## Send email
  blastula::smtp_send(
    email = email,
    to = to,
    from = from,
    subject = subject,
    credentials = email_credentials
  )
}

