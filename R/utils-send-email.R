#' Send email alerts to updated automation reports
#'
#' @param to A vector of email addresses serving as primary recipients for the
#'   message.
#' @param from The email address of the sender.
#' @param attach Logical. Should reports be attached to email? Default is
#'   FALSE. If TRUE, all HTML reports found in *"outputs"* folder will be
#'   attached to email.
#' @param test Logical. Is this an email alert for testing automation reports?
#'   Default is FALSE.
#' @param project_name String. Name of the project to use in email subject and body text.
#'
#' @return Invisible. Update email sent to list of recipients in `to`
#'
#' @export send_email_update

send_email_update <- function(to,
                              from = "rsconnect@ecohealthalliance.org",
                              project_name,
                              attach = FALSE,
                              test = FALSE) {
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
  reports <- list.files("outputs", pattern = "\\.html")

  ## Create links to HTML reports and create email
  if (test) {
    report_links <- sprintf("%s/%s/outputs/%s\n",
                            Sys.getenv("URL_PREFIX"),
                            Sys.getenv("GITHUB_REF"),
                            reports)


    subject <- glue::glue(
      "Testing {project_name} Automated Reports for ", {readable_date_time}
    )

    email <- blastula::compose_email(
      body = glue::glue(
        "The test automation reports can be viewed here: \n\n",
        {report_links}, "\n\n",
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

    report_links_collapse <- glue::glue_collapse(report_links,sep = ", ",last = "and")

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
    for (i in list.files("outputs", pattern = "\\.html$", full.names = TRUE)) {
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
