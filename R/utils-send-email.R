#' Send email alerts to updated automation reports
#'
#' @param to A vector of email addresses serving as primary recipients for the
#'   message.
#' @param from The email address of the sender. Default is
#'   `rsconnectatecohealthalliance.org`.
#' @param test Logical. Is this an email alert for testing automation reports?
#'   Default is FALSE.
#'
#' @return Invisible. Update email sent to list of recipients in `to`
#'
#'

send_email_update <- function(to,
                              from = "rsconnect@ecohealthalliance.org",
                              test = FALSE) {
  email_credentials <- blastula::creds_envvar(
    user = from,
    host = "smtp.gmail.com",
    port = 587,
    use_ssl = TRUE
  )

  readable_date_time <- blastula::add_readable_time()

  reports <- list.files("outputs", pattern = "\\.html")

  if (test) {
    report_links <- paste0(
      Sys.getenv("URL_PREFIX"), "/", Sys.getenv("GITHUB_REF"),
      "/", reports, "\n"
    )

    subject <- glue::glue(
      "Testing Nipah Bangladesh Automated Reports for ", {readable_date_time}
    )

    email <- blastula::compose_email(
      body = glue::glue(
        "The test automation reports can be viewed here: \n\n",
        {report_links}, "\n\n"
      ) |>
        blastula::md()
    )
  } else {
    report_links <- paste0(
      Sys.getenv("URL_PREFIX"), "/", reports, "\n"
    )

    subject <- glue::glue(
      "Nipah Bangladesh Automated Reports for ", {readable_date_time}
    )

    email <- blastula::compose_email(
      body = glue::glue(
        "The updated automation reports can be viewed here: \n\n",
        {report_links}, "\n\n"
      ) |>
        blastula::md()
    )
  }

  blastula::smtp_send(
    email = email,
    to = to,
    from = from,
    subject = subject,
    credentials = email_credentials
  )
}
