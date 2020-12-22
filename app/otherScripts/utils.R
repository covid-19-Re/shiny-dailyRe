cleanEnv <- function(keepObjects, keepSelf = TRUE) {
  envVars <- ls(name = 1)
  if (keepSelf) {
    keepObjects <- c(keepObjects, "cleanEnv", "sendSlackNotification")
  }
  rm(list = setdiff(envVars, keepObjects), pos = 1)
}

sendSlackNotification <- function(country, event, url, eTcompletion, webhookUrl) {
  if (event == "newData") {
    message <- list(
      text = str_c(country, ": new incidence data"),
      blocks = list(list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = str_c(
            Sys.time(), ": *New incidence data for ", country, " is available.*\n",
            "updating ", url, "\n",
            "Estimated time of completion of update: ~ ", eTcompletion)
        )
      ))
    )
  } else if (event == "updateDone") {
    message <- list(
      text = str_c(country, ": new Re estimates calculated"),
      blocks = list(list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = str_c(
            Sys.time(), ": *New Re estimates for ", country, " are available. Will go live 13:30 (Mon-Fri) / 15:30 (Sat/Sun)!*")
        )
      ))
    )
  } else if (event == "dataLive") {
    message <- list(
      text = str_c(country, ": new Re estimates"),
      blocks = list(list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = str_c(
            "<!channel>\n",
            Sys.time(), ": *New Re estimates for ", country, " are live!*\n",
            ":rocket: ", url)
        )
      ))
    )
  } else {
    warning("Invalid Event: No Slack notification sent")
  }

  out <- system(
      str_c(
        "curl -X POST -H 'Content-type: application/json' --data ",
        "'", jsonlite::toJSON(message, flatten = TRUE, auto_unbox = TRUE), "' ",
        webhookUrl
      )
    )

  invisible(out)
}
