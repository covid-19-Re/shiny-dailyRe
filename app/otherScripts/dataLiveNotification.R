library(tidyverse)
library(here)

if (file.exists(here("app/otherScripts/sendNotifications.txt")) &&
  file.exists(here("app/otherScripts/notificationsToSend.txt"))) {
    
  source(here("app/otherScripts/utils.R"))
  countries <- scan(here("app/otherScripts/notificationsToSend.txt"), what = "character")
  urls <- scan(here("app/otherScripts/slackWebhook.txt"), what = "character")
  urls <- scan(here("app/otherScripts/slackWebhook.txt"), what = "character")

  for (country in countries) {
    sendSlackNotification(
      country = country,
      event = "dataLive",
      url = urls[1],
      webhookUrl = urls[2]
    )
  }
  cat(str_c(Sys.time(), " | Slack notifications sent.\n"))
  system(str_c("rm ", here("app/otherScripts/notificationsToSend.txt")))
}
