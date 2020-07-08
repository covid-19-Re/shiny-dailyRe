cleanEnv <- function(keepObjects, keepSelf = TRUE) {
  envVars <- ls(name = 1)
  if (keepSelf) {
    keepObjects <- c(keepObjects, "cleanEnv")
  }
  rm(list = setdiff(envVars, keepObjects), pos = 1)
}
