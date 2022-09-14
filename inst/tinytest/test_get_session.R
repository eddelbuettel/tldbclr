
library(tiledbcloud)
library(tinytest)

userApiInstance <- tiledbcloud:::.pkgenv[["userApiInstance"]]

res <- userApiInstance$GetSession()

expect_true(is(res, "Token"))
expect_true(is.character(res$token))

getUTC <- function(x) as.POSIXct(x, tz="UTC", format="%Y-%m-%dT%H:%M:%OS")
issued <- getUTC(res$issued_at)
expires <- getUTC(res$expires_at)
expect_true(inherits(issued, "POSIXct"))
expect_true(inherits(expires, "POSIXct"))
expect_true(as.numeric(difftime(expires, issued, units="hours")) > 6)
