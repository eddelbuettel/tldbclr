if ((unitTestToken <- Sys.getenv("TILEDB_REST_UNIT_TEST_TOKEN")) != "") {
    Sys.setenv("TILEDB_REST_TOKEN"=unitTestToken)
} else {
    if (!file.exists("~/.tiledb/cloud.json")) exit_file("No authentication")
}

library(tiledbcloud)
library(tinytest)

api <- tiledbcloud:::.pkgenv[["api"]]

res <- api$GetSession()
expect_true(is(res, "Token"))
expect_true(is.character(res$token))

getUTC <- function(x) as.POSIXct(x, tz="UTC", format="%Y-%m-%dT%H:%M:%OS")
issued <- getUTC(res$issued_at)
expires <- getUTC(res$expires_at)
expect_true(inherits(issued, "POSIXct"))
expect_true(inherits(expires, "POSIXct"))
expect_true(as.numeric(difftime(expires, issued, units="hours")) > 6)
