
Sys.setenv(TILEDB_REST_USERNAME="unittest")
Sys.setenv(TILEDB_REST_TOKEN=Sys.getenv("TILEDB_REST_TOKEN_UNITTEST"))

library(tiledbcloud)
library(tinytest)

api <- tiledbcloud:::.pkgenv[["api"]]

userApiInstance <- tiledbcloud:::.pkgenv[["userApiInstance"]]
res <- userApiInstance$GetSession()

api <- UserApi$new(apiClient=api)

resultObject <- userApiInstance$GetUser()
body <- tiledbcloud:::.get_raw_response_body_or_stop(resultObject)
res <- jsonlite::fromJSON(rawToChar(body))
expect_true(is.list(res))
expect_true(length(names(res)) >= 13)
expect_equal(res$is_valid_email, TRUE)
if ("email" %in% names(res) && res$email == "aws-mvp@tiledb.io") {
    # local environment variable can get in the way while developing
    expect_equal(res$email, "aws-mvp@tiledb.io")
    expect_equal(res$name, "Unit Test")
    expect_equal(res$username, "unittest")
    expect_equal(res$organizations$username, "unittest")
}
