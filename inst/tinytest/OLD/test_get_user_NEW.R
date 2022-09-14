if ((unitTestToken <- Sys.getenv("TILEDB_REST_UNIT_TEST_TOKEN")) != "") {
    Sys.setenv("TILEDB_REST_TOKEN"=unitTestToken)
    Sys.setenv("TILEDB_REST_USERNAME"="")
    Sys.setenv("TILEDB_REST_PASSWORD"="")
} else {
    if (!file.exists("~/.tiledb/cloud.json")) exit_file("No authentication")
}

#options(verbose=TRUE)
library(tiledbcloud)
library(tinytest)

#api <- tiledbcloud:::.pkgenv[["api"]]
#print(api)

api_key <- unitTestToken
username <- password <- ""
host <- "https://api.tiledb.com/v1"

apiClientInstance <- ApiClient$new(basePath=paste(host, "v1", sep="/"),
                                   accessToken=api_key,
                                   username=username,
                                   password=password,
                                   retryStatusCodes=c(408, 502, 503, 504)
                                   )

api <- UserApi$new(apiClientInstance)
#api$apiKeys['X-TILEDB-REST-API-KEY'] <- api_key
#print(apiClientInstance)
#print(UserApi)

res <- api$GetUser()
print(res)
##expect_true(is.list(res))
#expect_true(length(names(res)) >= 13)
#expect_equal(res$is_valid_email, TRUE)
if ("email" %in% names(res) && res$email == "aws-mvp@tiledb.io") {
    # local environment variable can get in the way while developing
    expect_equal(res$email, "aws-mvp@tiledb.io")
    expect_equal(res$name, "Unit Test")
    expect_equal(res$username, "unittest")
    expect_equal(res$organizations$username, "unittest")
}
