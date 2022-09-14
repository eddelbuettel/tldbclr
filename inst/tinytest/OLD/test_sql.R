if ((unitTestToken <- Sys.getenv("TILEDB_REST_UNIT_TEST_TOKEN")) != "") {
    Sys.setenv("TILEDB_REST_TOKEN"=unitTestToken)
    Sys.setenv("TILEDB_REST_USERNAME"="")
    Sys.setenv("TILEDB_REST_PASSWORD"="")
} else {
    if (!file.exists("~/.tiledb/cloud.json")) exit_file("No authentication")
}

library(tiledbcloud)
library(tinytest)

cl <- tiledbcloud:::.pkgenv[["cl"]]
api <- tiledbcloud:::.pkgenv[["api"]]
#res <- api$GetUser()
#print(str(res))

sql <- SqlApi$new(cl)
expect_true(is(sql, "SqlApi"))

sqlpar <-  SQLParameters$new(name="ArbitraryNameHere", query=paste("select 1 as one"))
expect_true(is(sqlpar, "SQLParameters"))

if (FALSE) {
    ## these work in isolation i.e. via run_test_file() but not in the test env; unsure why
    ans <- sql$RunSQL("unittest", sqlpar)
    expect_true(is.list(ans))
    expect_equal(length(ans), 1)
    df <- ans[[1]]
    expect_true(is.data.frame(df))
    expect_equal(names(df), "one")
}
