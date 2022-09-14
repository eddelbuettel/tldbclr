
##' De-register a 'Group' object recursively
##'
##' This function de-registers a 'Group' object, the 'Group' objects therein
##' as well as any arrays.
##'
##' Note that 'Group' objects remain on the underlying storage such as S3.
##'
##' @param uri A TileDB + S3 URI
##' @param namespace A character like "TileDB-Inc"
##' @param name A characther "groupABC"
##' @param delete_from_group A logical value, default `TRUE`, whether arrays are removed from the group
##' @param delete_array A logical value, default `TRUE`, whether arrays are deleted too
##' @param verbose A logical value, default `FALSE`, whether operations are verbose or not
##'
##' @return Nothing is returned, the function is invoked for its side-effect
##' @family {manual-layer functions}
##' @export
deregister_group <- function(uri, namespace, name,
                             delete_from_group = TRUE, delete_array = FALSE,
                             verbose = FALSE) {
    stopifnot("The 'uri' argument should be character" = is.character(uri),
              "The 'uri' should point to a group object" = all.equal(tiledb::tiledb_object_type(uri), "GROUP"))

    apiClientInstance <- get_api_client_instance()
    grpapi <- GroupsApi$new(apiClientInstance)
    arrapi <- ArrayApi$new(apiClientInstance)

    grp <- tiledb::tiledb_group(uri, ctx = tiledb::tiledb_ctx())
    cnt <- tiledb::tiledb_group_member_count(grp)			# how many elements in group
    if (verbose) message("Entered namespace '", namespace, "' and group '", name, "' with ", cnt, " objects")

    rl <- lapply(seq_len(cnt) - 1, function(i) tiledb::tiledb_group_member(grp, i))	# get all of them (account for zero offset)
    grp <- tiledb::tiledb_group_close(grp)

    for (i in seq_along(rl)) {          		# and process them in order
        obj <- rl[[i]]

        if (obj[1] == "GROUP") {				# recursively descend into group
            deregister_group(obj[2], namespace, obj[3], delete_from_group, delete_array, verbose)
        }

        if (obj[1] == "ARRAY") {        		# arrays we just remove
            arrid <- obj[3]

            if (delete_from_group) {            ## Remove the array from group using internal URI
                if (verbose) message("Removing array '", obj[2])
                grp <- tiledb::tiledb_group_open(grp, "WRITE")
                grp <- tiledb::tiledb_group_remove_member(grp, obj[2]) # use _internal_ url from query above
                grp <- tiledb::tiledb_group_close(grp)
            }

            if (delete_array) {
                if (verbose) message("Deregistering and deleting array '", arrid, "' in '", namespace, "'")
                arrapi$DeregisterArray(namespace, arrid)
                arrapi$DeleteArray(namespace, arrid, 'application/json')
            }
        }
    }
    if (delete_from_group) {
        if (verbose) message("Removing group '", name, "' in '", namespace, "'")
        grpapi$DeleteGroup(namespace, name)
    }
    invisible(NULL)
}
