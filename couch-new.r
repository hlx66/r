library(httr)

url <- "http://pip:5984/p"
auth <- authenticate("admin", "mallakaY23")

addDoc <- function(name) {
    r <- POST(url, body = list(new = TRUE, name = name), encode = "json", auth)
    stop_for_status(r)
    return(content(r, "parsed", "application/json"))
}
getDocRev <- function(id) {
    r <- GET(paste(url, id, sep = "/"), auth)
    stop_for_status(r)
    return(content(r, "parsed", "application/json")$`_rev`)
}
add2Doc <- function(id, p) {
    rev <- getDocRev(id)
    stopifnot(!is.null(rev))
    files <- list.files(path = p, include.dirs = TRUE, full.names = TRUE, recursive = "FALSE")
    for (f in files) {
        if (dir.exists(f)) {
            fWalk(f)
        } else {
            rev <- uploadFile(f, id, rev)
		}
    }
    unlink(p, recursive = TRUE)
}
uploadFile <- function(path, id, rev) {
    stopifnot(is.character(path), length(path) == 1, file.exists(path))
    fname = sub("^_", "", basename(path))
    furl <- paste(url, id, paste(URLencode(fname), "?rev=", rev, sep = ""), sep = "/")
    print(furl)
    r <- PUT(furl, auth, body = httr::upload_file(path), encode = mime::guess_type(path))
    stop_for_status(r)
    unlink(path)
    print(paste(" ", "-->", path))
    return(content(r, "parsed", "application/json")$rev)
}
fWalk <- function(p) {
    name <- basename(p)
    print(name)
    files <- list.files(path = p, include.dirs = TRUE, full.names = TRUE, recursive = FALSE)
    if (length(files) > 0) {
        doc <- addDoc(name)
        id <- doc$id
        print(paste("new document:", id))
        rev <- doc$rev
        for (f in files) {
            if (dir.exists(f)) {
                fWalk(f)
            } else {
                rev <- uploadFile(f, id, rev)
            }
        }
        if (length(list.files(path = p, include.dirs = TRUE, recursive = FALSE)) == 0) {
            unlink(p, recursive = TRUE)
        }
    }
}
uploadSingleFile <- function(f) (
    if(file.exists(f) && !dir.exists(f)) {
        doc <- addDoc(basename(f))
        uploadFile(f, doc$id, doc$rev)
    }

)

