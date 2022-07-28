
# Copied from DelayedArray
.expand_Nindex_RangeNSBS <- function(Nindex){
    stopifnot(is.list(Nindex))
    expand_idx <- which(vapply(Nindex, is, logical(1), "RangeNSBS"))
    if (length(expand_idx) != 0L)
        Nindex[expand_idx] <- lapply(Nindex[expand_idx], as.integer)
    Nindex
}

# Copied from S4Vectors
.sapply_isNULL <- function(objects)
    vapply(objects, is.null, logical(1), USE.NAMES=FALSE)


# Copied from DelayedArray
.make_subscripts_from_Nindex <- function(Nindex, x){
    stopifnot(is.list(Nindex), length(Nindex) == length(dim(x)))

    if (is.array(x))
        Nindex <- .expand_Nindex_RangeNSBS(Nindex)

    ## Replace NULLs with list elements of class "name".
    subscripts <- rep.int(list(quote(expr=)), length(Nindex))
    names(subscripts) <- names(Nindex)
    not_missing_idx <- which(!.sapply_isNULL(Nindex))
    subscripts[not_missing_idx] <- Nindex[not_missing_idx]

    subscripts
}

# Modified from DelayedArray: Drop argument removed
.subset_by_Nindex <- function(x, Nindex){
    subscripts <- .make_subscripts_from_Nindex(Nindex, x)
    do.call(`[`, c(list(x), subscripts))
}

# Wrapper for filematrix in seed
.extract_array_from_filematrix <- function(x, index){
    # Open/close connection
    fm <- filematrix::fm.open(filenamebase=x@filepath,
                  readonly = TRUE,
                  lockfile = NULL)
    on.exit(filematrix::close(fm))

    # Supress messages if empty dim
    if(any(S4Vectors::elementNROWS(index) == 0)){
        o <- suppressWarnings(.subset_by_Nindex(x = fm, Nindex = index))
    }else{
        o <- .subset_by_Nindex(x = fm, Nindex = index)
    }

    # Return
    o
}

setMethod("extract_array", "FileArraySeed", .extract_array_from_filematrix)
