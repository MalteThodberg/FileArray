#### FileArraySeed ####

# Class
.FileArraySeed <- setClass("FileArraySeed",
                           slots=c(filepath="character",
                                   dim="integer",
                                   dimnames="list",
                                   type="character"))

# Validity
#' @importFrom S4Vectors setValidity2
setValidity2("FileArraySeed", function(object) {
    # More check here?

    # Try and load
    fm <- suppressWarnings(tryCatch(filematrix::fm.open(object@filepath), error=function(e) e))

    if(!is(fm, "filematrix")){
        o <- "Could not read filematrix from supplied location!"
    }else if(any(dim(fm) != object@dim)){
        o <- "Dimensions of the FileArray does not match the filematrix"
    }else{
        o <- TRUE
    }

    # Return
    o
})

# Constructor
FileArraySeed <- function(filepath){
    # Safe filename
    filepath <- paste0(filepath, ".bmat")
    filepath <- tools::file_path_as_absolute(filepath)
    filepath <- tools::file_path_sans_ext(filepath)

    # Open/close connection
    fm <- fm.open(filenamebase=filepath,
                  readonly = TRUE,
                  lockfile = NULL)
    on.exit(filematrix::close(fm))

    # Build and return seed
    .FileArraySeed(filepath=filepath,
                   dim=as.integer(dim(fm)),
                   dimnames=dimnames(fm),
                   type=fm$type)
}

# Show method
setMethod("show", "FileArraySeed", function(object){
    cat("# FileArraySeed-object:\n")
    cat("filepath:", object@filepath, "\n")
    cat("rows:", object@dim[1], "\n")
    cat("columns:", object@dim[2], "\n")
    cat("type:", object@type)
})



# Getters
#setMethod("path", "FileArraySeed", function(object){object@filepath})
#setMethod("type", "FileArrasySeed", function(x){x@type})

#### FileArray and FileMatrix ####

#' FileArray: DelayedArray backend for filematrix.
#'
#' FileArray allows you to use the filematrix package as a backend for DelayedArrays, similarly to the default HDF5Array package.
#'
#' @param filepath character: Path to the basename of a filematrix
#' @param x matrix, DelayedArray, etc.: matrix-like object to be written block-by-block to disk.
#' @return A FileMatrix object (A 2D FileArray)
#'
#' @details FileArraySeed, FileArray, FileMatrix, FileRealizationSink, read-only / open connections.
#'
#' @export
#' @examples
#' #### Examples ####
#'
#' mat <- as.matrix(iris[,-5])
#'
#' # Coerce a matrix to a FileArray using a temporary filematrix backing file:
#' as(mat, "FileArray")
#'
#' # Use writeFileArray to specify the location of the backing file:
#' my_backing_file <- tempfile(pattern="filematrix")
#' writeFileArray(mat, filepath=tempfile())
#'
#' # Create a FileArray directly from a filematrix backing file
#' manual_backing_file <- tempfile(pattern="filematrix")
#' filematrix::fm.create.from.matrix(mat=mat, filenamebase=manual_backing_file)
#' FileArray(manual_backing_file)
#'
#' # FileArray supports all DelayedArray operations. See vignette for examples.
#'
#' #### Sanity checks ####
#'
#' # Check that matrix can be converted to/from
#' dbl_mat <- replicate(n = 25, rnorm(1000))
#' int_mat <- round(dbl_mat * 100)
#' lgl_mat <- int_mat > 0
#'
#' dbl_fa <- as(dbl_mat, "FileArray")
#' int_fa <- as(int_mat, "FileArray")
#' lgl_fa <- as(lgl_mat, "FileArray")
#'
#' stopifnot(as.matrix(dbl_fa) == dbl_mat,
#'           as.matrix(int_fa) == int_mat,
#'           as.matrix(lgl_fa) == lgl_mat)
#'
#' # Check that empty matrices works
#' rows_mat <-  matrix(nrow = 0, ncol=10)
#' cols_mat <-  matrix(nrow = 10, ncol=0)
#' both_mat <- matrix(nrow=0, ncol=0)
#'
#' rows_fa <- as(rows_mat, "FileArray")
#' cols_fa <- as(cols_mat, "FileArray")
#' both_fa <- as(both_mat, "FileArray")
#'
#' stopifnot(as.matrix(rows_fa) == rows_mat,
#'           as.matrix(cols_fa) == cols_mat,
#'           as.matrix(both_fa) == both_mat)
setClass("FileArray",
                       contains  = "DelayedArray",
                       slots = c(seed = "FileArraySeed"))

#' @rdname FileArray-class
#' @export
setClass("FileMatrix",
                        contains = "DelayedMatrix",
                        slots = c(seed = "FileArraySeed"))

setMethod("DelayedArray", "FileArraySeed",
          function(seed) new_DelayedArray(seed, Class="FileArray"))

#' @rdname FileArray-class
#' @export
FileArray <- function(filepath){
    # Allow FileArraySeed
    if (is(filepath, "FileArraySeed")){
        seed <- filepath
    }else{
        seed <- FileArraySeed(filepath)
    }

    # Return
    DelayedArray(seed)
}


# FileMatrix infrastructure
setMethod("matrixClass", "FileArray", function(x) "FileMatrix")
setAs("FileArray", "FileMatrix", function(from) new("FileMatrix", from))
setAs("FileMatrix", "FileArray", function(from) from)  # no-op

#### FileRealizationSink ####

# Realization Sink for a FileArray
.FileRealizationSink <- setRefClass("FileRealizationSink",
                                    contains = c("filematrix", "RealizationSink"),
                                    inheritPackage=TRUE)

# Constructor
FileRealizationSink <- function(dim, dimnames, type, filepath=tempfile()){
    # Create filematrix
    fm <- fm.create(filenamebase = filepath,
                    nrow = dim[1],
                    ncol = dim[2],
                    type = type)

    # Set colnames
    dimnames(fm) <- dimnames

    # Return
    as(fm, "FileRealizationSink")
}


# Overwrite some methods to be compatible with DelayedArray
setMethod("type", "FileRealizationSink", function(x){x$type})

#' Internal functions
#'
#' Internal functions that should not be used called directly.
#'
#' @param x FileArraySeed, FileArray, FileRealizationSink
setMethod("dim", "FileRealizationSink", function(x){as.integer(callNextMethod())})
#setMethod("path", "FileRealizationSink", function(object){tools::file_path_sans_ext(object$data.filename)})

# Coercions
setAs("FileRealizationSink", "FileArraySeed", function(from) FileArraySeed(tools::file_path_sans_ext(from$data.filename)))
setAs("FileRealizationSink", "FileArray", function(from) DelayedArray(as(from, "FileArraySeed")))
setAs("FileRealizationSink", "DelayedArray", function(from) as(from, "FileArray"))

