#' @rdname FileArray-class
#' @export
writeFileArray <- function(x, filepath=tempfile()){
    sink <- FileRealizationSink(dim = dim(x),
                                dimnames = dimnames(x),
                                type = type(x),
                                filepath = filepath)
    on.exit(filematrix::close(sink))
    BLOCK_write_to_sink(sink = sink, x = x) # write_block works by inheritance
    o <- as(sink, "FileArray")
    #filematrix::close(sink)
    o
}

#### Coercions ####

#' @export
setAs("ANY", "FileArray", function(from) writeFileArray(from))

#' @export
setAs("DelayedArray", "FileArray", function(from) writeFileArray(from))

#' @export
setAs("DelayedMatrix", "FileArray", function(from) writeFileArray(from))
