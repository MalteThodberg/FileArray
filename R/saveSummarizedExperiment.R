#' Save/Load a FileArray-based SummarizedExperiment object
#'
#' Saves a SummarizedExperiment-object do disk where each assay is stored as a FileArray.
#' Modelled after \link[HDF5Array]{saveHDF5SummarizedExperiment} and \link[HDF5Array]{loadHDF5SummarizedExperiment}
#'
#' @param x SummarizedExperiment: All assays will be coerced to FileArray objects.
#' @param dir character: Path to directory where to save the data.
#' @param prefix character: Optional prefix to add to the saved files. Allows saving more than one object in the same directory
#' @param replace logical: Whether to overwrite/delete all files if directory already exists.
#'
#' @return A SummarizedExperiment object where each assay is stored as a FileArray.
#' @export
saveFileArraySummarizedExperiment <- function(x,
                                         dir = tempfile(pattern="dir"),
                                         prefix="",
                                         replace=FALSE){

    # Check SummarizedExperiment package is available
    if (!requireNamespace("SummarizedExperiment", quietly=TRUE)){
        stop("This function requires the SummarizedExperiment package!")
    }

    # Pre-checks
    checkmate::assertString(dir)
    checkmate::assertString(prefix)
    checkmate::assertFlag(replace)
    checkmate::assertClass(x, "SummarizedExperiment")

    # Make dir cascade
    if(dir.exists(dir)){
        if(isTRUE(replace)){
            if(prefix != ""){
                tmp <- length(list.files(dir, pattern = paste0("^", prefix)))
                if(tmp > 0){
                    stop("prefix already in use")
                }else{
                    message("Adding new files to directory...")
                }
            }else{
                message("Deleting contents of directory and adding new files...")
                file.remove(list.files(dir, full.names = TRUE))
            }
        }else{
            stop("Directory already exists (overwrite by setting replace=TRUE)")
        }
    }else{
        tmp <- dir.create(dir)

        if(isFALSE(tmp)){
            stop("Could not create directory")
        }else{
            message("Created new directory...")
        }
    }

    # Make files
    message("Setting up files...")
    rds_filepath <- tempfile(pattern=prefix, tmpdir=dir, fileext=".rds")
    n_assays <- length(SummarizedExperiment::assays(x))
    assay_filepaths <- replicate(n = n_assays, expr = tempfile(pattern=prefix, tmpdir=dir))

    # Write assays
    message("Writing FileArray data...")
    SummarizedExperiment::assays(x) <- S4Vectors::mendoapply(FUN = writeFileArray,
                          SummarizedExperiment::assays(x),
                          assay_filepaths)

    # Write RDS
    message("Writing RDS data...")
    saveRDS(x, file = rds_filepath)

    # Return
    invisible(x)
}

# Update the seed (Should be used carefully!)
.update_dir <- function(fa, dir){
    seed(fa)@filepath <- file.path(dir, basename(seed(fa)@filepath))
    fa
}

#' @rdname  saveFileArraySummarizedExperiment
#' @export
#' @examples
#' #### Examples ####
#'
#' library(SummarizedExperiment)
#'
#' nrow <- 200
#' ncol <- 6
#' counts <- matrix(as.integer(runif(nrow * ncol, 1, 1e4)), nrow)
#' colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                      row.names=LETTERS[1:6])
#' se0 <- SummarizedExperiment(assays=list(counts=counts), colData=colData)
#' se0
#'
#' ## Save 'se0' as an HDF5-based SummarizedExperiment object:
#' dir <- tempfile("h5_se0_")
#' h5_se0 <- saveFileArraySummarizedExperiment(se0, dir)
#' list.files(dir)
#'
#' h5_se0
#' assay(h5_se0)
#'
#' h5_se0b <- loadFileArraySummarizedExperiment(dir)
#' h5_se0b
#' assay(h5_se0b)
#'
#' #### Sanity checks ####
#'
#' a1 <- as(as.matrix(iris[,-5]), "FileArray")
#' a2 <- a1 * 10
#' a3 <- round(a2)
#' se1 <- SummarizedExperiment(assays=SimpleList(a1=a1, a2=a2),
#'                             rowData = iris[,5, drop=FALSE])
#' se2 <- SummarizedExperiment(assays=SimpleList(a3 = a3),
#'                             rowData = iris[,5, drop=FALSE])
#'
#' # Save to a directory
#' input_dir <- tempfile()
#' input_se1 <- saveFileArraySummarizedExperiment(se1, dir = input_dir)
#'
#' # Move and reload
#' output_dir <- paste0(input_dir, "_moved")
#' file.rename(input_dir, to = output_dir)
#' output_se1 <- loadFileArraySummarizedExperiment(output_dir)
#'
#' # Check reload works
#' stopifnot(all(a1 ==  assay(output_se1, "a1")),
#'           all(a2 ==  assay(output_se1, "a2")))
#'
#' # Check prefixes work
#' prefix_dir <- tempfile()
#' A1 <- saveFileArraySummarizedExperiment(se1, dir = prefix_dir, prefix = "A_")
#' B1 <- saveFileArraySummarizedExperiment(se2, dir = prefix_dir, prefix = "B_", replace = TRUE)
#'
#' A2 <- loadFileArraySummarizedExperiment(prefix_dir, prefix = "A_")
#' B2 <- loadFileArraySummarizedExperiment(prefix_dir, prefix = "B_")
#'
#' stopifnot(all(assay(A1) == assay(A2)),
#'           all(assay(B1) == assay(B2)))
loadFileArraySummarizedExperiment <- function(dir, prefix=""){
    # Check SummarizedExperiment package is available
    if (!requireNamespace("SummarizedExperiment", quietly=TRUE)){
        stop("This function requires the SummarizedExperiment package!")
    }

    # Pre-checks
    checkmate::assertString(dir)
    checkmate::assertString(prefix)
    checkmate::assertDirectoryExists(dir)

    # Find RDS
    message("Looking for RDS file...")
    rds <- list.files(dir, full.names = TRUE, pattern=".rds$")

    # Find specific prefix
    if(prefix != ""){
        tmp <- grepl(basename(rds), pattern = paste0("^", prefix))
        rds <- rds[tmp]
    }

    # Fail if more than one
    if(length(rds) != 1){
        stop("Directory must contain exactly one .rds file (with/without prefix)")
    }

    message("Reading RDS file...")
    o <- readRDS(rds)

    message("Updating paths...")
    SummarizedExperiment::assays(o) <- S4Vectors::endoapply(SummarizedExperiment::assays(o),
                                                            FUN = .update_dir, dir=dir)

    message("Checking FileArray data...")
    seeds <- S4Vectors::endoapply(SummarizedExperiment::assays(o), seed)
    tmp <- S4Vectors::endoapply(seeds, validObject)

    # Return
    o
}

#### Heap ####


# # Fake data
# dir = "~/Desktop/DelayedArrayTest/"
# prefix=""
# replace=FALSE
# SE0 <- SummarizedExperiment::SummarizedExperiment(assays=list(A=as.matrix(iris[,-5]),
#                                                             B=as.matrix(iris[,-5])))


#SE <- saveFileArraySummarizedExperiment(x=SE0, dir = dir, replace = TRUE, prefix = "A_")
#SE2 <- saveFileArraySummarizedExperiment(x=SE0, dir = dir, replace = TRUE, prefix = "B_")
# SE3 <- saveFileArraySummarizedExperiment(x=SE0, dir = dir, replace = TRUE, prefix = "A_") should fail
#SE4 <- saveFileArraySummarizedExperiment(x=SE0, dir = dir, replace = TRUE, prefix = "")

#
#
#
#
#
# ####
#
# .setup_folder <- function(folder, assays, replace=FALSE, prefix=""){
#     # If cascade
#
#     if(dir.exists(folder)){
#         if(isTRUE(replace)){
#             if(prefix != ""){
#                 message("Deleting contents of directory and adding new files...")
#                 file.remove(list.files(folder, full.names = TRUE))
#             }else{
#                 message("Adding new files to directory...")
#             }
#         }else{
#             stop("Directory already exists (overwrite by setting replace=TRUE)")
#         }
#     }else{
#         dir.create(folder)
#     }
#
#     # Return
#     dir
# }
#
#
#
# .write_data_from_se <- 1
#
#
# #### Modified from HDF5Array ####
#
# .create_dir <- function(dir)
# {
#     if (file.exists(dir))
#         stop(wmsg("\"", dir, "\" already exists and is a file, ",
#                   "not a directory"))
#     if (!suppressWarnings(dir.create(dir)))
#         stop(wmsg("cannot create directory \"", dir, "\""))
# }
#
# .replace_dir <- function(dir, replace)
# {
#     if (!replace)
#         stop(wmsg("Directory \"", dir, "\" already exists. ",
#                   "Use 'replace=TRUE' to replace it. ",
#                   "Its content will be lost!"))
#     if (unlink(dir, recursive=TRUE) != 0L)
#         stop(wmsg("failed to delete directory \"", dir, "\""))
#     if (!suppressWarnings(dir.create(dir)))
#         stop(wmsg("cannot create directory \"", dir, "\""))
# }
#
# .check_and_delete_files <- function(rds_path, h5_path, replace)
# {
#     if (dir.exists(rds_path) || dir.exists(h5_path))
#         stop(wmsg("\"", rds_path, "\" and/or \"", h5_path, "\" ",
#                   "already exist and are directories, not files"))
#     if (!(file.exists(rds_path) || file.exists(h5_path)))
#         return()
#     if (!replace)
#         stop(wmsg("Files \"", rds_path, "\" and/or \"", h5_path, "\" ",
#                   "already exist. Use a different 'prefix' or use ",
#                   "'replace=TRUE' if you really want to replace them."))
#     if (unlink(rds_path, recursive=TRUE) != 0L)
#         stop(wmsg("failed to delete file \"", rds_path, "\""))
#     if (unlink(h5_path, recursive=TRUE) != 0L)
#         stop(wmsg("failed to delete file \"", h5_path, "\""))
# }
#
# ### Save all the assays in HDF5 format, including in-memory assays.
# ### Delayed assays with delayed operations on them are realized while they
# ### are written to disk..
# saveHDF5SummarizedExperiment <- function(x, dir="my_h5_se", prefix="",
#                                          replace=FALSE,
#                                          chunkdim=NULL, level=NULL,
#                                          as.sparse=NA,
#                                          verbose=NA)
# {
#     .load_SummarizedExperiment_package()
#
#     if (!is(x, "SummarizedExperiment"))
#         stop(wmsg("'x' must be a SummarizedExperiment object"))
#
#     if (!isSingleString(dir))
#         stop(wmsg("'dir' must be a single string specifying the path ",
#                   "to the directory where to save the ", class(x),
#                   " object (the directory will be created if needed)"))
#
#     if (!isSingleString(prefix))
#         stop(wmsg("'prefix' must be a single string"))
#
#     if (!isTRUEorFALSE(replace))
#         stop(wmsg("'replace' must be TRUE or FALSE"))
#
#     verbose <- DelayedArray:::normarg_verbose(verbose)
#
#     if (!dir.exists(dir)) {
#         create_dir(dir)
#     } else if (prefix == "") {
#         replace_dir(dir, replace)
#     }
#     rds_path <- file.path(dir, paste0(prefix, .SE_RDS_BASENAME))
#     h5_path <- file.path(dir, paste0(prefix, .ASSAYS_H5_BASENAME))
#     if (prefix != "")
#         check_and_delete_files(rds_path, h5_path, replace)
#
#     .write_HDF5SummarizedExperiment(x, rds_path=rds_path,
#                                     h5_path=h5_path,
#                                     chunkdim=chunkdim, level=level,
#                                     as.sparse=as.sparse,
#                                     verbose=verbose)
# }
#
#
#
#
#
