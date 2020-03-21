is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}

make_block <- function(x, n = raster::nlayers(x), blocksize = 1e9, 
                       n_rows = NULL) {
  stopifnot(inherits(x, "Raster"))
  stopifnot(is.numeric(blocksize), length(blocksize) == 1, blocksize > 1000)
  stopifnot(is_integer(n), length(n) == 1, n > 0)
  if (!is.null(n_rows)) {
    stopifnot(is_integer(n_rows), length(n_rows) == 1, n_rows > 0)
    n_rows <- min(n_rows, raster::nrow(x))
    rows_per_block <- n_rows
  } else {
    row_bytes <- 8 * n * raster::ncol(x)
    rows_per_block <- floor(blocksize / row_bytes)
  }
  # always process full rows, limit to rows in input raster
  rows_per_block <- min(raster::nrow(x), max(rows_per_block, 1))
  
  # create blocks
  start_row <- seq(1, raster::nrow(x), by = rows_per_block)
  n_blocks <- length(start_row)
  n_rows <- rep(rows_per_block, n_blocks)
  
  # correct size of final block
  dif <- n_blocks * rows_per_block - raster::nrow(x)
  n_rows[length(n_rows)] <- n_rows[length(n_rows)] - dif
  
  return(list(start_row = start_row, n_rows = n_rows, n = n_blocks))
}

quick_calc <- compiler::cmpfun(function(x, fun = c("mean", "sum"), 
                                        blocksize = 1e9, n_rows = NULLL,
                                        filename = tempfile(), ...) {
  stopifnot(inherits(x, "Raster"))
  stopifnot(is.numeric(blocksize), length(blocksize) == 1, blocksize > 1000)
  if (!is.null(n_rows)) {
    stopifnot(is_integer(n_rows), length(n_rows) == 1, n_rows > 0)
  }
  stopifnot(is.character(filename), length(filename) == 1)
  fun <- match.arg(fun)
  
  out <- raster::raster(x)
  
  if (fun == "mean") {
    fun = rowMeans
  } else if (fun == "sum") {
    fun = rowSums
  }
  
  x <- raster::readStart(x)
  out <- raster::writeStart(out, filename = filename, ...)
  b <- make_block(out, n = 2 * (raster::nlayers(x) + 1),
                  blocksize = blocksize, n_rows = n_rows)
  
  for (i in seq_along(b$start_row)) {
    v <- raster::getValues(x, row = b$start_row[i], nrows = b$n_rows[i])
    v <- fun(v, na.rm = TRUE)
    out <- raster::writeValues(out, v, b$start_row[i])
  }
  out <- raster::writeStop(out)
  x <- raster::readStop(x)
  return(out)
})
