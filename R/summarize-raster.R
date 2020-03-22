is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}

make_block <- function(x, n = nlayers(x), chunksize = 1, n_rows = NULL) {
  stopifnot(inherits(x, "Raster"))
  stopifnot(is.numeric(chunksize), length(chunksize) == 1, chunksize > 0.01)
  stopifnot(is_integer(n), length(n) == 1, n > 0)
  
  # convert to gb
  chunksize <- chunksize * 1e9
  
  # calculate rows per chunksize
  if (!is.null(n_rows)) {
    stopifnot(is_integer(n_rows), length(n_rows) == 1, n_rows > 0)
    n_rows <- min(n_rows, nrow(x))
    rows_per_block <- n_rows
  } else {
    row_bytes <- 8 * n * ncol(x)
    rows_per_block <- floor(chunksize / row_bytes)
  }
  # always process full rows, limit to rows in input raster
  rows_per_block <- min(nrow(x), max(rows_per_block, 1))
  
  # create blocks
  start_row <- seq(1, nrow(x), by = rows_per_block)
  n_blocks <- length(start_row)
  n_rows <- rep(rows_per_block, n_blocks)
  
  # correct size of final block
  dif <- n_blocks * rows_per_block - nrow(x)
  n_rows[length(n_rows)] <- n_rows[length(n_rows)] - dif
  
  return(list(start_row = start_row, n_rows = n_rows, n = n_blocks))
}

summarize_raster <- function(x, fun = c("mean", "sum"), 
                        chunksize = 1, n_rows = NULL,
                        filename = tempfile(), ...) {
  stopifnot(inherits(x, "Raster"))
  stopifnot(is.numeric(chunksize), length(chunksize) == 1, chunksize > 0.01)
  if (!is.null(n_rows)) {
    stopifnot(is_integer(n_rows), length(n_rows) == 1, n_rows > 0)
  }
  stopifnot(is.character(filename), length(filename) == 1)
  fun <- match.arg(fun)
  
  out <- raster(x)
  
  if (fun == "mean") {
    fun = rowMeans
  } else if (fun == "sum") {
    fun = rowSums
  }
  
  x <- readStart(x)
  out <- writeStart(out, filename = filename, ...)
  b <- make_block(out, n = 2 * (nlayers(x) + 1),
                  chunksize = chunksize, n_rows = n_rows)
  
  message(paste("Using", b$n, "blocks of", b$n_rows[1], "rows."))
  
  for (i in seq_along(b$start_row)) {
    v <- getValues(x, row = b$start_row[i], nrows = b$n_rows[i])
    v <- fun(v, na.rm = TRUE)
    out <- writeValues(out, v, b$start_row[i])
  }
  out <- writeStop(out)
  x <- readStop(x)
  return(out)
}