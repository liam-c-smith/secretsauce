#' Helpful defaults for saving ggplot plots
#'
#' This is a wrapper around \code{ggplot2::ggsave()} which allows one to easily save plots
#' to the commonly used paper sizes of the ISO 216 A standard. "A4" is the default but
#' the full range - "A0" to "A10" - can be specified.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param path Path to save plot to (combined with filename).
#' @param page_size Page size to use. Must be one of "A0" through to "A10".
#' @param portrait The orientation of the page.
#' @param dpi Plot resolution.
#'
#' @export
ggsaver <- function(filename, plot = last_plot(), path = NULL,
                    page_size = "A4", portrait = FALSE, dpi = 300){

  size <- paste0("A", 0:10)

  if (!(page_size %in% size)){
    stop("`page_size` must be one of 'A0' through to 'A10'.")
  }

	dim_seq <- c(1189, 841, 594, 420, 297, 210, 148, 105, 74, 52, 37, 26)

	dim_tbl <- data.frame(size = size,
	                      dim1 = dim_seq[-1],
	                      dim2 = dim_seq[-length(dim_seq)])

	pg_dims <- dim_tbl[dim_tbl$size == page_size, ]

	if (portrait) {
		pg <- c(w = pg_dims$dim1, h = pg_dims$dim2)
	} else {
		pg <- c(w = pg_dims$dim2, h = pg_dims$dim1)
	}

	ggplot2::ggsave(filename, plot = plot, path = path,
	                width = pg["w"], height = pg["h"],
	                dpi = dpi, units = "mm")
}
