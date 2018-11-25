ggsaver <- function(filename, plot = last_plot(), path = NULL,
					page_size = "A4", portrait = FALSE, dpi = 300){

	dim_seq <- c(1189, 841, 594, 420, 297, 210, 148, 105, 74, 52, 37, 26)

	dim_tbl <- data.frame(size = paste0("A", 0:10),
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
