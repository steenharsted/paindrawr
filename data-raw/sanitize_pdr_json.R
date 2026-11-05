# Strip identifying metadata from PainDraw JSON files, in place.
# Sourced by the dataset scripts in data-raw/ so new files can be dropped into
# data-raw/<dataset>/ and are cleaned before they are committed.
#
# Whitelist approach: only fields known to be non-identifying are kept, so
# unexpected new fields in future exports are dropped rather than published.
#   - id  is replaced by the file name stem (e.g. "3_palmar")
#   - ts  and crc are set to null
#   - everything else not listed below is removed

sanitize_pdr_json <- function(files) {
  top_keep <- c(
    "v",
    "id",
    "w",
    "h",
    "units",
    "tpl",
    "tpl_md5",
    "view",
    "app",
    "ts",
    "crc",
    "s"
  )
  stroke_keep <- c(
    "strokeId",
    "drawInputType",
    "tool",
    "color",
    "lineWidth",
    "sprayDensity",
    "sprayRadius",
    "alpha",
    "scaleFactor",
    "dpr",
    "p"
  )

  for (f in files) {
    x <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    x <- x[intersect(names(x), top_keep)]
    x$id <- tools::file_path_sans_ext(basename(f))
    x["ts"] <- list(NULL)
    x["crc"] <- list(NULL)
    x$s <- lapply(x$s, \(s) s[intersect(names(s), stroke_keep)])
    jsonlite::write_json(
      x,
      f,
      auto_unbox = TRUE,
      null = "null",
      digits = NA,
      pretty = FALSE
    )
  }
  invisible(files)
}
