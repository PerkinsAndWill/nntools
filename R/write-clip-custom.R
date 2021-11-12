#' Writes to Clipboard with Header
#'
#' @param tbl Tibble or data frame for writing out to clipboard
#' @param desc Optional description
#' @param ... Optional additional parameters to be passed to internal functions
#'
#' @return Writes table to your clipboard
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
write_clip_with_header = function(tbl, desc = NULL, ...){

  object_type = "character"
  breaks = NULL
  eos = NULL
  return_new = FALSE

  envir_vars = Sys.getenv()

  header_content = paste0(
    "User: ",envir_vars["USERNAME"]," Timestamp: ",Sys.time(),
    " Clipped From: ", rstudioapi::getSourceEditorContext()$path
  )

  line_tbl = tbl %>%
    tidyr::unite(line,tidyselect::everything(),sep = "\t")

  string_tbl = line_tbl %>%
    dplyr::summarise(all_lines = paste(.data$line,collapse = "\n")) %>%
    dplyr::pull(.data$all_lines)

  out_tbl = paste0(header_content,"\n",
                   {if(!is.null(desc)){paste0("Description: ",desc,"\n")}},
                   "\n",
                   paste(names(tbl),collapse = "\t"),"\n",
                   string_tbl)

  sys.type <- Sys.info()["sysname"]
  chosen_write_clip <- switch(sys.type, Darwin = osx_write_clip,
                              Windows = win_write_clip, X11_write_clip)
  invisible(chosen_write_clip(out_tbl, object_type, breaks,
                              eos, return_new, ...))
}

# Helper function to write to the Windows clipboard
win_write_clip <- function(content, object_type, breaks, eos, return_new, ...) {

  .dots <- list(...)

  # If no custom line separator has been specified, use Windows's default
  # newline character '\r\n'
  breaks <- ifelse(is.null(breaks), '\r\n', breaks)

  # If no custom tab separator for tables has been specified, use Windows's
  # default tab character: '\t'
  .dots$sep <- ifelse(is.null(.dots$sep), '\t', .dots$sep)

  # Pass the object to rendering functions before writing out to the clipboard
  rendered_content <- render_object(content, object_type, breaks, .dots)
  utils::writeClipboard(rendered_content, format = 1)
  if (return_new) {
    rendered_content
  } else {
    content
  }
}

# Helper function to write to the OS X clipboard
# Adapted from https://github.com/jennybc/reprex/blob/master/R/clipboard.R
osx_write_clip <- function(content, object_type, breaks, eos, return_new, ...) {
  .dots <- list(...)
  con <- pipe("pbcopy")

  write_nix(content, object_type, breaks, eos, return_new, con, .dots)
}

# The same content rendering and writing steps are used in both OS X and Linux,
# just with different connection objects
write_nix <- function(content, object_type, breaks, eos, return_new, con, .dots) {
  # If no custom line separator has been specified, use Unix's default newline
  # character '\n'
  breaks <- ifelse(is.null(breaks), '\n', breaks)

  # If no custom tab separator for tables has been specified, use Unix's default
  # tab character: '\t'
  .dots$sep <- ifelse(is.null(.dots$sep), '\t', .dots$sep)

  # Pass the object to rendering functions before writing out to the clipboard
  rendered_content <- render_object(content, object_type, breaks, .dots)

  # Suppress pipe() warning when writing an empty string with a NULL string
  # ending.
  if (identical(rendered_content, "")) {
    suppressWarnings(writeChar(rendered_content, con = con, eos = eos))
  } else {
    writeChar(rendered_content, con = con, eos = eos)
  }

  close(con)

  if (return_new) {
    rendered_content
  } else {
    content
  }
}

# Determine if a given utility is installed AND accessible
# Takes a character vector whose first element is the name of the
# utility executable and whose subsequent elements are command-line
# arguments to the utility for the test run.
has_util <- function(util_test) {
  if (nzchar(Sys.which(util_test[1]))) {
    # If utility is accessible, check that DISPLAY can be opened.
    try_res <- tryCatch(system2(util_test[1], util_test[-1], stdout = TRUE, stderr = TRUE),
                        error = function(c) FALSE,
                        warning = function(c) FALSE)

    # In the case of an error/warning on trying the function, then the util is
    # not available
    if (identical(try_res, FALSE)) {
      notify_no_display()
    } else {
      TRUE
    }
  } else {
    FALSE
  }
}

# Determine if system has 'xclip' installed AND it's accessible
has_xclip <- function() has_util(c("xclip", "-o", "-selection", "clipboard"))

# Determine if system has 'xsel' installed
has_xsel <- function() has_util(c("xsel", "--clipboard", "--output"))

# Stop read/write and return an error of missing clipboard software.
notify_no_cb <- function() {
  stop(msg_no_clipboard(), call. = FALSE)
}

notify_no_display <- function() {
  stop(msg_no_display(), call. = FALSE)
}

# Helper function to read from the X11 clipboard
#
# Requires the utility 'xclip' or 'xsel'. This function will stop with an error
# if neither is found. Adapted from:
# https://github.com/mrdwab/overflow-mrdwab/blob/master/R/readClip.R and:
# https://github.com/jennybc/reprex/blob/master/R/clipboard.R
X11_read_clip <- function() {
  if (has_xclip()) {
    con <- pipe("xclip -o -selection clipboard")
  } else if (has_xsel()) {
    con <- pipe("xsel --clipboard --output")
  } else {
    notify_no_cb()
  }
  content <- scan(con, what = character(), sep = "\n",
                  blank.lines.skip = FALSE, quiet = TRUE)
  close(con)
  return(content)
}

# Helper function to write to the X11 clipboard
#
# Requires the utility 'xclip' or 'xsel'. This function will stop with an error
# if neither is found. Adapted from
# https://github.com/mrdwab/overflow-mrdwab/blob/master/R/writeClip.R
#
# Targets "primary" and "clipboard" clipboards if using xclip, see:
# http://unix.stackexchange.com/a/69134/89254
X11_write_clip <- function(content, object_type, breaks, eos, return_new, ...) {
  if (has_xclip()) {
    con <- pipe("xclip -i -sel p -f | xclip -i -sel c", "w")
  } else if (has_xsel()) {
    con <- pipe("xsel --clipboard --input", "w")
  } else {
    notify_no_cb()
  }

  .dots <- list(...)

  write_nix(content, object_type, breaks, eos, return_new, con, .dots)
}

# Check object type to determine if it will be handled as a simple table or as a
# character vector
render_object <- function(content, object_type, breaks, .dots) {
  if (object_type == "auto")
    object_type <- eval_object(content)
  switch(object_type,
         "table" = table_str(content, breaks, .dots),
         "character" = flat_str(content, breaks))
}


eval_object <- function(content) {
  ifelse(is.data.frame(content) | is.matrix(content), "table", "character")
}

# If object is a table, default to a multiline string with tab separators
table_str <- function(content, breaks, .dots) {
  # Take the system-specific collapse out of the list
  .dots$x <- content
  .dots$sep <- .dots$sep
  .dots$quote <- ifelse(is.null(.dots$quote), FALSE, .dots$quote)
  .dots$na <- ifelse(is.null(.dots$na), "", .dots$na)
  .dots$col.names <- ifelse(is.null(.dots$col.names), !is.null(colnames(content)), .dots$col.names)

  # Check if dataframe rownames are anything different than the default numbered names
  numbered_rownames <- all(rownames(content) == as.character(seq_along(rownames(content))))

  .dots$row.names <- ifelse(is.null(.dots$row.names), ifelse(numbered_rownames, FALSE, !is.null(rownames(content))), .dots$row.names)

  # Writing to and reading from a temp file is much faster than using capture.output
  tbl_file <- tempfile()
  .dots$file = tbl_file
  do.call(utils::write.table, .dots)
  read_tbl <- paste0(readLines(tbl_file), collapse = breaks)
  unlink(tbl_file)

  # If row.names = TRUE and col.names = TRUE, add additional sep character to
  # the start of the table
  if (.dots$row.names & .dots$col.names) {
    read_tbl <- paste0(.dots$sep, read_tbl)
  }

  return(read_tbl)
}

# Helper function to flatten content into 1-tuple character vector (i.e. a
# string)
flat_str <- function(content, breaks) {
  if (typeof(content) != "character") {
    warning("Coercing content to character")
    content <- as.character(content)
  }

  if (length(content) < 1) {
    content <- ""
  } else if (length(content) > 1) {
    content <- paste0(content, collapse = breaks)
  } else if (is.na(content)) {
    content <- "NA"
  }

  return(content)
}

msg_no_clipboard <- function() "Clipboard on X11 requires 'xclip' (recommended) or 'xsel'."

msg_no_display <- function() "Clipboard on X11 requires that the DISPLAY envvar be configured."

# write_clip_with_header(airports, "Airports test")
#
# write_clip()
