#' read gbd file
#'
#' @param file Path to gbd data
#' @return list
#' @export
read_gbd <- function(file){
  con <- file(file, "rb")
  header.str <- read_header(con)
  header <- parse_header(header.str)
  data <- read_data(con, header)
  close(con)
  ret <- list(
    header = header,
    data = data
  )
  ret
}


#' read header region
#'
#' @rdname read_header
#' @param con file connection
#' @return character
#' @export
read_header <- function(con){
  ret <- ""
  header_part <- readChar(con, 4096, useBytes = TRUE)
  ret <- str_c(ret, header_part)
  endheader <- str_detect(header_part, pattern=fixed("$EndHeader"))
  while (TRUE) {
    if (endheader) {
      break
    }
    header_part <- readChar(con, 2048, useBytes = TRUE)
    ret <- str_c(ret, header_part)
    endheader <- str_detect(header_part,pattern=fixed("$EndHeader"))
  }
  ret
}
#' parse header region
#'
#' @rdname parse_header
#' @param header.str header reagion read by read_header()
#' @return list
#' @export
parse_header <- function(header.str){
  header.line <- str_split(header.str, pattern="\r\n")
  ret <- list()
  section <- list()
  thesection <- ""
  subsection <- list()
  thesubsection <- ""
  subsubsection <- list()
  thesubsubsection <- ""
  sectionlevel <- 1
  for (line in header.line[[1]]) {
    if (stringi::stri_startswith_fixed(line, "$$$")){
      if (length(subsubsection) != 0){
        subsection[[thesubsubsection]] = subsubsection
        subsubsection <- list()
      }
      thesubsubsection <- str_replace(line, fixed("$$$"), "")
      sectionlevel <- 3
    } else if (stringi::stri_startswith_fixed(line, "$$")) {
      if (length(subsection) != 0){
        section[[thesubsection]] = subsection
        subsection <- list()
      }
      thesubsection <- str_replace(line, fixed("$$"), "")
      sectionlevel <- 2
    } else if (stringi::stri_startswith_fixed(line, "$")) {
      if (length(section) != 0){
        ret[[thesection]] = section
        section <- list()
      }
      thesection <- str_replace(line, fixed("$"), "")
      sectionlevel <- 1
    } else {
      key_and_val <- str_split_fixed(line, "=", 2)
      if (sectionlevel == 1){
        section[[str_trim(key_and_val[1])]] = str_trim(key_and_val[2])
      } else if (sectionlevel == 2) {
        subsection[[str_trim(key_and_val[1])]] = str_trim(key_and_val[2])
      } else {
        subsubsection[[str_trim(key_and_val[1])]] = str_trim(key_and_val[2])
      }
    }
  }
  ret
}

#' read data region
#'
#' @rdname read_data
#' @param con file connection
#' @param header header
#' @return tibble
#' @export
read_data <- function(con, header){
  n_row <- as.integer(header$Common$Data$Counts)
  col_names <- header$Common$Data$Order %>%
    {str_split(string =., pattern=",")} %>%
    {str_trim(string = .[[1]], side="both")}
  n_data <- n_row * length(col_names)
  data <- readBin(con, what=integer(), n=n_data, size=2L, signed=TRUE, endian="big")
  m <- matrix(data, ncol=4, byrow = TRUE)
  colnames(m) <- col_names
  ret <- as_data_frame(m)
  ret
}
