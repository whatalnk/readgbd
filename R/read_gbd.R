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

  data_ <- data %>%
    mutate(DateTime = get_datetime_column(header))
  ret <- list(
    header = header,
    data = data_
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

#' get datetime column
#'
#' get datetime column based on Stop Time (header$Measure$$Time > Stop) and Trigger Time ($Measure$$Time > Trigger). tz= "UTC"
#'
#' @importFrom lubridate parse_date_time
#' @param header header
#' @return POSIXct
get_datetime_column <- function(header){
  time.stop <- parse_date_time(header$Measure$Time$Stop, orders = "%Y-%m-%d, %H:%M:%S")
  time.trigger <- parse_date_time(header$Measure$Time$Trigger, orders = "%Y-%m-%d, %H:%M:%S")
  s <- header$Common$Data$Sample
  ret <- seq(time.trigger, time.stop, by = sampling_interval(s))
  ret
}
#' get sampling interval
#'
#' @importFrom lubridate milliseconds seconds minutes hours
#' @param s header$Common$$Data > Sample
#' @return Period
sampling_interval <- function(s){
  if (str_detect(s, "ms")){
    num <- str_replace(s, "ms", "") %>% as.integer()
    ret <- milliseconds(num)
  } else if (str_detect(s, "s")){
    num <- str_replace(s, "s", "") %>% as.integer()
    ret <- seconds(num)
  } else if (str_detect(s, "min")){
    num <- str_replace(s, "min", "") %>% as.integer()
    ret <- minutes(num)
  } else if (str_detect(s, "h")) {
    num <- str_replace(s, "h", "") %>% as.integer()
    ret <- hours(num)
  }
  ret
}

#' Value represent overflow
#'
#' Upper: 110% of upper full scale (+7FCC)
GBD_OF_V_U <- 32764

#' Value represent overflow
#'
#' Lower: -110% of lower full scale (-7FFF)
GBD_OF_V_L <- -32767

#' Convert voltage value
#'
convert_voltage <- function(v, ch, header){
  p1 <- v >= GBD_OF_V_U
  p2 <- v <= GBD_OF_V_L
  v_ <- if_else(p1 | p2, NA_real_, v)
  range <- header$Amp[[ch]]
}

#' Convert temperature value
#'
convert_temperature <- function(x, header){

}

#' parse Amp
#'
#' offset: undocumented
#' @importFrom rlang set_names
parse_amp <- function(header, CH){
  ret <-  header$Amp[[CH]] %>%
    stringr::str_split(pattern=",") %>%
    .[[1]] %>%
    stringr::str_trim(side="both") %>%
    rlang::set_names(c("type", "input", "range", "filter", "thermocouple", "offset"))
  ret
}

#' parse range of Amp
#'
#'
parse_range <- function(){

}

