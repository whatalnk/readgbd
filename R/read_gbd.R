#' read gbd file
#'
#' @param file Path to gbd data
#' @return list of header (list) and data (tibble)
#' @examples
#' gbd_data <- read_gbd(path_to_gbd_data)
#' header <- gbd_data[["header"]]
#' data <- gbd_data[["data"]]
#' @export
read_gbd <- function(file){
  con <- file(file, "rb")
  header.str <- read_header(con)
  header <- parse_header(header.str)
  data <- read_data(con, header)
  close(con)

  data_ <- data %>%
    mutate(DateTime = get_datetime_column(header)) %>%
    select(DateTime, everything()) %>%
    convert_data(header)
  ret <- list(
    header = header,
    data = data_
  )
  ret
}


#' read header region
#'
#' @param con file connection
#' @return character
#' @keywords internal
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
#' @param header.str header reagion read by read_header()
#' @return list
#' @keywords internal
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
#' @param con file connection
#' @param header header
#' @return tibble
#' @keywords internal
read_data <- function(con, header){
  n_row <- as.integer(header$Common$Data$Counts)
  print(str_glue("Number of measurement: {n_row}"))
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
#' @keywords internal
get_datetime_column <- function(header){
  time.start <- parse_date_time(header$Measure$Time$Start, orders = "%Y-%m-%d, %H:%M:%S")
  time.stop <- parse_date_time(header$Measure$Time$Stop, orders = "%Y-%m-%d, %H:%M:%S")
  time.trigger <- parse_date_time(header$Measure$Time$Trigger, orders = "%Y-%m-%d, %H:%M:%S")
  print(str_glue("Start: {time.start}"))
  print(str_glue("End: {time.stop}"))
  print(str_glue("Trigger: {time.trigger}"))
  s <- header$Common$Data$Sample
  n_row <- as.integer(header$Common$Data$Counts)
  ret <- seq(time.trigger, time.trigger + (n_row - 1) * sampling_interval(s), by = sampling_interval(s))
  ret
}
#' get sampling interval
#'
#' @importFrom lubridate milliseconds seconds minutes hours
#' @param s header$Common$$Data > Sample
#' @return Period
#' @keywords internal
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

#' value represent overflow
#'
#' upper: 110% of upper full scale (+7FCC)
#' @keywords internal
GBD_OF_V_U <- 32764

#' value represent overflow
#'
#' lower: -110% of lower full scale (-7FFF)
#' @keywords internal
GBD_OF_V_L <- -32767

#' convert data
#'
#' @param data data
#' @param header header
#' @return tibble
#' @keywords internal
convert_data <- function(data, header){
  ret <- colnames(data) %>% set_names() %>%
    map(~ convert_variable(.x, data[[.x]], header)) %>%
    as_data_frame()
}

#' convert each variables
#'
#' @param ch channel name
#' @param v variable; column of data
#' @param header header
#' @return atomic vector
#' @keywords internal
convert_variable <- function(ch, v, header){
  if (!str_detect(ch, "^CH")) {
    ret <- v
  } else {
    amp <- parse_amp(header$Amp, ch)
    if (amp["input"] == "DC") {
      range <- parse_range(amp[["range"]])
      span <- parse_span_ad(header$Measure$Span[[ch]], range)
      ret <- convert_voltage(v, range)
      unit <- range$unit
    } else if (amp["input"] == "TEMP") {
      span <- parse_span_temp(header$Measure$Span[[ch]])
      ret <- convert_temperature(v, span)
      unit <- header$Common$Data$TempUnit
    } else {
      span <- c(0, 0)
      ret <- v
      unit <- "Unknown"
    }
    print(str_glue("Span for {ch}: {span[1]} -- {span[2]} {unit}"))
  }

  ret
}

#' convert voltage value
#'
#' @param v column of data which stores voltage
#' @param range returned value of parse_range()
#' @return numeric
#' @keywords internal
convert_voltage <- function(v, range){
  p1 <- v >= GBD_OF_V_U
  p2 <- v <= GBD_OF_V_L
  v_ <- as.numeric(v)
  ret <- if_else(p1 | p2, NA_real_, v_)

  # Conversion of Voltage Values
  if (range[["base"]] == 1){
    ret <- ret / 2
  } else if (range[["base"]] == 2) {
    ret <- ret / 1
  } else if (range[["base"]] == 5){
    ret <- ret / 4
  }

  # Conversion of the Decimal Point Position
  if (range[["range"]] < 50) {
    ret <- ret / 1000
  } else if (range[["range"]] < 500) {
    ret <- ret / 100
  } else if (range[["range"]] < 5000) {
    ret <- ret / 10
  } else if (range[["range"]] < 50000) {
    ret <- ret / 1
  } else {
    ret <- ret * 10
  }
  if (range[["unit"]] == "V"){
    ret <- ret / 1000
  }

  ret
}

#' convert temperature value
#'
#' @param v column of data which stores temperature
#' @param span returned value of parse_span()
#' @return numeric
#' @keywords internal
convert_temperature <- function(v, span){
  v_ <- v / 10
  p1 <- v_ > span$upper
  p2 <- v_ < span$lower
  ret <- if_else(p1 | p2, NA_real_, v_)
  ret
}

#' parse Amp
#'
#' offset: undocumented
#' @param Amp header$Amp
#' @param CH Channel
#' @return named character
#' @keywords internal
parse_amp <- function(Amp, CH){
  ret <-  Amp[[CH]] %>%
    str_split(pattern=",") %>%
    .[[1]] %>%
    str_trim(side="both") %>%
    set_names(c("type", "input", "range", "filter", "thermocouple", "offset"))
  ret
}

#' parse range of Amp
#'
#' @param range header$Amp$[[CH]]
#' @return list
#' @keywords internal
parse_range <- function(range){
  if (str_detect(range, "mV")) {
    v_unit <- "mV"
    n <- str_replace(range, "mV", "") %>% as.integer()
    v_base <- get_base(n)
    v_range <- n
  } else if (str_detect(range, "V")){
    if (str_detect(range, "-")) {
      v_base <- 5
      v_range <- v_base * 1000
    } else {
      n <- str_replace(range, "V", "") %>% as.integer()
      v_base <- get_base(n)
      v_range <- n * 1000
    }
    v_unit <- "V"
  }

  list(
    base = v_base,
    range = v_range,
    unit = v_unit
  )
}

#' get base
#'
#' @param n integer
#' @return numeric
#' @keywords internal
get_base <- function(n){
  while (n %/% 10 != 0) {
    n <- n %/% 10
  }
  n
}

#' parse span
#'
#' @param span header$Measure$Span[[CH]]
#' @return numeric
#' @keywords internal
parse_span <- function(span){
  ret <- str_split(span, ",") %>%
    .[[1]] %>%
    str_trim() %>%
    as.numeric()
  ret
}

#' parse span ad
#'
#' @param span header$Measure$Span[[CH]]
#' @param range returned value of parse_range()
#' @return list
#' @keywords internal
parse_span_ad <- function(span, range){
  span_ <- parse_span(span)
  span_ad <- convert_voltage(span_[1:2], range)

  list(
    lower = span_ad[1],
    upper = span_ad[2]
  )
}

#' parse span temp
#'
#' @param span header$Measure$Span[[CH]]
#' @return list
#' @keywords internal
parse_span_temp <- function(span){
  span_ <- parse_span(span)
  span_temp <- span_ / 10

  list(
    lower = span_temp[1],
    upper = span_temp[2]
  )
}
