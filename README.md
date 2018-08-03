# readgbd

Read Graphtech Binary Data file (*.gbd).

Imperimentation of [GL220 / GL820 GBD File Specification Sheet](http://produkte.althen.de/public/media/PDF_Manual/4b_Messdatenerfassung/de/lv-GL220-820-GBD-File-Specification-Sheet.pdf)

## Install

```
devtools::install_github("whatalnk/readgbd")
```

## Usage

```
gbd_data <- read_gbd(path_to_gbd_data)
header <- gbd_data[["header"]]
data <- gbd_data[["data"]]
```


## Notes

### Devices

* GL220

### milliseconds

* Depends on OS
* `options(digits.secs=n)`, `%OSn`
