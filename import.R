
## code used to transform the ICASA Dictionary to the "vocal" format

.icasa_variables <- function(x) {
	names(x)[1] <- "name"
	x$description <- x$Description
	x$valid_min <- ifelse(is.na(x$MinVal), "", x$MinVal)
	x$valid_max <- ifelse(is.na(x$MaxVal), "", x$MaxVal)
	x$Description <- x$MinVal <- x$MaxVal <- NULL

	x$unit <- gsub("text|number|code", "", x$Unit_or_type)
	x$unit <- gsub("arc_degrees", "degrees", x$unit)
	x$unit <- ifelse(is.na(x$unit), "", x$unit)
	x$type <- x$Data_type
	x$type <- gsub("single", "numeric", x$type)
	x$type[x$Unit_or_type == "number"] <- "numeric"
	x$type[x$Unit_or_type %in% c("date", "code")] <- "character"
	x$type <- gsub("memo|text", "character", x$type)
	x$vocabulary <- ""

	i <- na.omit(match(c("name", "type", "unit", "vocabulary", "valid_min", "valid_max", "description"), names(x)))
	data.frame(x[, i], x[,-i])
}

.icasa_values <- function(x, code, split=NULL) {
	x <- data.frame(name=x[[code]], x)
	x[[code]] <- NULL
	x <- x[!is.na(x$name), ]
	if (!is.null(split)) {
		x <- split(x, x[[split]])
		names(x) <- gsub(" ", "_", trimws(gsub(", factors", "", tolower(names(x)))))
	} 
	x
}

.set_voc <- function(x, nms) {
	for (i in 1:length(nms)) {
		m <- grep(nms[i], x$Code_Display, ignore.case=TRUE)
		x$vocabulary[m] <- nms[i]
	}
	m <- grep("CH_TARGETS", x$Code_Display, ignore.case=TRUE)
	x$vocabulary[m] <- "ch_targets"
	m <- grep("CRID", x$Code_Display, ignore.case=TRUE)
	x$vocabulary[m] <- "crop"
	x
}

.write_split <- function(x, pval) {
	lapply(names(x), \(name) write.csv(x[[name]], file.path(pval, paste0("values_", name, ".csv")), row.names=FALSE))
}

.ICASA <- function(filename, outpath) {
	pvar <- file.path(outpath, "variables")
	pval <- file.path(outpath, "values")
	dir.create(pvar, FALSE, TRUE)
	dir.create(pval, FALSE, TRUE)

	sheets <- readxl::excel_sheets(filename)
	d <- lapply(sheets, \(s) readxl::read_excel(filename, sheet=s))
	names(d) <- sheets
	
	meta_vars <- .icasa_variables(d[["Metadata"]])
	mngt_vars <- .icasa_variables(d[["Management_info"]])
	soil_vars <- .icasa_variables(d[["Soils_data"]])
	meas_vars <- .icasa_variables(d[["Measured_data"]])
	wth_vars <- .icasa_variables(d[["Weather_data"]])

	meta_vals <- .icasa_values(d[["Metadata_codes"]], "Code", split="Code_display")#
	mngt_vals <- .icasa_values(d[["Management_codes"]], "Code", split="Group.Topic")
	othr_vals <- .icasa_values(d[["Other_codes"]], "Code", split="Variable")
	pest_vals <- .icasa_values(d[["Pest_codes"]], "Pest_code")
	pest_vals <- pest_vals[, 1:which(names(pest_vals)=="ICASA_standard")]
	crop_vals <- .icasa_values(d[["Crop_codes"]], "Crop_code")

	valnms <- c(names(meta_vals), names(mngt_vals), names(othr_vals))

	meta_vars <- .set_voc(meta_vars, valnms)
	mngt_vars <- .set_voc(mngt_vars, valnms)
	soil_vars <- .set_voc(soil_vars, valnms)
	meas_vars <- .set_voc(meas_vars, valnms)
	wth_vars <- .set_voc(wth_vars, valnms)

	write.csv(meta_vars, file.path(pvar, "variables_metadata.csv"), row.names=FALSE)
	write.csv(mngt_vars, file.path(pvar, "variables_management.csv"), row.names=FALSE)
	write.csv(soil_vars, file.path(pvar, "variables_soil.csv"), row.names=FALSE)
	write.csv(wth_vars, file.path(pvar, "variables_weather.csv"), row.names=FALSE)
	write.csv(meas_vars, file.path(pvar, "variables_measure.csv"), row.names=FALSE)

	.write_split(meta_vals, pval)
	.write_split(mngt_vals, pval)
	.write_split(othr_vals, pval)
	write.csv(pest_vals, file.path(pval, "values_pests.csv"), row.names=FALSE)
	write.csv(crop_vals, file.path(pval, "values_crop.csv"), row.names=FALSE)

	writeLines(paste(unlist(d[["ReadMe"]]), collapse="\n\n"), file.path(outpath, "readme.txt"))
	write.csv(d[["Glossary"]], file.path(outpath, "glossary.csv"), row.names=FALSE)

}


testicasa <- function() {
	filename <- "C:/github/carob/ICASA Data Dictionary.xlsx"
	outpath <- "d:/icasa"
	.ICASA(filename, outpath)
}

#testicasa()