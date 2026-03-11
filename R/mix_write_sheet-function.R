#' Enhanced write_sheet with override support
#' @export
mix_write_sheet <- function(data, ss, sheet = NULL, ...) {

  # Check for export bucket override first (higher priority)
  export_override <- Sys.getenv('MIXTAPE_EXPORT_BUCKET_OVERRIDE', default = "")
  if (export_override != "") {
    # Get script name from environment
    script_name <- Sys.getenv('CURRENT_SCRIPT_NAME', default = "unknown")
    sheet_name <- if (is.null(sheet)) "Sheet1" else sheet
    obj_name <- paste0(script_name, "_", sheet_name, "_")

    message(paste("Exporting to bucket:", export_override, "object:", obj_name))
    return(mix_gcs_data_upload(
      project = 'rekab-ds',
      df = data,
      object_name = obj_name,
      bucket = export_override
    ))
  }

  # Check for Google Sheets override
  gs_override <- Sys.getenv('MIXTAPE_GS_OVERRIDE_ID', default = "")
  if (gs_override != "") {
    ss <- googlesheets4::as_sheets_id(gs_override)
    message(paste("Redirecting write_sheet to override sheet:", gs_override))
  }

  # Default behavior - call original googlesheets4
  googlesheets4::write_sheet(data = data, ss = ss, sheet = sheet, ...)
}