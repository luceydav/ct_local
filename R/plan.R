the_plan <-
  
  drake_plan(
    mfi_raw = get_mfi(),
    mfi_cleaned = clean_mfi(mfi_raw),
    mfi_pension = load_pension(),
    mfi_opeb = load_opeb(),
    save_mfi = saveRDS(list(mfi_cleaned, mfi_pension, mfi_opeb),
              file_out("data/ct_mfi_DT.RDS")),
    yankee = build_yankee(file_in("data/ct_mfi_DT.RDS")),
    save_yankee = saveRDS(yankee, file_out("data/yankee.RDS")),
    deployment = rsconnect::deployApp(
      appFiles = file_in(
        "data/yankee.RDS",
        "app.R",
        "R/map_score.R"
        ),
      appName = "yankee_shiny",
      forceUpdate = TRUE
    )
)
