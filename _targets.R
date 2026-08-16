tar_option_set(
  packages = c("daner"),
  imports = c("daner")
)

list(
  plan_ingest_eevv(),
  plan_ingest_nac(),
  # flowme::tar_bookdown("report"),
  NULL
)
