# EEVV

Ingests, homologates, and compiles Colombia's DANE "Estadísticas Vitales"
(EEVV) vital statistics microdata -- Nacimientos (births), Defunciones
Fetales (fetal deaths), and Defunciones No Fetales (general deaths) -- into
harmonized, analysis-ready parquet tables. Variable coding is inconsistent
across years, so each dataset's variables are individually homologated
using DDI metadata to recover labels where the raw files lack them.

## Data

Raw data is not included in this repo, and neither is this code's
processed output: DANE's terms of use prohibit redistributing their data
to multiple users without DANE's prior written approval, so this applies
to both the raw microdata and the compiled/homologated datasets this
pipeline produces. You need to download the yearly `.sav` microdata files
(and DDI `.xml` codebooks) from DANE
(<https://microdatos.dane.gov.co>, under Estadísticas Vitales) yourself,
place them under `data/EEVV/<period>/` (see `data/README.md`), matching
the paths hard-coded in `R/ingest_eevv_nac.R`,
`R/ingest_eevv_defun_fetal.R`, and `R/ingest_eevv_defun_nofetal.R`, and
run the pipeline to get the processed datasets.

Any use of DANE's data requires this citation:

> Fuente: Departamento Administrativo Nacional de Estadística - DANE:
> www.dane.gov.co

## Running the pipeline

```r
renv::restore()   # install dependencies pinned in renv.lock
targets::tar_make()
```

Final outputs (`nac`, `defun_fetal`, `defun_nofetal`) are written as
parquet; read them with `targets::tar_read(nac)`, etc.

## Origin

This code was refactored out of a larger codebase originally written for
the Atlas project.
