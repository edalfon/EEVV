# data/

Raw EEVV data is **not** included in this repo (see `data/.gitignore`,
which ignores `.zip`/`.xml`/`.pdf`/`.docx` under `EEVV/`) and must be
downloaded from DANE yourself:
<https://microdatos.dane.gov.co>.

Each `data/EEVV/<period>/` folder has a `.url` shortcut (and sometimes a
`readme.txt`) pointing to the exact DANE catalog page/download link used
for that period. Download the Nacimientos/Defunciones `.sav` zip files
and DDI `.xml` codebook from there, and keep the same folder layout
DANE ships them in -- the file paths are hard-coded in
`R/ingest_eevv_nac.R`, `R/ingest_eevv_defun_fetal.R`, and
`R/ingest_eevv_defun_nofetal.R`.

DANE's terms of use prohibit redistributing their data to multiple users
without prior written approval from DANE -- this applies to the raw
files above, and equally to this code's *output* (the compiled/
homologated `nac`/`defun_fetal`/`defun_nofetal` datasets). So neither
the raw data nor the processed output can be shared publicly (e.g.
committed to this repo, or published elsewhere): each user needs to
download the raw data from DANE themselves and run the pipeline to get
the processed datasets.
