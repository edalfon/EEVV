source("renv/activate.R")

# --- vscode-R session watcher on R >= 4.6 ---------------------------------
# vscode-R hooks the end of R's startup by shadowing .First.sys in globalenv
# (<ext>/R/session/init.R). R 4.6 stopped calling .First.sys from globalenv and
# calls base::.First.sys directly, so the watcher's init_last() is queued and
# never invoked: no `tools:vscode`, no .vsc.attach, and rstudioapi::isAvailable()
# returns FALSE. R still calls .First from globalenv, so drain the queue there.
#
# Requires jsonlite and rlang in the project library, or init.R declines to
# queue itself at all. No-op outside VS Code, so this is safe to commit.
# Upstream: https://github.com/REditorSupport/vscode-R/issues/1696
local({
  prev <- if (exists(".First", envir = globalenv(), inherits = FALSE)) {
    get(".First", envir = globalenv())
  }
  assign(
    ".First",
    function() {
      if (is.function(prev)) {
        try(prev())
      }
      if (exists(".First.sys", envir = globalenv(), inherits = FALSE)) {
        try(get(".First.sys", envir = globalenv())())
      }
    },
    envir = globalenv()
  )
})

options(conflicts.policy = "strict")

if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}

library(stats) # package 'stats' in options("defaultPackages") was not found
library(utils)
library(
  dplyr,
  mask.ok = c("filter", "lag", "intersect", "setdiff", "setequal", "union")
)
library(tidyr)
library(ggplot2)
library(targets)
library(tarchetypes)
library(haven) # needed to be able to convert haven_labelled to string or factor
library(testthat, mask.ok = "matches")
library(stringr)
# library(furrr)
# library(dtplyr)
# library(data.table, exclude = c("between", "first", "last"))

# make all fns available to {targets} (and interactively, via load_all)
if (rlang::is_interactive()) {
  devtools::load_all()
} else {
  # I do not remember anymore what was the problem of just doing load_all for targets
  # that led to this if statement. I think it is something related to the environments.
  # load_all puts everything some place else, while targets tracks the functions
  # in the global environment? is it that?, yeap, it seems to be that.
  # now, the problem I am facing is that, for functions like document or test
  invisible(
    lapply(list.files("./R", full.names = TRUE), source, encoding = "UTF-8")
  )
}

# filter(mtcars, mpg > 31)
