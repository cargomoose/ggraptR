# suppressWarnings(rm(plotLog, p, envir=.GlobalEnv))

state <- list(txtCfg="")
state$gDefaultDataFrame <- if (exists('gDefaultDataFrame')) gDefaultDataFrame else ""
# rm(list=setdiff(ls(), "state"))

const <- list(gcnFileWidthDefault=10,
              gcnFileHeightDefault=10,
              gcnFileDPIDefault=100,
              gcnFileWidthMax=50,
              gcnFileHeightMax=50,
              gcnFileDPIMax=500)
