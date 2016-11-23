# debugger configuration

# prepare txtCfg to read configuration file
txtCfg <- tryCatch(
  read.delim("debug/debug.cfg.json", 
             comment.char="#", quote="", header=F)$V1 %>% as.character,
  error = function(e) { "" })

# if txtCfg populated then logging is on, read extracted json record
if (nchar(txtCfg) > 0) {
  # read json configuration string
  jsonCfg <- fromJSON(txtCfg)
  
  if (jsonCfg$logs == "all") {
    flog.appender(appender.file('./debug/debug.log'), name='all')
    flog.threshold(DEBUG, name='all')  
  }
  
  if (jsonCfg$format == "simple") {
    flog.layout(layout.simple, name='all')
  }
  
  if (jsonCfg$format == "json") {
    flog.layout(layout.json, name='all')
  }
} else {
  # logging is off, only logs of level FATAL will be written. no such logs exist in the code,
  # so none will be written
  flog.threshold(FATAL)  
}

suppressWarnings(rm(txtCfg))
flog.debug("Debug logging initiated", name='all')
