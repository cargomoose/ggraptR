#Modal panel definitions

shinyBS::bsModal("modalExportOptions", "Export Options", "exportPlotCtl", size = "small",
                 uiOutput('fileHeightCtl'),
                 uiOutput('fileWidthCtl'),
                 uiOutput('fileDPICtl'),
                 uiOutput('fileTypeCtl'),
                 uiOutput('dlBtnPlot'))