div(
  
  bsModal("modalUploadOptions", "Upload dataset", "uploadDataCtrl",
          source('./views/addDatasetCtrlsUI.R', local=TRUE)$value),
  
  bsModal("modalExportOptions", "Export Options", "exportPlotCtl", size = "small",
          uiOutput('fileHeightCtl'),
          uiOutput('fileWidthCtl'),
          uiOutput('fileDPICtl'),
          uiOutput('fileTypeCtl'),
          uiOutput('dlBtnPlot')),
  
  bsModal("modalCodeV", "Generated Code", "generatePlotCodeCtl",
          textOutput('generateCode'))

)
