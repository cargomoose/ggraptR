div(
  
  bsModal("modalUploadOptions", "Upload dataset", "uploadDataCtrl",
          source('./views/addDatasetCtrlsUI.R', local=TRUE)$value),
  
  bsModal("modalDownloadPlotOptions", "Plot download options", "downloadPlotCtrl", 
          size = "small",
          uiOutput('fileHeightCtrl'), uiOutput('fileWidthCtrl'),
          uiOutput('fileDPICtrl'), uiOutput('fileTypeCtrl'), uiOutput('modalDlBtnPlot'))
  
)
