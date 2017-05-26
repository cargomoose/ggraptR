div(
  
  bsModal("modalUploadOptions", "Upload dataset", "uploadDataCtrl",
          source('./views/addDatasetCtrlsUI.R', local=TRUE)$value),
  
  bsModal("modalDownloadPlotOptions", "Plot download options", "downloadPlotCtl", 
          size = "small",
          uiOutput('fileHeightCtl'), uiOutput('fileWidthCtl'),
          uiOutput('fileDPICtl'), uiOutput('fileTypeCtl'), uiOutput('modalDlBtnPlot'))

)
