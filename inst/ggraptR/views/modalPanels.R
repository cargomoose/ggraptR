div(
  
  bsModal("modalUploadOptions", "Upload dataset", "uploadDataCtrl",
          source('./views/addDatasetCtrlsUI.R', local=TRUE)$value),
  
  bsModal("modalDownloadPlotOptions", "Plot download options", "downloadPlotCtrl", 
          size = "small",
          uiOutput('fileHeightCtrl'), uiOutput('fileWidthCtrl'),
          uiOutput('fileDPICtrl'), uiOutput('fileTypeCtrl'), uiOutput('modalDlBtnPlot')),
  
  bsModal("datasetOptions", "Dataset options", "datasetOptionsCtrl", size = "small", 
          numericInput(
            "nCatUniqVals", value=6, min=0,
            label = "Count of unique values to treat numeric feature as categorical")),
  
  bsModal("pTypesWarnModal", "Plot type warning", "pTypesWarnBtnCtrl", size = "small", 
          textOutput('pTypesWarnModalMessageCtrl'))

)
