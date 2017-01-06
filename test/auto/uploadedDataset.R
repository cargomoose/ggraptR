# Run this script with 'testthat::test_file(paste0(getwd(), '/test/auto/uploadDataset.R'))'

source('script/checkInitPlot.R')

driver %>% getEl('a[data-value="importTab"]') %>% click()
rootEl <- waitFor('#fileInputSelectCtrl', driver, timeout = 3)
uplEl <- rootEl %>% getEl('input#file')



# uplEl$setElementAttribute('style', '')
print('111')
browser()

# jsScript <- '
#   var input = $("input#file")[0];
#   input.style = "";
#   input.className = "deanclass";
#   input.parentElement.style = "";
#   input.parentElement.parentElement.parentElement.className = "";'
# driver$executeScript(jsScript, args = list())

uplEl$sendKeysToElement(list(
  # 'C:\\GoogleDrive\\dev\\R\\ggraptR\\test\\auto\\data\\charData.csv',
  # 'C:/GoogleDrive/dev/R/ggraptR/
  'test/auto/data/charData.csv',
  # 'test/auto/data/charData.csv',
  # 'charData.csv',
  key = "enter"))
driver$executeScript('$("input#file")[0].msg = $("input#file")[0].files.length', args = list())
print(uplEl %>% attr('msg'))

browser()



#file_progress %>%  text == "Upload complete"

#### scatter -> test inputs -> release resources ####
getSelectOptions(driver, 'plotTypes') %>% 
  Filter(function(el) attr(el, 'data-value') == 'scatter', .) %>% `[[`(1) %>% 
  click()

source('script/checkInputs.R')

stopExternals(driver, selServer)
