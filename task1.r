library(stringi)
dir <- "F://PvD//Studying//Programming//CTRRAssignment//CTRR_BTL"
setwd(dir)
dataRaw <- read.csv2("owid-covid-data.csv", header = TRUE, sep = ",")

# Years of research
  Years <- {}
  curYear <- "2000"
  cntYear <- 0
  iCountry <- dataRaw$iso_code[1]
  
  for (i in 1 : 1000)
  {
    if (iCountry != dataRaw$iso_code[i])
      break;
    if (curYear != stri_sub(dataRaw$date[i], -4))
    {
      cntYear = cntYear + 1
      Years[cntYear] = stri_sub(dataRaw$date[i], -4)
      curYear = Years[cntYear]
    }
  }
  write.table(Years, file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
  
  

# Print first 10 countries
  
  cntCountry <- 0
  curCountry <- "0"
  Country <- {}
  Iso_code <- {}
  for (i in 1 : (163*2))
  {
    if (curCountry != dataRaw$iso_code[i*500])
    {
      cntCountry = cntCountry + 1
      curCountry = dataRaw$iso_code[i*500]
      Country[cntCountry] = dataRaw$location[i*500]
      Iso_code[cntCountry] = curCountry
    }
    if (cntCountry == 10)
      break
  }
  list_first10Country <- data.frame(Iso_code, Country)
  write.table(list_first10Country, file = "task1.csv", row.names = FALSE, quote = FALSE, sep = ",", append = TRUE)
  
# Counting number of Countries
  
  cntCountry <- 1
  curCountry <- "AFG"
  for (i in 1 : (163*2))
  {
    if (curCountry != dataRaw$iso_code[i*500])
    {
      cntCountry = cntCountry + 1
      curCountry = dataRaw$iso_code[i*500]
    }
  }
  write.table(cntCountry, file = "task1.csv", row.names = FALSE, col.names = FALSE, append = TRUE)
  
