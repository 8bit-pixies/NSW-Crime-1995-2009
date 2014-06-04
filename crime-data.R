# Recorded Crime Dataset (NSW)
# 
# **Chapman Siu**
# 
# Dataviz using this data
# 
# Source:
# * [http://data.nsw.gov.au/data/dataset/recorded-crime-dataset-nsw/resource/1046c49f-c896-4831-ab06-a26c8666f01f]
# * [http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.006July%202011?OpenDocument]

library(stringr)
library(reshape2)

crime <- read.csv("tmpRaJWeSRCI_OffenceByMonth.csv")
lga_mapping <- read.csv("1270055006_CG_POSTCODE_2011_LGA_2011.csv") # this is tab 5

cleanLgaName <- function(x) {
  # strip out the last three characters??  
  return(str_trim(substr(x,1,nchar(x)-3)))
}

lga_mapping$LGA = sapply(as.character(lga_mapping$LGA_NAME_2011), cleanLgaName)

# dedup by pulling out the first LGA/POSTCODE match
ag <- by(lga_mapping[,c("LGA", "POSTCODE")], lga_mapping$LGA, FUN = function(x) x[1, ])
ag <- do.call(rbind, ag)

crime_full <- merge(crime, ag)
crime_full <- crime_full[crime_full$POSTCODE != '',]
m_crime <- melt(crime_full[,!(names(crime_full) %in% c("LGA", "Statistical.Division.or.Subdivision"))], 
                id=c("POSTCODE", "Offence.category", "Subcategory"))

# remove all obs, where nothing actually happend...
# remove all postcodes that don't start with 2 (this is when LGAs border different states)
# transform "variable" into date object.
# remove the column variable
m_crime <- m_crime[m_crime$value > 0,]
m_crime <- m_crime[substr(m_crime$POSTCODE,1,1) == 2,]
m_crime$variable <- sapply(as.character(m_crime$variable), function(x) paste0("01.",x))
m_crime$date <- as.Date(m_crime$variable, "%d.%b.%y")

# now we can probably use this in tableau!
write.csv(m_crime, "crime_data.csv", row.names=FALSE)


















