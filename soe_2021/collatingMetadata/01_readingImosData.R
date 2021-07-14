library(xml2)
library(data.table)
library(tidyverse)

# Extracting data from the XML
names <- c("identifier", "metadataLinkage", "title", "description", "beginPosition", "endPosition", "westBoundLongitude", 
           "eastBoundLongitude", "northBoundLatitude", "southBoundLatitude", "keywords")

url <- c("https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/oaipmh?verb=ListRecords&metadataPrefix=iso19115-3.2018")

imos <- matrix(ncol=11, nrow=0)
colnames(imos) <- names

while ( url != 0 ) {
  xml <- read_xml(url)
  ns <- xml_ns(xml)
  xmlRecords <- xml_find_all(xml, "//d1:record", ns)
  
  for (xmlRecord in xmlRecords) {
    identifier <- trimws(xml_text(xml_find_all(xmlRecord, ".//mdb:metadataIdentifier/mcc:MD_Identifier/mcc:code/gco:CharacterString[1]", ns)))
    metadataLinkage <- trimws(xml_text(xml_find_all(xmlRecord, ".//mdb:metadataLinkage//cit:linkage//gco:CharacterString", ns)))
    title <- trimws(xml_text(xml_find_all(xmlRecord, ".//mri:citation//cit:title//gco:CharacterString", ns)))
    description <- trimws(xml_text(xml_find_all(xmlRecord, ".//mri:abstract//gco:CharacterString", ns)))
    beginPosition <- trimws(xml_text(xml_find_all(xmlRecord, ".//gex:EX_TemporalExtent//gml:beginPosition", ns)))
    endPosition <- trimws(xml_text(xml_find_all(xmlRecord, ".//gex:EX_TemporalExtent//gml:endPosition", ns)))
    westBoundLongitude <- trimws(xml_text(xml_find_all(xmlRecord, ".//gex:westBoundLongitude//gco:Decimal", ns)))
    eastBoundLongitude <- trimws(xml_text(xml_find_all(xmlRecord, ".//gex:eastBoundLongitude//gco:Decimal", ns)))
    northBoundLatitude <- trimws(xml_text(xml_find_all(xmlRecord, ".//gex:northBoundLatitude//gco:Decimal", ns)))
    southBoundLatitude <- trimws(xml_text(xml_find_all(xmlRecord, ".//gex:southBoundLatitude//gco:Decimal", ns)))
    keywords <- paste(c(trimws(xml_text(xml_find_all(xmlRecord, ".//mri:MD_Keywords/mri:keyword/gcx:Anchor", ns))),
                        trimws(xml_text(xml_find_all(xmlRecord, ".//mri:MD_Keywords/mri:keyword/gco:CharacterString", ns)))),
                      collapse = " ; ")
    
    if (length(beginPosition) == 0) beginPosition = ""
    if (length(endPosition) == 0) endPosition = ""
    if (length(westBoundLongitude) == 0) westBoundLongitude = ""
    if (length(eastBoundLongitude) == 0) eastBoundLongitude = ""
    if (length(northBoundLatitude) == 0) northBoundLatitude = ""
    if (length(southBoundLatitude) == 0) southBoundLatitude = ""
    if (length(keywords) == 0) keywords = ""
    
    imos <- rbind(imos, 
                  c(identifier, metadataLinkage, title, description, beginPosition, endPosition, westBoundLongitude, 
                    eastBoundLongitude, northBoundLatitude, southBoundLatitude, keywords))
  }
  
  token <- trimws(xml_text(xml_find_all(xml, "//d1:resumptionToken", ns)))
  
  if ( nchar(token) != 0 ) {
    print(paste("Resumption token:", token))
    url <- paste("https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/oaipmh?verb=ListRecords&resumptionToken=", token, sep = "")
  } else {
    url <- 0
  } 
  
}

# Converting the data into a dataframe
df <- as.data.frame(imos)

fwrite(df, "cache/data/imos_original.csv")
