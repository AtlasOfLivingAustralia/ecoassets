
# SQL queries for getting data out of AWS Athena

# 1. distinct_loc -----
# every spatial location (i.e. unique lon/lat) and associated values for 
# state/territory and protected areas (CAPAD, IMCRA, IBRA, Forests of Australia)

####
# SELECT DISTINCT decimallatitude as decimalLatitude, 
# decimallongitude as decimalLongitude,
# cl22 as stateTerritory,
# cl111032 as capad_m_class,
# cl111033 as capad_t_class,
# cl10000 as forest2018, 
# cl10902 as forest2013, 
# cl1048 as ibraRegion, 
# cl966 as imcraRegion
# FROM "ecoassets"."occ_parquet"
# WHERE year >= 1900 
# AND decimallatitude IS NOT NULL
# AND decimallongitude IS NOT NULL
# AND speciesId IS NOT NULL
# AND (cl1048 IS NOT NULL OR cl966 IS NOT NULL)
# AND occurrencestatus = 'PRESENT';
####

# 2. distinct_taxon -----
# every species and associated values for higher taxonomic ranks

####
# SELECT DISTINCT speciesid as speciesId,
# kingdom,
# phylum,
# class,
# _order as "order",
# family,
# genus,
# species as speciesName
# WHERE year >= 1900 
# AND decimallatitude IS NOT NULL
# AND decimallongitude IS NOT NULL
# AND speciesId IS NOT NULL
# AND (cl1048 IS NOT NULL OR cl966 IS NOT NULL)
# AND occurrencestatus = 'PRESENT';
####

# 3. grouped_occ -----
# counts of records grouped by unique combinations of 
# species, location (lat/lon), year, basis of record, and EPBC status

####
# SELECT speciesid as speciesId, 
# species as speciesName,    
# year, 
# decimallatitude as decimalLatitude, 
# decimallongitude as decimalLongitude, 
# basisofrecord as basisOfRecord,
# COUNT(*) as "count"
# FROM "ecoassets"."occ_parquet"
# WHERE year >= 1900 
# AND decimallatitude IS NOT NULL 
# AND decimallongitude IS NOT NULL
# AND speciesId IS NOT NULL 
# AND (cl1048 IS NOT NULL OR cl966 IS NOT NULL)
# AND occurrencestatus = 'PRESENT'
# GROUP BY speciesid, 
# species,
# year, 
# decimalLatitude, 
# decimalLongitude, 
# basisOfRecord; 
####

