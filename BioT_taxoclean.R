

#### Biodiversity Time series DATA ----
#Load BioTIME, assemblage time series data
BioT <- read.csv('~/BioTIMEQuery02_04_2018.csv', sep = ',', na.strings = c('','NA'), stringsAsFactors = FALSE)
head(BioT)
dim(BioT)

#Load BioTIME, characterisitcs of time series
BioT_Char <- read.csv('~/BioTIMEMetadata_02_04_2018.csv', sep = ',', na.strings = c('','NA'), stringsAsFactors = FALSE)
head(BioT_Char)

#Select the columms of "BioT_Char" that I'm going to use
names(BioT_Char)[c(1,2,3,8,9,10,11,12,14,16:22,36,37,40,41)]
BioT_Char2 <- BioT_Char[,c(1,2,3,8,9,10,11,12,14,16:22,36,37,40,41)]
head(BioT_Char2)

#Make some corrections in the "ORGANISMS" Column
BioT_Char2$ORGANISMS <- gsub("Bat", "bats", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Beetles", "beetles", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Bird", "birds", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Birds", "birds", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("birdss", "birds", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("breeding bird pairs", "breeding birds", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Breeding birds", "breeding birds", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Butterflies", "butterflies", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Copepods", "copepods", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Demersal Fish", "Demersal fish", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Fish", "fish", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("fish & marine invertebrates", "fish and marine invertebrates", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Grass", "grasses", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("grassesland plants", "grasses", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Phytoplankton", "phytoplankton", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Plants", "plants", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Sclereactinian corals", "Scleractinian corals", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Small mammal", "small mammals", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Small mammals", "small mammals", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("small mammalss", "small mammals", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Trees ", "Trees", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Woody plants", "woody plants", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Zooplankton", "zooplankton", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("ZooplanKton", "zooplankton", BioT_Char2$ORGANISMS, fixed = T)
BioT_Char2$ORGANISMS <- gsub("Zooplanton", "zooplankton", BioT_Char2$ORGANISMS, fixed = T)

unique(BioT_Char2$ORGANISMS) ###--- Check that there is no different organisms because typing errors (eg. fish and Fish)

# Merge data time series with characteristics
gc() # To clean RAM memory that I'm not using
BioTAll <- merge(BioT, BioT_Char2, by="STUDY_ID", all.x=T)
head(BioTAll)
BioTAll2 <- BioTAll

# convert all letters of GENUS, SPECIES and GENUS_SPECIES in lowercase
BioTAll2$GENUS <- tolower(BioTAll2$GENUS)
BioTAll2$SPECIES <- tolower(BioTAll2$SPECIES)
BioTAll2$GENUS_SPECIES <- tolower(BioTAll2$GENUS_SPECIES)

# Remove leading and trailing white space from GENUS, SPECIES and GENUS_SPECIES
BioTAll2$GENUS <- trimws(BioTAll2$GENUS)
BioTAll2$SPECIES <- trimws(BioTAll2$SPECIES)
BioTAll2$GENUS_SPECIES <- trimws(BioTAll2$GENUS_SPECIES)

# Remove doble space within species names
BioTAll2$GENUS_SPECIES <- gsub("  ", " ",BioTAll2$GENUS_SPECIES, fixed = T)

# Remove "?" from the column SPECIES. I checked it and all of them are typo erros
BioTAll2$SPECIES <- gsub("?","",BioTAll2$SPECIES,fixed=T)
BioTAll2$GENUS <- gsub("?","",BioTAll2$GENUS,fixed=T)
BioTAll2$GENUS_SPECIES <- gsub("?","",BioTAll2$GENUS_SPECIES,fixed=T)
head(BioTAll2)


#Create a new ID_SPECIES because when I corrected the above typo some species names converged
spp_BioT_typo1 <- unique(BioTAll2$GENUS_SPECIES) #unique species after correct first typos
new_ID_SPP <- data.frame(GENUS_SPECIES = spp_BioT_typo1, NEW_ID_SPECIES= seq(1:length(spp_BioT_typo1))) #table with the IDs for all species
#gc()
#memory.limit()
#memory.limit(size = 12000)
BioTAll3 <- merge(BioTAll2, new_ID_SPP, by= "GENUS_SPECIES", all.x = T)
length(unique(BioTAll3$GENUS_SPECIES))
length(unique(BioTAll3$NEW_ID_SPECIES))


#### Check species names and get different codes of species for different databases ----

#get IDs from different digital databases
spp_BioT <- unique(BioTAll3$GENUS_SPECIES)
cod_sp_itis <- taxadb::get_ids(spp_BioT)
cod_sp_col <- taxadb::get_ids(spp_BioT, db = "col")
cod_sp_gbif <- taxadb::get_ids(spp_BioT, db = "gbif")
cod_sp_ott <- taxadb::get_ids(spp_BioT, db = "ott")
cod_sp_iucn <- taxadb::get_ids(spp_BioT, db = "iucn")
#get valid species name from different digital databases
spp_val_itis <- taxadb::get_names(cod_sp_itis, format= "prefix")
spp_val_col <- taxadb::get_names(cod_sp_col, db = "col", format= "prefix")
spp_val_gbif <- taxadb::get_names(cod_sp_gbif, db = "gbif", format= "prefix")
spp_val_ott <- taxadb::get_names(cod_sp_ott, db = "ott", format= "prefix")
spp_val_iucn <- taxadb::get_names(cod_sp_iucn, db = "iucn", format= "prefix")

# Dataframe with species name from the Source (BioTIME) and ID codes and valid species names according different taxonomic databases
tax_valid <- data.frame(SPECIES_Source = spp_BioT, CODE_itis = cod_sp_itis, CODE_col = cod_sp_col, CODE_gbif = cod_sp_gbif, CODE_ott = cod_sp_ott, CODE_iucn = cod_sp_iucn, SPECIES_VAL_itis = spp_val_itis, SPECIES_VAL_col = spp_val_col, SPECIES_VAL_gbif = spp_val_gbif, SPECIES_VAL_ott = spp_val_ott, SPECIES_VAL_iucn = spp_val_iucn)
head(tax_valid)
dim(tax_valid)
length(tax_valid$SPECIES_Source)

# Create a table with the taxonomy data of BioTIME
BioT_taxono <- unique(BioTAll3[,c("NEW_ID_SPECIES","GENUS","SPECIES","GENUS_SPECIES")])
dim(BioT_taxono)

# Merge table of the valid species names and the species codes with taxonomy table of BioTIME
tax_valid2 <- merge(tax_valid, BioT_taxono, by.x="SPECIES_Source", by.y="GENUS_SPECIES")
dim(tax_valid)
dim(tax_valid2)
head(tax_valid2)

# Select species with no valid name in any taxonomic database
taxo_NA <- tax_valid2[Reduce(intersect,list(which(is.na(tax_valid2$SPECIES_VAL_itis)), which(is.na(tax_valid2$SPECIES_VAL_col)), which(is.na(tax_valid2$SPECIES_VAL_gbif)), which(is.na(tax_valid2$SPECIES_VAL_ott)))),]
head(taxo_NA)
dim(taxo_NA)

#There are some species (40 in total) that have code but the valid species names in all taxonomy databases are NA. Because there are just few of them, I'll work with taxo_NA that are all species that don't have match in species name, no matter, if they have valid code or no.
#taxo_NA_CODE <- tax_valid2[Reduce(intersect,list(which(is.na(tax_valid2$CODE_itis)), which(is.na(tax_valid2$CODE_col)), which(is.na(tax_valid2$CODE_gbif)), which(is.na(tax_valid2$CODE_ott)))),]
#head(taxo_NA_CODE)
#dim(taxo_NA_CODE)

# Check if there are some typo errors identifiable by "taxize" packages
res_typo_correc <- list() #List to save the results of each species
#Loop to check typo
for(i in 1:length(taxo_NA$SPECIES_Source)){
  res_typo_correc[[i]] <- as.data.frame(taxize::gnr_resolve(taxo_NA$SPECIES_Source[i], best_match_only = TRUE, canonical = TRUE))
  print(i)
}

#res_typo_correc2 <- do.call("rbind", res_typo_correc) #rbind all results
#head(res_typo_correc2)
#dim(res_typo_correc2)
#res_typo_correc2$number_words <- sapply(strsplit(res_typo_correc2$matched_name2, " "), length) # Number of words in the recovered species names
#table(res_typo_correc2$number_words)
#res_typo_correc2$correct_typo <- rep(TRUE, nrow(res_typo_correc2)) # Add a column to identify to which species (records) have typo corrections



#Merge the corrections of typo with table of species that for which I didn't find species or IDs using "taxadb" packages (ie. taxo_NA2)
head(taxo_NA)
taxo_NA_correc_typo <- merge(taxo_NA, res_typo_correc2[,-2], by.x = "SPECIES_Source", by.y = "user_supplied_name", all.x = T)
taxo_NA_correc_typo$matched_name2 <- tolower(taxo_NA_correc_typo$matched_name2) # all lowercase to match this species names with those reported in BioTIME
taxo_NA_correc_typo$correct_typo <- gsub(NA,FALSE,taxo_NA_correc_typo$correct_typo, fixed = T)
taxo_NA_correc_typo[c("correct_typo")][is.na(taxo_NA_correc_typo[c("correct_typo")])] <- FALSE #Change NA values by FALSE to create a logical column (TRUE, for species with typo correction, FALSE, no typo corrections)

head(taxo_NA_correc_typo)
dim(taxo_NA)
dim(taxo_NA_correc_typo)

#get IDs from different digital databases of corrected species names
spp_corec_typo <- taxo_NA_correc_typo$matched_name2
cod_sp_itis_corec_typo <- taxadb::get_ids(spp_corec_typo)
cod_sp_col_corec_typo <- taxadb::get_ids(spp_corec_typo, db = "col")
cod_sp_gbif_corec_typo <- taxadb::get_ids(spp_corec_typo, db = "gbif")
cod_sp_ott_corec_typo <- taxadb::get_ids(spp_corec_typo, db = "ott")
cod_sp_iucn_corec_typo <- taxadb::get_ids(spp_corec_typo, db = "iucn")
#get valid species name from different digital databases of corrected species names
spp_val_itis_corec_typo <- taxadb::get_names(spp_corec_typo, format= "prefix")
spp_val_col_corec_typo <- taxadb::get_names(spp_corec_typo, db = "col", format= "prefix")
spp_val_gbif_corec_typo <- taxadb::get_names(spp_corec_typo, db = "gbif", format= "prefix")
spp_val_ott_corec_typo <- taxadb::get_names(spp_corec_typo, db = "ott", format= "prefix")
spp_val_iucn_corec_typo <- taxadb::get_names(spp_corec_typo, db = "iucn", format= "prefix")

#Add new IDs and species names as columns
taxo_NA_correc_typo$cod_sp_itis_corec_typo <- cod_sp_itis_corec_typo
taxo_NA_correc_typo$cod_sp_col_corec_typo <- cod_sp_col_corec_typo
taxo_NA_correc_typo$cod_sp_gbif_corec_typo <- cod_sp_gbif_corec_typo
taxo_NA_correc_typo$cod_sp_ott_corec_typo <- cod_sp_ott_corec_typo
taxo_NA_correc_typo$cod_sp_iucn_corec_typo <- cod_sp_iucn_corec_typo

taxo_NA_correc_typo$spp_val_itis_corec_typo <- spp_val_itis_corec_typo
taxo_NA_correc_typo$spp_val_col_corec_typo <- spp_val_col_corec_typo
taxo_NA_correc_typo$spp_val_gbif_corec_typo <- spp_val_gbif_corec_typo
taxo_NA_correc_typo$spp_val_ott_corec_typo <- spp_val_ott_corec_typo
taxo_NA_correc_typo$spp_val_iucn_corec_typo <- spp_val_iucn_corec_typo

head(taxo_NA_correc_typo)
head(tax_valid2)

# Merge the original table of species-taxonomy validation to the corrected species names
first_taxo_match <- ifelse(tax_valid2$NEW_ID_SPECIES %in% unique(taxo_NA$NEW_ID_SPECIES), FALSE, TRUE) # create a new column showing if species that matched (TRUE) or not (FALSE) with a species (or genus, or family,...) during the first search
tax_valid2$first_taxo_match <- first_taxo_match
tax_valid_final <- merge(tax_valid2, taxo_NA_correc_typo[,-which(names(taxo_NA_correc_typo) %in% c("SPECIES_Source","CODE_itis","CODE_col","CODE_gbif","CODE_ott","CODE_iucn","SPECIES_VAL_itis","SPECIES_VAL_col","SPECIES_VAL_gbif","SPECIES_VAL_ott","SPECIES_VAL_iucn","GENUS","SPECIES")) ], by = "NEW_ID_SPECIES", all.x = T)
tax_valid_final[c("correct_typo")][is.na(tax_valid_final[c("correct_typo")])] <- FALSE #Change NA values by FALSE generated in the merge of "tax_valid2" and taxo_NA_correc_typo" (TRUE, for species with typo correction, FALSE, no typo corrections)
head(tax_valid_final)
dim(tax_valid_final)

#Rename some variables and then reorder some columns
names(tax_valid_final)
tax_valid_final2 <- tax_valid_final[,c(1,2,13,14,3:12,15:18,20:30)] #reoder columns and remove column "number_words"
head(tax_valid_final2)
names(tax_valid_final2)[16:17] <- c("data_source_correc", "score_correc")
names(tax_valid_final2)[20:29] <- c("cod_sp_itis_correc","cod_sp_col_correc","cod_sp_gbif_correc","cod_sp_ott_correc","cod_sp_iucn_correc","spp_val_itis_correc","spp_val_col_correc","spp_val_gbif_correc","spp_val_ott_correc","spp_val_iucn_correc")
head(tax_valid_final2)
dim(tax_valid_final2)


#### Get the taxonomy of species ----

#Add taxonomy according to gbif and where is no data for based on catalog of life (col). I use gbif and col because these are the unique databases that gives taxonomic information in the "taxadb" packages

taxo_gbif <- as.data.frame(taxadb::filter_id(tax_valid_final2$CODE_gbif, provider = "gbif")) # taxonomy based on "CODE_gbif"
taxo_gbif_correc <- as.data.frame(taxadb::filter_id(tax_valid_final2$cod_sp_gbif_correc, provider = "gbif"))  # taxonomy based on "cod_sp_gbif_correc"

taxo_col <- as.data.frame(taxadb::filter_id(tax_valid_final2$CODE_col, provider = "col")) # taxonomy based on "CODE_col"
taxo_col_correc <- as.data.frame(taxadb::filter_id(tax_valid_final2$cod_sp_col_correc, provider = "col")) # taxonomy based on "cod_sp_col_correc"

taxo_results <- data.frame() #dataframe to save results


#loop to extract taxonomy
for(i in 1:nrow(tax_valid_final2)){
  if(is.na(tax_valid_final2[i,"CODE_gbif"]) == FALSE){
    taxo_results[i,1] <- taxo_gbif[i,"taxonomicStatus"]
    taxo_results[i,2] <- taxo_gbif[i,"taxonRank"]
    taxo_results[i,3] <- taxo_gbif[i,"kingdom"]
    taxo_results[i,4] <- taxo_gbif[i,"phylum"]
    taxo_results[i,5] <- taxo_gbif[i,"class"]
    taxo_results[i,6] <- taxo_gbif[i,"order"]
    taxo_results[i,7] <- taxo_gbif[i,"family"]
    taxo_results[i,8] <- taxo_gbif[i,"genus"]
    taxo_results[i,9] <- taxo_gbif[i,"specificEpithet"]
    taxo_results[i,10] <- taxo_gbif[i,"infraspecificEpithet"]
    taxo_results[i,11] <- "gbif"
    print(i)
  } else if (is.na(tax_valid_final2[i,"cod_sp_gbif_correc"]) == FALSE){
    taxo_results[i,1] <- taxo_gbif_correc[i,"taxonomicStatus"]
    taxo_results[i,2] <- taxo_gbif_correc[i,"taxonRank"]
    taxo_results[i,3] <- taxo_gbif_correc[i,"kingdom"]
    taxo_results[i,4] <- taxo_gbif_correc[i,"phylum"]
    taxo_results[i,5] <- taxo_gbif_correc[i,"class"]
    taxo_results[i,6] <- taxo_gbif_correc[i,"order"]
    taxo_results[i,7] <- taxo_gbif_correc[i,"family"]
    taxo_results[i,8] <- taxo_gbif_correc[i,"genus"]
    taxo_results[i,9] <- taxo_gbif_correc[i,"specificEpithet"]
    taxo_results[i,10] <- taxo_gbif_correc[i,"infraspecificEpithet"]
    taxo_results[i,11] <- "gbif"
    print(i)
  } else if (is.na(tax_valid_final2[i,"CODE_col"]) == FALSE){
    taxo_results[i,1] <- taxo_col[i,"taxonomicStatus"]
    taxo_results[i,2] <- taxo_col[i,"taxonRank"]
    taxo_results[i,3] <- taxo_col[i,"kingdom"]
    taxo_results[i,4] <- taxo_col[i,"phylum"]
    taxo_results[i,5] <- taxo_col[i,"class"]
    taxo_results[i,6] <- taxo_col[i,"order"]
    taxo_results[i,7] <- taxo_col[i,"family"]
    taxo_results[i,8] <- taxo_col[i,"genus"]
    taxo_results[i,9] <- taxo_col[i,"specificEpithet"]
    taxo_results[i,10] <- taxo_col[i,"infraspecificEpithet"]
    taxo_results[i,11] <- "col"
    print(i)
  } else {
    taxo_results[i,1] <- taxo_col_correc[i,"taxonomicStatus"]
    taxo_results[i,2] <- taxo_col_correc[i,"taxonRank"]
    taxo_results[i,3] <- taxo_col_correc[i,"kingdom"]
    taxo_results[i,4] <- taxo_col_correc[i,"phylum"]
    taxo_results[i,5] <- taxo_col_correc[i,"class"]
    taxo_results[i,6] <- taxo_col_correc[i,"order"]
    taxo_results[i,7] <- taxo_col_correc[i,"family"]
    taxo_results[i,8] <- taxo_col_correc[i,"genus"]
    taxo_results[i,9] <- taxo_col_correc[i,"specificEpithet"]
    taxo_results[i,10] <- taxo_col_correc[i,"infraspecificEpithet"]
    taxo_results[i,11] <- "col"
    print(i)
  }
}

names(taxo_results) <- c("taxonomicStatus","taxonRank","kingdom","phylum","class","order","family","genus","specificEpithet","infraspecificEpithet","code_source_taxo") # add column names 
taxo_results$ID_code <- seq(1:nrow(taxo_results))# add code
head(taxo_results)
dim(taxo_results)


#Subset species (rows) with no taxonomic results
taxo_results_NA <- taxo_results[Reduce(intersect,list(which(is.na(taxo_results$taxonomicStatus)),which(is.na(taxo_results$taxonRank)),which(is.na(taxo_results$kingdom)),which(is.na(taxo_results$phylum)),which(is.na(taxo_results$class)),which(is.na(taxo_results$order)),which(is.na(taxo_results$family)),which(is.na(taxo_results$genus)),which(is.na(taxo_results$specificEpithet)),which(is.na(taxo_results$infraspecificEpithet)))),]

#Subset the variable "GENUS" from "tax_valid_final2" for the species that don't have taxonomic info, to search the taxonomy based of "GENUS"instead of the species name in the original database.
GENUS_taxo_NA <- tax_valid_final2[taxo_results_NA$ID_code,"GENUS"]
head(GENUS_taxo_NA)

#Get the ID of the genus in gbif and col
cod_GENUS_gbif <- taxadb::get_ids(GENUS_taxo_NA, db = "gbif")
#cod_GENUS_col <- taxadb::get_ids(GENUS_taxo_NA, db = "col") # there is no code for any genus in col
head(cod_GENUS_gbif)
#head(cod_GENUS_col)

#Get the taxonomy of the GENUS according  gbif. 
taxo_gbif_GENUS <- as.data.frame(taxadb::filter_id(cod_GENUS_gbif, provider = "gbif"))
taxo_gbif_GENUS$code_source_taxo <- rep("gbif", nrow(taxo_gbif_GENUS))#add the "code_source_taxo" column 
taxo_gbif_GENUS$ID_code <- taxo_results_NA$ID_code# Add ID_code
head(taxo_gbif_GENUS)

taxo_gbif_GENUS2 <- taxo_gbif_GENUS[,which(names(taxo_gbif_GENUS)%in%names(taxo_results))]#Subset the columns in the same order as "taxo_results"
taxo_gbif_GENUS2 <- taxo_gbif_GENUS2[,c(2,1,3:ncol(taxo_gbif_GENUS2))] #reorder to match column names of both tables

taxo_results_2 <- taxo_results
taxo_results_2[taxo_gbif_GENUS2$ID_code,] <- taxo_gbif_GENUS2

head(taxo_results)
head(taxo_results_2)

dim(taxo_results)
dim(taxo_results_2)

###### Identify records of fishes and extract the taxonomy according FishBase

#Identify which species are fishes using the "validate_taxonomy" function
#Then use the function "taxonomy to identify if the genus of family are exist in FishBase

head(BioTAll3)
head(tax_valid_final2)
head(taxo_results_2)
dim(tax_valid_final2)
dim(taxo_results_2)


#Loop to capitalize the first letter of the first word (genus) of the species name. I do that, because FishBase always has the first letter of the genus in uppercase.
spp_uppr <- c()

for(i in 1:nrow(tax_valid_final2)){
  spl_sp <- unlist(strsplit(tax_valid_final2$SPECIES_Source[i], " ")) #split words
  upp_case <- gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",spl_sp[1], perl=TRUE)    # Uppercase the first letter of a the first word
  spp_uppr [i] <- paste(upp_case, spl_sp [-1], sep = " ") #paste all the words of the species name
  print(i)
}


# Loop to check the taxonomy according fishbase
spp_val_FB <- data.frame()
for(i in 1:length(spp_uppr)){
  spp_val_FB[i,1] <- spp_uppr [i] #save species name in the sources
  spp_val_FB[i,2] <- tax_valid_final2$NEW_ID_SPECIES[i]
  if(length(rfishbase::validate_names(spp_uppr[i]))>0){
    spp_val_FB[i,3] <- rfishbase::validate_names(spp_uppr[i]) [1]#save valid species name in case it exists. Otherwise NA (Species that are not fishes or species names of fishes that are not valid in FishBase)
  } else {spp_val_FB[i,3] <- NA}
  print(i)
}

#i <- 4902
#4902 has two values of valid scientific name: "Centriscus cristatus" and "Notopogon lilliei". Then I put manually "Centriscus cristatus" as scientific name

names(spp_val_FB) <- c("SPECIES_Source","NEW_ID_SPECIES","val_sp_name_FB") # rename columns


#save the taxonomy table of fish base in other object
fishbase2 <- as.data.frame(fishbase)
fishbase2$Sci_name <- paste(fishbase2$Genus, fishbase2$Species)
head(fishbase2)


#Loop to extract taxonomy of fish species based on the species name of the given in the database source
fish_match <- data.frame()
for(i in 1:nrow(spp_val_FB)){
  if(!is.na(spp_val_FB$val_sp_name_FB[i])){
    spp_match <- grep(pattern=paste0("\\b",spp_val_FB$val_sp_name_FB[i],"\\b"), x=fishbase2$Sci_name)[1] # check when species names match, I just keep one of the match because there are more than one result just when there are more than 1 subspecies, as I'm working at species level I keep just one of them
    fish_match[i,1] <- "species"
    fish_match[i,2] <- fishbase2$Sci_name[spp_match]
    fish_match[i,3] <- fishbase2$SpecCode[spp_match]
    fish_match[i,4] <- fishbase2$Genus[spp_match]
    fish_match[i,5] <- fishbase2$Family[spp_match]
    fish_match[i,6] <- fishbase2$Order[spp_match]
    fish_match[i,7] <- fishbase2$Class[spp_match]
    fish_match[i,8] <- "Chordata"
    fish_match[i,9] <- "Animalia"
  } else {
    fish_match[i,1] <- NA
    fish_match[i,2] <- NA
    fish_match[i,3] <- NA
    fish_match[i,4] <- NA
    fish_match[i,5] <- NA
    fish_match[i,6] <- NA
    fish_match[i,7] <- NA
    fish_match[i,8] <- NA
    fish_match[i,9] <- NA
  }
  print(i)
}

names(fish_match) <- c("Taxonomy_based","Valid_SciName_FB", "SpecCode_FB","genus","family","order","class","phylum","kingdom")
head(fish_match)
dim(fish_match)



# Extract taxonomy according the genus

fishbase3 <- unique(fishbase2[,c("Genus","Family","Order","Class","GenCode","FamCode")]) #save unique values of genus, family, order and class

#Subset NEW_ID_SPECIES for records that belong to fishes according to the variables "TAXA" and "ORGANISMS" from BioTIME 
TAXA_code_BT <- unique(BioTAll3[,c("TAXA", "NEW_ID_SPECIES")])
ORGA_code_BT <- unique(BioTAll3[,c("ORGANISMS", "NEW_ID_SPECIES")])

unique(TAXA_code_BT$TAXA)
ID_Fish_TAXA <- TAXA_code_BT$NEW_ID_SPECIES[which(TAXA_code_BT$TAXA == "Fish")] # Get species "NEW_ID_SPECIES" of fish species based "TAXA"

sort(unique(ORGA_code_BT$ORGANISMS))
ID_Fish_ORGA <- ORGA_code_BT$NEW_ID_SPECIES[which(ORGA_code_BT$ORGANISMS %in% c("benthic and demersal fish ","Chondrichthyes and Osteichthyes","coral reef fish","Demersal fish","Diadromous fishes","fish","Freshwater fish","Groundfish","Marine fish","reef fish","Stream fish","teleost fishes and elasmobranchs","tropical reef fish"))] # Get species "NEW_ID_SPECIES" of fish species based "ORGANISMS"

All_fish_ID <- sort(unique(ID_Fish_TAXA, ID_Fish_ORGA)) #Get the "NEW_ID_SPECIES" for all fish species according to the "ORGANISMS" and "TAXA" columns.

#Loop to extract taxonomy based on genus
i <- 1
fish_match_2 <- fish_match
for(i in 1:nrow(fish_match_2)){
  if(length(which(All_fish_ID == i))>0){
    if(is.na(fish_match_2$Taxonomy_based[i])){
      spl_sp <- unlist(strsplit(tax_valid_final2$SPECIES_Source[i], " ")) #split words of species name
      upp_case_gen <- gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",spl_sp[1], perl=TRUE)
      genus_match <- which(fishbase3$Genus == upp_case_gen)
      fam_match <- which(fishbase3$Family == upp_case_gen)
      if(length(genus_match) > 0){
        fish_match_2[i,1] <- "genus"
        fish_match_2[i,4] <- fishbase3$Genus[genus_match]
        fish_match_2[i,5] <- fishbase3$Family[genus_match]
        fish_match_2[i,6] <- fishbase3$Order[genus_match]
        fish_match_2[i,7] <- fishbase3$Class[genus_match]
        fish_match_2[i,8] <- "Chordata"
        fish_match_2[i,9] <- "Animalia"
        print(i)
      } else if (length(fam_match) > 0) {
        fish_match_2[i,1] <- "family"
        fish_match_2[i,5] <- unique(fishbase3$Family[fam_match])
        fish_match_2[i,6] <- unique(fishbase3$Order[fam_match])
        fish_match_2[i,7] <- unique(fishbase3$Class[fam_match])
        fish_match_2[i,8] <- "Chordata"
        fish_match_2[i,9] <- "Animalia"
        print(i)
      } else {
        print(i)
      }
    } else {
      print(i)
    }
  } else {
    print(i)
  }
}


# Check which species with typo correction match with FishBase taxonomy

#Some manual correction of species names
# "Catastomus commersoni" by "Catostomus commersonii"
# "Xiphius gladius" by "Xiphias gladius"
# "Evermanella indica" by "Evermannella indica"
# "Semotilis atromaculatus" by "Semotilus atromaculatus"
# "Pimephalus notatus" by "Pimephales notatus"
# "Xyrichthys dea" by "Iniistius dea"
tax_valid_final2$matched_name2 <- gsub("Catastomus commersoni","Catostomus commersonii",tax_valid_final2$matched_name2,fixed = T)
tax_valid_final2$matched_name2 <- gsub("Xiphius gladius","Xiphias gladius",tax_valid_final2$matched_name2,fixed = T)
tax_valid_final2$matched_name2 <- gsub("Evermanella indica","Evermannella indica",tax_valid_final2$matched_name2,fixed = T)
tax_valid_final2$matched_name2 <- gsub("Semotilis atromaculatus","Semotilus atromaculatus",tax_valid_final2$matched_name2,fixed = T)
tax_valid_final2$matched_name2 <- gsub("Pimephalus notatus","Pimephales notatus",tax_valid_final2$matched_name2, fixed = T)
tax_valid_final2$matched_name2 <- gsub("Xyrichthys dea","Iniistius dea",tax_valid_final2$matched_name2,fixed=T)

#New object to save results
fish_match_3 <- fish_match_2

rows_typo_corr <- which(!is.na(tax_valid_final2$matched_name2)) # rows with typo corrections
rows_tax_FB_NA <- which(is.na(fish_match_3$Taxonomy_based)) # rows of "fish_match_3" with no FB taxonomy
All_fish_ID # "NEW_ID_SPECIES"  (row number) for all fish species according to the "ORGANISMS" and "TAXA" columns in BioTIME.

rows_check <- intersect(All_fish_ID,intersect(which(!is.na(tax_valid_final2$matched_name2)),which(is.na(fish_match_3$Taxonomy_based))))# rows belonging to fish taxa with typo correction and no FB taxonomy

#Loop to assing taxonomy data for species with typo corrections and valid species name for Rfishbase.
for(i in 1:length(rows_check)){
  sp_typo <- tax_valid_final2$matched_name2[rows_check[i]] # get the taxon name
  sp_valid <- rfishbase::validate_names(sp_typo) # get valid species name from FishBase
  
  if(length(sp_valid) > 0){
    fish_match_3[rows_check[i],1] <- "species" # Taxonomy based in "species"
    fish_match_3[rows_check[i],2] <- sp_valid # Valid species
    fish_match_3[rows_check[i],3] <- fishbase2[which(fishbase2$Sci_name %in% sp_valid),"SpecCode"] #FishBase code
    fish_match_3[rows_check[i],4] <- fishbase2[which(fishbase2$Sci_name %in% sp_valid),"Genus"] #Genus
    fish_match_3[rows_check[i],5] <- fishbase2[which(fishbase2$Sci_name %in% sp_valid),"Family"] #Family
    fish_match_3[rows_check[i],6] <- fishbase2[which(fishbase2$Sci_name %in% sp_valid),"Order"] #Order
    fish_match_3[rows_check[i],7] <- fishbase2[which(fishbase2$Sci_name %in% sp_valid),"Class"] #Class
    fish_match_3[rows_check[i],8] <- "Chordata" #Phylum
    fish_match_3[rows_check[i],9] <- "Animalia" #Kingdom
    print(i)
  } else {print (i)}
}


# Unify the taxonomy tables: "tax_valid_final2" (table with valid species name and ID codes according to different taxonomy database included in the "taxadb" packages) and "taxo_results_2" (species taxonomy according to gibf and col)
head(tax_valid_final2,2)
head(taxo_results_2,2)
dim(tax_valid_final2)
dim(taxo_results_2)

Taxonomy_BioTIME <- cbind(tax_valid_final2,taxo_results_2[,1:11])
head(Taxonomy_BioTIME,2)

names(Taxonomy_BioTIME)[which(names(Taxonomy_BioTIME) %in% c("GENUS","SPECIES","first_taxo_match","matched_name2","correct_typo"))] <- c("GENUS_Source","specificEpithet_Source","first_sp_code_match", "name_typo_correc","typo_correc") #change some column names


# Integrate info of "fish_match_3" (taxonomy based on fishbase) into "Taxonomy_BioTIME" 
head(Taxonomy_BioTIME,2)
head(fish_match_3,2)
dim(Taxonomy_BioTIME)
dim(fish_match_3)

Taxonomy_BioTIME_2 <- Taxonomy_BioTIME
rows_taxon_FB <- which(!is.na(fish_match_3$Taxonomy_based))

i <- 390
CODE_fb <- c()
SPECIES_VAL_fb <- c()

#Loop to integrate FishBase data
for (i in 1:nrow(Taxonomy_BioTIME_2)){
  if(length(which(rows_taxon_FB == i)) > 0) {
    species_FB <- fish_match_3[i,"Valid_SciName_FB"]
    CODE_fb[i] <- paste0("FB:",fish_match_3[i,"SpecCode_FB"])
    SPECIES_VAL_fb[i] <- fish_match_3[i,"Valid_SciName_FB"]
    Taxonomy_BioTIME_2[i,"taxonomicStatus"] <- "accepted"
    Taxonomy_BioTIME_2[i,"taxonRank"] <- fish_match_3[i,"Taxonomy_based"]
    Taxonomy_BioTIME_2[i,"kingdom"] <- fish_match_3[i,"kingdom"]
    Taxonomy_BioTIME_2[i,"phylum"] <- fish_match_3[i,"phylum"]
    Taxonomy_BioTIME_2[i,"class"] <- fish_match_3[i,"class"]
    Taxonomy_BioTIME_2[i,"order"] <- fish_match_3[i,"order"]
    Taxonomy_BioTIME_2[i,"family"] <- fish_match_3[i,"family"]
    Taxonomy_BioTIME_2[i,"genus"] <- fish_match_3[i,"genus"]
    Taxonomy_BioTIME_2[i,"specificEpithet"] <- unlist(strsplit(species_FB, " "))[2]
    Taxonomy_BioTIME_2[i,"infraspecificEpithet"] <- unlist(strsplit(species_FB, " "))[3]
    Taxonomy_BioTIME_2[i,"code_source_taxo"] <- "fb"
    print(i)
  } else {
    species_FB <- NA
    CODE_fb[i] <- NA
    print(i)
  }
}

## Add valid species names and codes according FishBase.

SPECIES_VAL_fb[44178:nrow(Taxonomy_BioTIME_2)] <- NA #Add some NA for species with no valid species name at the end of the vector
length(SPECIES_VAL_fb)

Taxonomy_BioTIME_2$CODE_fb <- CODE_fb # Add column with FishBase codes of species
Taxonomy_BioTIME_2$SPECIES_VAL_fb <- SPECIES_VAL_fb # Add column with valid species name in FishBase
head(Taxonomy_BioTIME_2)
dim(Taxonomy_BioTIME_2)
names(Taxonomy_BioTIME_2)

Taxonomy_BioTIME_3 <- Taxonomy_BioTIME_2[,c(1:19,42:41,20:40)] # Reorder columns
head(Taxonomy_BioTIME_3)


#### Add species code, valid species names and taxonomy  for species with no codes using "rgif" ----
Taxonomy_BioTIME_4 <- Taxonomy_BioTIME_3
head(Taxonomy_BioTIME_4,2)

rows_spp_NA_gbif <- which(is.na(Taxonomy_BioTIME_3$SPECIES_VAL_gbif)) # rows with no species in gbif "SPECIES_VAL_gbif"
#which(!is.na(Taxonomy_BioTIME_4$spp_val_gbif_correc)) # There is no species after typo correction with valid species name according "taxadb-gbif". Thus, I just work with "rows_spp_NA_gbif"

match_rgbif <- c()
i <- 1
for(i in 1:nrow(Taxonomy_BioTIME_4)){
  if (length(which(rows_spp_NA_gbif %in% i)) > 0) {
    gbif_sp <- Taxonomy_BioTIME_4$SPECIES_Source[i]
    data_gbif_sp <- data.frame(rgbif::name_backbone(gbif_sp))
    if (dim(data_gbif_sp)[1] >0 & dim(data_gbif_sp)[2] > 9){
      Taxonomy_BioTIME_4[i,"CODE_gbif"] <- ifelse(length(which(names(data_gbif_sp) %in% "acceptedUsageKey")) > 0, paste0("GBIF:",data_gbif_sp[1,"acceptedUsageKey"]), paste0("GBIF:",data_gbif_sp[1,"usageKey"]))  # To select the acepted code in case there is a synonym, otherwise the species code
      match_rgbif [i] <- TRUE 
      Taxonomy_BioTIME_4[i,"taxonomicStatus"] <- tolower(data_gbif_sp[1,"status"])
      Taxonomy_BioTIME_4[i,"taxonRank"] <- tolower(data_gbif_sp[1,"rank"])
      # now I putting ifelse becuse in the taxonomy of many algues and bentos there are sepcies that are not assigned to genus and/or family, and so on...
      Taxonomy_BioTIME_4[i,"kingdom"] <- ifelse(length(which(names(data_gbif_sp)%in% "kingdom")) > 0, data_gbif_sp[1,"kingdom"], NA)
      Taxonomy_BioTIME_4[i,"phylum"] <- ifelse(length(which(names(data_gbif_sp)%in% "phylum")) > 0, data_gbif_sp[1,"phylum"], NA)
      Taxonomy_BioTIME_4[i,"class"] <- ifelse(length(which(names(data_gbif_sp)%in% "class")) > 0, data_gbif_sp[1,"class"], NA)
      Taxonomy_BioTIME_4[i,"order"] <- ifelse(length(which(names(data_gbif_sp)%in% "order")) > 0, data_gbif_sp[1,"order"], NA)
      Taxonomy_BioTIME_4[i,"family"] <- ifelse(length(which(names(data_gbif_sp)%in% "family")) > 0, data_gbif_sp[1,"family"], NA)
      Taxonomy_BioTIME_4[i,"genus"] <- ifelse(length(which(names(data_gbif_sp)%in% "genus")) > 0, data_gbif_sp[1,"genus"], NA)
      Taxonomy_BioTIME_4[i,"code_source_taxo"] <- "gbif"
      
      if(data_gbif_sp[1,"rank"] == "SPECIES") {
        Taxonomy_BioTIME_4[i,"SPECIES_VAL_gbif"] <- data_gbif_sp[1,"canonicalName"]
        Taxonomy_BioTIME_4[i,"specificEpithet"] <- unlist(strsplit(data_gbif_sp[1,"species"], " "))[2]
        Taxonomy_BioTIME_4[i,"infraspecificEpithet"] <- unlist(strsplit(data_gbif_sp[1,"species"], " "))[3]
      } else if (data_gbif_sp[1,"rank"] == "GENUS") {
        Taxonomy_BioTIME_4[i,"SPECIES_VAL_gbif"] <- data_gbif_sp[1,"genus"]
      } else if (data_gbif_sp[1,"rank"] == "FAMILY") {
        Taxonomy_BioTIME_4[i,"SPECIES_VAL_gbif"] <- data_gbif_sp[1,"family"]
      } else if (data_gbif_sp[1,"rank"] == "ORDER") {
        Taxonomy_BioTIME_4[i,"SPECIES_VAL_gbif"] <- data_gbif_sp[1,"order"]
      } else if (data_gbif_sp[1,"rank"] == "CLASS") {
        Taxonomy_BioTIME_4[i,"SPECIES_VAL_gbif"] <- data_gbif_sp[1,"class"]
      } else if (data_gbif_sp[1,"rank"] == "PHYLUM") {
        Taxonomy_BioTIME_4[i,"SPECIES_VAL_gbif"] <- data_gbif_sp[1,"phylum"]
      } else {
        Taxonomy_BioTIME_4[i,"SPECIES_VAL_gbif"] <- NA
      }
      print(i)
    } else {
      match_rgbif [i] <- FALSE
      print(i)
    }
  } else {
    match_rgbif [i] <- FALSE
    print(i)
  }
}

Taxonomy_BioTIME_4$match_rgbif <- match_rgbif
head(Taxonomy_BioTIME_4)

names(Taxonomy_BioTIME_4)[c(15:16,42)] <- c("match_taxadb", "source_correc","taxonomy_source")



#Remove columns "spp_val_itis_correc", "spp_val_col_correc", "spp_val_gbif_correc" and "spp_val_ott_correc" that do not have info. TO use the valid species name according to the correction see column "name_typo_correc"

Taxonomy_BioTIME_5 <- Taxonomy_BioTIME_4[,-which(names(Taxonomy_BioTIME_4) %in% c("spp_val_itis_correc","spp_val_col_correc","spp_val_gbif_correc","spp_val_ott_correc"))]



#### Check and add taxonomy using "worrms" packages for marine species that are not fishes  ----

#Subset species source and NEW_ID_CODE for marine species - No fishes - in BIoTIME.
BioT_Marine <- BioTAll3[which(BioTAll3$REALM == "Marine"),] #Species distributed in the Marine realm
BioT_Marine_NoFish <- BioT_Marine[which(BioT_Marine$TAXA != "Fish"),] #Species distributed in the Marine realm and are not fishes
head(Spp_Code_MAr_NoFish)
dim(Spp_Code_MAr_NoFish)

head(BioT_Marine_NoFish)
Spp_Code_MAr_NoFish <- unique(BioT_Marine_NoFish[,c("GENUS_SPECIES","NEW_ID_SPECIES")]) #get species_source and ID in "BioT_Marine_NoFish"
head(Spp_Code_MAr_NoFish)
dim(Spp_Code_MAr_NoFish)

# Subset Marine species (Not fishes) that still don't have species code of gbif
head(BioT_no_gbif)
Taxo_BioT_Marine_NoFish <- Taxonomy_BioTIME_5[which(Taxonomy_BioTIME_5$NEW_ID_SPECIES %in% Spp_Code_MAr_NoFish$NEW_ID_SPECIES),]
dim(Taxo_BioT_Marine_NoFish)
Taxo_BioT_Marine_NoFish$SPECIES_Source

#Remove some words from species name
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("family ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("class ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("order ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("class ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("phylum ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("unid ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("suborder ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("unidentified ","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub(" unidentified","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub(" ind","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub(" indet","",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T)
Taxo_BioT_Marine_NoFish$SPECIES_Source <- gsub("[[:digit:]]","",Taxo_BioT_Marine_NoFish$SPECIES_Source, perl = T)#remove numbers

rows_Taxo_BioT_Marine_NoFish_sp <- grep(" sp",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T) # identify rows belonging to species with sp.
rows_Taxo_BioT_Marine_NoFish_gen <- grep(" genus",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T) # identify rows belonging to species with genus
rows_Taxo_BioT_Marine_NoFish_fam <- grep(" fam",Taxo_BioT_Marine_NoFish$SPECIES_Source, fixed = T) # identify rows belonging to species with fam


#Loop to remove sp, genus and fam from the some species names
species_sp <- c()
for (i in 1:nrow(Taxo_BioT_Marine_NoFish)) {
  if(length(which(rows_Taxo_BioT_Marine_NoFish_sp %in% i)) > 0) {
    species_sp [i] <- unlist(strsplit(Taxo_BioT_Marine_NoFish$SPECIES_Source[i], " "))[1]
    print(i)
  } else if(length(which(rows_Taxo_BioT_Marine_NoFish_gen %in% i)) > 0 | length(which(rows_Taxo_BioT_Marine_NoFish_fam %in% i)) > 0) {
    species_sp [i] <- unlist(strsplit(Taxo_BioT_Marine_NoFish$SPECIES_Source[i], " "))[2]
    print(i)
  }
  else {
    species_sp [i] <- Taxo_BioT_Marine_NoFish$SPECIES_Source[i]
    print(i)
  }
}

Taxo_BioT_Marine_NoFish$species_sp <- species_sp # Merge the vector with "Taxo_BioT_Marine_NoFish"
uni_specie_sp <- unique(species_sp) # unique species

head(Taxo_BioT_Marine_NoFish)
dim(Taxo_BioT_Marine_NoFish)
worms_taxo <- data.frame() # dataframe to save taxonomy results

doble_order <- c() # vector to save taxons that have two orders according worms

for(i in 12878:length(uni_specie_sp)){
  worms_taxo[i,1] <- uni_specie_sp[i] # "Taxon"
  worms_taxo[i,2] <- tryCatch(worrms::wm_name2id(name = uni_specie_sp[i]), error=function(err) NA) # "CODE_worms" (print NA in case of error)
  
  if(!is.na(worms_taxo[i,2])){
    
    worms_clas <- as.data.frame(worrms::wm_classification(worms_taxo[i,2]))
    worms_taxo[i,3] <- worms_clas[nrow(worms_clas),"scientificname"] # "SPECIES_VAL_worms"
    worms_taxo[i,4] <- ifelse(tolower(uni_specie_sp[i]) == tolower(worms_taxo[i,3]),"accepted", "synonym") # "taxonomicStatus"
    worms_taxo[i,5] <- worms_clas[nrow(worms_clas),"rank"] # "taxonRank"
    worms_taxo[i,6] <- worms_clas[which(worms_clas$rank == "Kingdom"),"scientificname"] # "kingdom"
    
    test_phylum <- length(which(worms_clas$rank == "Phylum")) # "phylum"
    if(test_phylum > 0){
      worms_taxo[i,7] <- worms_clas[which(worms_clas$rank == "Phylum"),"scientificname"]
    } else {
      worms_taxo[i,7] <- NA
    }
    
    test_class <- length(which(worms_clas$rank == "Class")) # "class"
    if(test_class > 0){
      worms_taxo[i,8] <- worms_clas[which(worms_clas$rank == "Class"),"scientificname"]
    } else {
      worms_taxo[i,8] <- NA
    }
    
    test_order <- length(which(worms_clas$rank == "Order")) # "order"
    if(test_order > 0){
      worms_taxo[i,9] <- if(length(which(worms_clas$rank == "Order")) > 1) {worms_clas[which(worms_clas$rank == "Order"),"scientificname"][2]; doble_order[i] <- i} else {worms_clas[which(worms_clas$rank == "Order"),"scientificname"]} #There are some taxons with two orders, in this cases y keep the second
    } else {
      worms_taxo[i,9] <- NA
    }
    
    test_fam <- length(which(worms_clas$rank == "Family")) # "family"
    if(test_fam > 0){
      worms_taxo[i,10] <- worms_clas[which(worms_clas$rank == "Family"),"scientificname"]
    } else {
      worms_taxo[i,10] <- NA
    }
    
    test_genus <- length(which(worms_clas$rank == "Genus")) # "genus"
    if(test_genus > 0){
      worms_taxo[i,11] <- worms_clas[which(worms_clas$rank == "Genus"),"scientificname"]
    } else {
      worms_taxo[i,11] <- NA
    }
    
    test_sp_intra <- length(unlist(strsplit(worms_clas[which(worms_clas$rank == "Species"),"scientificname"]," ")))
    if(test_sp_intra > 1) {
      worms_taxo[i,12] <- unlist(strsplit(worms_clas[which(worms_clas$rank == "Species"),"scientificname"]," "))[2] # "specificEpithet"
      worms_taxo[i,13] <- unlist(strsplit(worms_clas[which(worms_clas$rank == "Species"),"scientificname"]," "))[3] #  infraspecificEpithet"
    } else {
      worms_taxo[i,12] <- NA # "specificEpithet"
      worms_taxo[i,13] <- NA # infraspecificEpithet"
    }
    
    worms_taxo[i,14] <- "worms" # "taxonomy_source"
    worms_taxo[i,15] <- TRUE # "match_worms"
    print(i)
    
  } else {
    worms_taxo[i,3] <- NA # "SPECIES_VAL_worms"
    worms_taxo[i,4] <- NA # "taxonomicStatus"
    worms_taxo[i,5] <- NA # "taxonRank"
    worms_taxo[i,6] <- NA # "kingdom"
    worms_taxo[i,7] <- NA # "phylum"
    worms_taxo[i,8] <- NA # "class"
    worms_taxo[i,9] <- NA # "order"
    worms_taxo[i,10] <- NA # "family"
    worms_taxo[i,11] <- NA # "genus"
    worms_taxo[i,12] <- NA # "specificEpithet"
    worms_taxo[i,13] <- NA # infraspecificEpithet"
    worms_taxo[i,14] <- NA # "taxonomy_source"
    worms_taxo[i,15] <- FALSE # "match_worms"
    print(i)
  }
}

doble_order2 <- doble_order[which(!is.na(doble_order))] # row number of species that have more than one order

names(worms_taxo) <- c("species_sp","CODE_worms","SPECIES_VAL_worms","taxonomicStatus","taxonRank","kingdom","phylum","class","order","family","genus","specificEpithet","infraspecificEpithet","taxonomy_source","match_worms")
head(worms_taxo)
dim(worms_taxo)



#Merge worms taxonomy (worms_taxo) with BioT_no_gbif_Mar
Taxo_BioT_Marine_NoFish_2 <- Taxo_BioT_Marine_NoFish
Taxo_BioT_Marine_NoFish_2$CODE_worms <- NA
Taxo_BioT_Marine_NoFish_2$SPECIES_VAL_worms <- NA

# 25061, 25062, 25064 don't have values (e.i "")

for (i in 25065:nrow(Taxo_BioT_Marine_NoFish_2)) {
  
  sp_loop <- Taxo_BioT_Marine_NoFish_2$species_sp[i]
  sp_line_taxo <- which(worms_taxo$species_sp == sp_loop)
  Taxo_BioT_Marine_NoFish_2[i,"CODE_worms"] <- worms_taxo[sp_line_taxo,"CODE_worms"]
  Taxo_BioT_Marine_NoFish_2[i,"SPECIES_VAL_worms"] <- worms_taxo[sp_line_taxo,"SPECIES_VAL_worms"]
  Taxo_BioT_Marine_NoFish_2[i,"taxonomicStatus"] <- worms_taxo[sp_line_taxo,"taxonomicStatus"]
  Taxo_BioT_Marine_NoFish_2[i,"taxonRank"] <- worms_taxo[sp_line_taxo,"taxonRank"]
  Taxo_BioT_Marine_NoFish_2[i,"kingdom"] <- worms_taxo[sp_line_taxo,"kingdom"]
  Taxo_BioT_Marine_NoFish_2[i,"phylum"] <- worms_taxo[sp_line_taxo,"phylum"]
  Taxo_BioT_Marine_NoFish_2[i,"class"] <- worms_taxo[sp_line_taxo,"class"]
  Taxo_BioT_Marine_NoFish_2[i,"order"] <- worms_taxo[sp_line_taxo,"order"]
  Taxo_BioT_Marine_NoFish_2[i,"family"] <- worms_taxo[sp_line_taxo,"family"]
  Taxo_BioT_Marine_NoFish_2[i,"genus"] <- worms_taxo[sp_line_taxo,"genus"]
  Taxo_BioT_Marine_NoFish_2[i,"specificEpithet"] <- worms_taxo[sp_line_taxo,"specificEpithet"]
  Taxo_BioT_Marine_NoFish_2[i,"infraspecificEpithet"] <- worms_taxo[sp_line_taxo,"infraspecificEpithet"]
  Taxo_BioT_Marine_NoFish_2[i,"taxonomy_source"] <- worms_taxo[sp_line_taxo,"taxonomy_source"]
  Taxo_BioT_Marine_NoFish_2[i,"match_worms"] <- worms_taxo[sp_line_taxo,"match_worms"]
  print(i)
  
}

