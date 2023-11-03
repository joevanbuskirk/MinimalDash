#### Updated Data Sources for deployment
### Get data from census
library(tidyverse)
library(epitools)

source('https://raw.githubusercontent.com/joevanbuskirk/ABSFunctions/main/GetFunctions.R')
GetFunctions(ABS_API = TRUE, LHD_Map = TRUE, ABS_Geog = TRUE)

## Limit to SLHD
SLHD <- LHD.Map[LHD.Map$Name == 'Sydney',]
SLHD <- sf::st_transform(SLHD, 4326)

## Define URL for ABS
url <- 'https://api.data.abs.gov.au'

### Going to need to suppress this somehow when deployed
apikey <- 'x9wGOTZ2X2obWGBvSAPI8o7GBN9MyZUaMR3OYCXj'

# tab <- DataKeys$URP$Table
# key <- DataKeys$URP$DataKey

## Function to access ABS API given table id
ABS_Pull_Table <- function(tab, key){

  url <- 'https://api.data.abs.gov.au'
  
  Tab <- Get_ABS_Table(tab,
                       args = key,
                       apikey = apikey)
  
  rs <- rsdmx::readSDMX(
    glue::glue("{url}/datastructure/ABS/{tab}?",
               "references=codelist&detail=referencestubs"))
  
  cl <- slot(rs@codelists, 'codelists')
  dims <- purrr::map_chr(cl, slot, name = 'id')
  dims <- dims[!grepl("ASGS|REGION|STATE|AGE", dims)]
  
  Dict <- purrr::map(
    setNames(dims, dims),
    ~as.data.frame(rsdmx::readSDMX(
      glue::glue("{url}/codelist/ABS/{.x}"))), 
    .id = 'codelistID')
  
  return(list('Table' = Tab,
              'Dict' = Dict))
  
}


### Use data codeset to label the table
ABS_label_columns <- function(tab){
  tab_tbl <- tab$Table
  tab_dict <- tab$Dict
  
  lgl_vec <- purrr::map_dbl(
    names(tab_tbl),
    function(.x){
      out <- grep(.x, names(tab_dict))
      out <- ifelse(length(out) == 0, 0, out)
      return(out)
    })
  
  Dict_vars <- names(tab_dict)[lgl_vec[lgl_vec != 0]]
  Tab_vars <- names(tab_tbl)[which(lgl_vec > 0)]
  
  Label_cols <- purrr::map_dfc(
    seq_along(Dict_vars),
    function(col){
      tab_tbl <- tab_tbl %>% 
        dplyr::mutate(dplyr::across(dplyr::all_of(Tab_vars), 
                                    as.character))
      out <- dplyr::left_join(tab_tbl,
                              tab_dict[[Dict_vars[col]]][, 1:2],
                              by = setNames('id', Tab_vars[col]))  %>% 
        dplyr::select(label.en)
      
      names(out) <- paste0(Tab_vars[col], "_lab")
      return(out)
    })
  
  return(cbind(tab_tbl, Label_cols))
}


#### Ok - we need a function that - 
### 1 - takes an indicator as the input
### 2 - Looks up the datakey and applies it
### 3 - Submits the request, and calculates the metric
### 4 - returns a summarised dataframe of the estimate
### We can use the above functions, just need to expand the capability

agevec <- '0_4+5_14+15_19+20_24+25_34+35_44+45_54+55_64+65_74+75_84+GE85'

DataKeys <- list(URP = list(Table = 'C21_T01_SA2',
                            DataKey = c('P_1', '', '', 'SA2', '1'),
                            type = 'num'),
                 URP_Age = list(Table = 'C21_T01_SA2',
                            DataKey = c(agevec, '', '', 'SA2', '1'),
                            type = 'num'),
                 BornOS = list(Table = 'C21_T01_SA2',
                               DataKey = c('B_11+B_O', '', '', 'SA2', '1'),
                               type = 'pct'),
                 LOTE = list(Table = 'C21_T01_SA2',
                            DataKey = c('L_O+L_1201', '', '', 'SA2', '1'),
                            type = 'pct'),
                 MedIncome = list(Table = 'C21_T02_SA2',
                                  DataKey = c('2', '', 'SA2', '1')),
                 LowIncome = list(Table = 'C21_T25_SA2',
                                  DataKey = c('_T', '', '', 'SA2', '1')),
                 Assist = list(Table = 'C21_T28_SA2',
                               DataKey = c(agevec, '', '', '', 'SA2', '1')),
                 Arthritis = list(Table = 'C21_G19_SA2',
                                  DataKey = c('', '11', '', '', 'SA2', '1')),
                 Asthma = list(Table = 'C21_G19_SA2',
                               DataKey = c('', '21', '', '','SA2', '1')),
                 Cancer = list(Table = 'C21_G19_SA2',
                               DataKey = c('', '31', '', '','SA2', '1')),
                 Dementia = list(Table = 'C21_G19_SA2',
                                 DataKey = c('', '41', '', '','SA2', '1')),
                 Diabetes = list(Table = 'C21_G19_SA2',
                                 DataKey = c('', '51', '', '', 'SA2', '1')),
                 Heart = list(Table = 'C21_G19_SA2',
                              DataKey = c('', '61', '', '','SA2', '1')),
                 Kidney = list(Table = 'C21_G19_SA2',
                               DataKey = c('', '71', '', '','SA2', '1')),
                 Lung = list(Table = 'C21_G19_SA2',
                             DataKey = c('', '81', '', '','SA2', '1')),
                 MH = list(Table = 'C21_G19_SA2',
                           DataKey = c('', '91', '', '','SA2', '1')),
                 Stroke = list(Table = 'C21_G19_SA2',
                               DataKey = c('', '101', '', '', 'SA2', '1')))

All_Data_Tables <- purrr::map(DataKeys, 
                              ~with(.x, ABS_Pull_Table(Table, DataKey)), 
                              .progress = TRUE)

All_Data_Tables_Formatted <- purrr::map(All_Data_Tables, ABS_label_columns)
All_Data_Tables_Formatted <- purrr::map(All_Data_Tables_Formatted, 
                                        ~dplyr::mutate(.x, 
                                                       REGION = as.character(REGION)))



unique(All_Data_Tables_Formatted$Heart$AGEP)
unique(All_Data_Tables_Formatted$Assist$AGEP)

### Now we limit all the extracted data to the SA2s we will visualise
### Also calculate the LHD level estimates

## First download 2021 SA2s
SA2_21 <- Get_ABS_Geography('SA2', 2021, 1)
GCC_21 <- Get_ABS_Geography('GCCSA', 2021, 1)

GSYD <- GCC_21[GCC_21$gccsa_code_2021 == '1GSYD', ]

## Intersect with LHDs
SA2_21_LHD <- Int_with_LHD(SA2_21)

## Simplify this into a lookup table
SA2_LHD <- sf::st_drop_geometry(SA2_21_LHD) %>% 
  dplyr::select(sa2_code_2021, sa2_name_2021, Name) 

## Crop to just NSW/transform
SA2_21_LHD_Crop <- sf::st_transform(SA2_21_LHD, 4326) 

## Let's stick to Greater Sydney
# %>% 
#   dplyr::filter(gccsa_code_2021 == '1GSYD')

## Download the standard population
## Summarise this at the age groups in the census tables
standard_pop_single <- readxl::read_xlsx(dir(file.path(Sys.getenv('OneDriveUsyd'),
                                                'JoeVB/Data/ABS/stdpops'),
                                      full.names = TRUE),
                                  sheet = 2,
                                  skip = 4) %>% 
  dplyr::select(Age = 1, stdpop = 2) %>% 
  dplyr::filter(grepl('^\\d', Age)) 


standard_pop <- standard_pop_single %>% 
  dplyr::mutate(Age = as.numeric(gsub('\\D', '', Age)),
                AGEP = dplyr::case_when(Age <= 14 ~ "0_14",
                                        Age <= 24 ~ "15_24",
                                        Age <= 34 ~ "25_34",
                                        Age <= 44 ~ "35_44",
                                        Age <= 54 ~ "45_54",
                                        Age <= 64 ~ "55_64",
                                        Age <= 74 ~ "65_74",
                                        Age <= 84 ~ "75_84",
                                        Age > 84 ~ "GE85")) %>% 
  dplyr::summarise(stdpop = sum(stdpop),
                   .by = AGEP)

standard_pop_five <- standard_pop_single %>% 
  dplyr::mutate(Age = as.numeric(gsub('\\D', '', Age)),
                AGEP = cut(Age, c(0, 5, 15, 20, seq(25, 100, 10)), 
                           right = FALSE)) %>% 
  dplyr::mutate(AGEP = paste0(min(Age), '_', max(Age)),
                AGEP = ifelse(Age >= 85, 'GE85', AGEP),
                .by = AGEP) %>%
  dplyr::summarise(stdpop = sum(stdpop),
                   .by = AGEP)



### Create a new table, with age groups consistent with the health conditions
All_Data_Tables_Formatted$URP_Age_Grps <- All_Data_Tables_Formatted$URP_Age %>% 
  dplyr::mutate(AGEP = dplyr::case_when(PCHAR %in% c('0_4', '5_14') ~ "0_14",
                                        PCHAR %in% c('15_19', '20_24') ~ "15_24",
                                        TRUE ~ PCHAR)) %>% 
  dplyr::summarise(URP = sum(OBS_VALUE),
                   .by = c(REGION, SEXP, TIME_PERIOD, AGEP))



## Age standardise the health variables
names(All_Data_Tables_Formatted)
Health_variables_Vector <- c('Arthritis', 'Asthma', 'Cancer', 'Dementia',
                             'Diabetes', 'Heart', 'Kidney', 'Lung', 'MH', 'Stroke')


Data_Tables_SA2 <- All_Data_Tables_Formatted[Health_variables_Vector] %>%
  purrr::map(function(df){
    df %>% 
      dplyr::filter(AGEP != "_T") %>% 
      dplyr::left_join(All_Data_Tables_Formatted$URP_Age_Grps,
                       by = c('REGION', 'AGEP', 'SEXP', 'TIME_PERIOD')) %>%
      dplyr::left_join(standard_pop,
                       by = c('AGEP')) %>%
      dplyr::group_nest(REGION, SEXP, TIME_PERIOD, LTHP, LTHP_lab) %>% 
      dplyr::mutate(purrr::map_df(
        data, 
        ~1e5 * epitools::ageadjust.direct(count = .x$OBS_VALUE,
                                          pop = .x$URP,
                                          stdpop = .x$stdpop)), 
        .keep = 'unused') 
  })


## Format individual tables to get proportions
#### Born overseas
Data_Tables_SA2$BornOS <- All_Data_Tables_Formatted$BornOS %>% 
  dplyr::mutate(BornOS = OBS_VALUE/sum(OBS_VALUE),
                .by = c(REGION, SEXP, TIME_PERIOD)) %>% 
  dplyr::filter(grepl("Elsewhere", PCHAR_lab))

#### Languages other than English spoken
Data_Tables_SA2$LOTE <- All_Data_Tables_Formatted$LOTE %>% 
  dplyr::mutate(LOTE = OBS_VALUE/sum(OBS_VALUE),
                .by = c(REGION, SEXP, TIME_PERIOD)) %>% 
  dplyr::filter(grepl("Other", PCHAR_lab))


All_Data_Tables_Formatted$URP_Age <- All_Data_Tables_Formatted$URP_Age %>% 
  dplyr::rename(URP = OBS_VALUE) %>%
  dplyr::select(-DATAFLOW, -REGION_TYPE, -STATE)

Data_Tables_SA2$Assist <- All_Data_Tables_Formatted$Assist %>% 
  dplyr::filter(grepl("^Has need", ASSNP_lab)) %>% 
  dplyr::left_join(All_Data_Tables_Formatted$URP_Age,
                   by = c('REGION', 'AGEP' = 'PCHAR', 'SEXP', 'TIME_PERIOD')) %>%
  dplyr::left_join(standard_pop_five,
                   by = c('AGEP')) %>%
  dplyr::group_nest(REGION, SEXP, TIME_PERIOD, ASSNP, ASSNP_lab) %>% 
  dplyr::mutate(purrr::map_df(
    data, 
    ~1e5 * epitools::ageadjust.direct(count = .x$OBS_VALUE,
                                      pop = .x$URP,
                                      stdpop = .x$stdpop)), 
    .keep = 'unused') 

## Stick to median income
Data_Tables_SA2$MedIncome <- All_Data_Tables_Formatted$MedIncome %>%
  dplyr::mutate(SEXP = 3)

Data_Tables_SA2$URP <- All_Data_Tables_Formatted$URP




## Limit to SA2s within Greater Sydney
Data_Tables_SA2 <- purrr::map(Data_Tables_SA2, function(df){
  dplyr::filter(df, REGION %in% SA2_21_LHD_Crop$sa2_code_2021)
})


### Rename the tables to make them all consistent
Data_Tables_SA2 <- purrr::map(
  Data_Tables_SA2,
  function(df){
    if(any(grepl("LOTE|BornOS", names(df)))){
      names(df) <- gsub("LOTE|BornOS", 'Value', names(df))
    } else {
      names(df) <- gsub('OBS_VALUE|adj.rate|URP', 'Value', names(df))
    }
    return(df)
  })

### Add on LHD 
Data_Tables_SA2 <- purrr::map(
  Data_Tables_SA2,
  ~dplyr::left_join(.x, SA2_LHD,
                    by = c('REGION' = 'sa2_code_2021')) %>% 
    dplyr::filter(dplyr::if_all(dplyr::matches('Value|lci|uci'),
                                is.finite)))

saveRDS(Data_Tables_SA2, 'SA2_Tables.rds')


### Compute LHD level estimates
Data_Tables_LHD <- All_Data_Tables_Formatted[Health_variables_Vector] %>%
  purrr::map(function(df){
    df %>% 
      dplyr::mutate(REGION = as.character(REGION)) %>% 
      dplyr::filter(AGEP != "_T") %>% 
      dplyr::left_join(All_Data_Tables_Formatted$URP_Age_Grps,
                       by = c('REGION', 'AGEP', 'SEXP', 'TIME_PERIOD')) %>%
      dplyr::left_join(SA2_LHD,
                       by = c('REGION' = 'sa2_code_2021')) %>%
      dplyr::summarise(dplyr::across(c(URP, OBS_VALUE),
                                     sum),
                       .by = c(Name, AGEP, SEXP, TIME_PERIOD, LTHP, LTHP_lab)) %>% 
      dplyr::left_join(standard_pop,
                       by = c('AGEP')) %>%
      dplyr::group_nest(Name, SEXP, TIME_PERIOD, LTHP, LTHP_lab) %>% 
      dplyr::mutate(purrr::map_df(
        data, 
        ~1e5 * epitools::ageadjust.direct(count = .x$OBS_VALUE,
                                          pop = .x$URP,
                                          stdpop = .x$stdpop)), 
        .keep = 'unused') 
  })


## Format individual tables to get proportions
#### Born overseas
Data_Tables_LHD$BornOS <- All_Data_Tables_Formatted$BornOS %>% 
  dplyr::left_join(SA2_LHD,
                   by = c('REGION' = 'sa2_code_2021')) %>%
  dplyr::summarise(OBS_VALUE = sum(OBS_VALUE),
                   .by = c(Name, SEXP, TIME_PERIOD, PCHAR, PCHAR_lab)) %>% 
  dplyr::mutate(BornOS = OBS_VALUE/sum(OBS_VALUE),
                .by = c(Name, SEXP, TIME_PERIOD)) %>% 
  dplyr::filter(grepl("Elsewhere", PCHAR_lab))

#### Languages other than English spoken
Data_Tables_LHD$LOTE <- All_Data_Tables_Formatted$LOTE %>% 
  dplyr::left_join(SA2_LHD,
                   by = c('REGION' = 'sa2_code_2021')) %>%
  dplyr::summarise(OBS_VALUE = sum(OBS_VALUE),
                   .by = c(Name, SEXP, TIME_PERIOD, PCHAR, PCHAR_lab)) %>% 
  dplyr::mutate(LOTE = OBS_VALUE/sum(OBS_VALUE),
                .by = c(Name, SEXP, TIME_PERIOD)) %>% 
  dplyr::filter(grepl("Other", PCHAR_lab))

### Assistance with daily activities
Data_Tables_LHD$Assist <- All_Data_Tables_Formatted$Assist %>% 
  dplyr::left_join(SA2_LHD,
                   by = c('REGION' = 'sa2_code_2021')) %>%
  dplyr::filter(grepl("^Has need", ASSNP_lab)) %>% 
  dplyr::left_join(All_Data_Tables_Formatted$URP_Age,
                   by = c('REGION', 'AGEP' = 'PCHAR', 'SEXP', 'TIME_PERIOD')) %>%
  dplyr::summarise(dplyr::across(c(URP, OBS_VALUE),
                                 sum),
                   .by = c(Name, AGEP, SEXP, TIME_PERIOD, ASSNP, ASSNP_lab)) %>% 
  dplyr::left_join(standard_pop_five,
                   by = c('AGEP')) %>%
  dplyr::group_nest(Name, SEXP, TIME_PERIOD, ASSNP, ASSNP_lab) %>% 
  dplyr::mutate(purrr::map_df(
    data, 
    ~1e5 * epitools::ageadjust.direct(count = .x$OBS_VALUE,
                                      pop = .x$URP,
                                      stdpop = .x$stdpop)), 
    .keep = 'unused') 

## Stick to median income
Data_Tables_LHD$MedIncome <- All_Data_Tables_Formatted$MedIncome %>% 
  dplyr::left_join(SA2_LHD,
                   by = c('REGION' = 'sa2_code_2021')) %>%
  dplyr::summarise(OBS_VALUE = median(OBS_VALUE),
                   .by = c(Name, TIME_PERIOD, MEDAVG, MEDAVG_lab)) %>% 
  dplyr::mutate(SEXP = 3)
  
## Sum the URP
Data_Tables_LHD$URP <- All_Data_Tables_Formatted$URP %>% 
  dplyr::left_join(SA2_LHD,
                   by = c('REGION' = 'sa2_code_2021')) %>%
  dplyr::summarise(OBS_VALUE = sum(OBS_VALUE),
                   .by = c(Name, TIME_PERIOD, SEXP, PCHAR, PCHAR_lab))  


Data_Tables_LHD <- purrr::map(
  Data_Tables_LHD,
  ~filter(.x, 
          Name %in% unique(SA2_21_LHD_Crop$Name),
          dplyr::if_all(dplyr::matches('Value|lci|uci'),
                        is.finite)))

Data_Tables_LHD <- purrr::map(
  Data_Tables_LHD,
  function(df){
    if(any(grepl("LOTE|BornOS", names(df)))){
      names(df) <- gsub("LOTE|BornOS", 'Value', names(df))
    } else {
      names(df) <- gsub('OBS_VALUE|adj.rate|URP', 'Value', names(df))
    }
    return(df)
  })


Data_Tables_LHD <- purrr::map(Data_Tables_LHD, 
                              ~dplyr::mutate(.x, 
                                             Name = gsub(" \\(.*", "", Name)))


saveRDS(Data_Tables_LHD, 'LHD_Tables.rds')


### Output SA2 map
SA2_21_Crop <- sf::st_transform(SA2_21, 4326) 
# %>% 
#   dplyr::filter(gccsa_code_2021 == "1GSYD")

SA2_21_Crop_LHD <- SA2_21_Crop %>% 
  sf::st_simplify(dTolerance = 10) %>%
  dplyr::left_join(SA2_LHD,
                   by = 'sa2_code_2021') 


SA2_21_Crop_LHD$sa2_name_2021.y <- NULL
names(SA2_21_Crop_LHD) <- gsub('sa2_name_2021.x', 
                               'sa2_name_2021', 
                               names(SA2_21_Crop_LHD))


SA2_21_Crop_LHD[!sf::st_is_empty(SA2_21_Crop_LHD),] %>% 
  terra::vect() %>%
  terra::writeVector('SA2_2021_Cropped', overwrite = TRUE)

### Output LHD map

# New LHD Map based on intersection
LHD_Crop <- sf::st_transform(sf::st_zm(LHD.Map), 4326) %>%
  sf::st_make_valid() %>%
  sf::st_simplify(dTolerance = 100) 
# %>%
#   sf::st_intersection(GSYD)

terra::writeVector(terra::vect(LHD_Crop),
                   'LHD_Cropped', 
                   overwrite = TRUE)

LHD_Cropped[LHD_Cropped$Name == 'Sydney',] %>% 
  sf::st_centroid()

## Pull in SEIFA variables
SEIFA <- readRDS(file.path(
  'c:/Users/jvan8679/NSW Health Department/SLHD PHU - Epi/PHRAME/',
  'SEIFA Time Series/data_derived/SA2_SEIFA_2011_2016_2021.rds'))


## Make the names consistent
names(Data_Tables_SA2$Arthritis)
names(SEIFA)


SEIFA_sum <- SEIFA %>% 
  dplyr::filter(STE_CODE21 == 1) %>% 
  dplyr::left_join(dplyr::distinct(Data_Tables_SA2$Arthritis,
                                   REGION, 
                                   sa2_name_2021,
                                   Name),
                   by = c('SA2_CODE21' = 'REGION')) %>% 
  dplyr::mutate(SEXP = '3') %>% 
  dplyr::select(TIME_PERIOD,
                REGION = SA2_CODE21, 
                Value = IRSD10,
                SEXP,
                INDEX_TYPE,
                Name, 
                sa2_name_2021) 

Data_Tables_SA2 <- append(Data_Tables_SA2,
                          split(SEIFA_sum, SEIFA_sum$INDEX_TYPE))

saveRDS(Data_Tables_SA2, 'SA2_Tables.rds')



### Do some sort of summary for LHD
SEIFA_Labels <- setNames(c('Relative Socio-Economic Disadvantage (IRSD)',
                           'Relative Socio-Economic Advantage and Disadvantage (IRSAD)',
                           'Education and Occupation (IEO)',
                           'Economic Resources (IER)'),
                         c('IRSD', 'IRSAD', 'IEO', 'IER'))

SEIFA_LHD <- SEIFA_sum %>% 
  dplyr::filter(!is.na(Name)) %>%
  dplyr::summarise(Value = sum(Value %in% c(7:10))/dplyr::n(),
                   .by = c(TIME_PERIOD, Name, INDEX_TYPE)) %>% 
  dplyr::mutate(TIME_PERIOD = as.character(TIME_PERIOD),
                SEIFA_Label = SEIFA_Labels[INDEX_TYPE],
                Name = gsub(" \\(.*", "", Name))

Data_Tables_LHD <- append(Data_Tables_LHD,
                         split(SEIFA_LHD, SEIFA_LHD$INDEX_TYPE))


Data_Tables_LHD <- purrr::map(
  Data_Tables_LHD,
  function(df){
    names(df) <- gsub('PropHighest', 'Value', names(df))
    return(df)
  })


Data_Tables_LHD <- purrr::imap(Data_Tables_LHD,
                               function(df, name){
                                 if(name %in% c('IEO', 'IRSAD', 'IRSD', 'IER')){
                                   dplyr::mutate(df, SEXP = '3')
                                 } else {
                                   df
                                 }
                               })


Data_Tables_LHD <- purrr::map(
  Data_Tables_LHD,
  function(df){
    names(df) <- gsub('PropHighest', 'Value', names(df))
    df$Name <- gsub(' \\(.*', '', df$Name)
    return(df)
  })


saveRDS(Data_Tables_LHD, 'LHD_Tables.rds')






















  


