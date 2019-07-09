cces <- readRDS("./temp/keep_zips.rds") %>% 
  mutate(zip_code = as.integer(zip_code))

zip_demos <- get_basic_census_stats(geo = "zcta", year = 2017) %>% 
  mutate(GEOID = as.integer(GEOID))


cces <- left_join(cces, zip_demos, by = c("zip_code" = "GEOID"))

saveRDS(cces, "./temp/cces_zips_census.rds")