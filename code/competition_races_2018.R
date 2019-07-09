house_results <- fread("./raw_data/results/district_overall_2018.csv") %>% 
  group_by(state_fips, district, candidate) %>% 
  filter(row_number() == 1) %>% 
  group_by(state_fips, district) %>% 
  mutate(totalvotes = sum(candidatevotes)) %>% 
  arrange(desc(candidatevotes)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(share = candidatevotes / totalvotes,
         district = gsub("District ", "", district)) %>% 
  select(share, state = state_fips, district)

saveRDS(house_results, "./temp/house_results.rds")

gubernatorial <- fread("./raw_data/results/state_overall_2018.csv") %>% 
  filter(office %in% c("Governor", "Governor & Lt Governor", "Governor / Lt. Governor",
                       "Governor and Lieutenant Governor", "Governor and Lt. Governor",
                       "Governor/Lieutenant Governor", "Governor/Lt. Governor")) %>% 
  group_by(state_fips) %>% 
  filter(row_number() == 1) %>% 
  select(state = state_fips)

senate <- fread("./raw_data/results/senate_overall_2018.csv") %>% 
  group_by(state_fips) %>% 
  filter(row_number() == 1) %>% 
  select(state = state_fips)