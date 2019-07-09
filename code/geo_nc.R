cces <- readRDS("./temp/keep_zips.rds") %>% 
  mutate(zip_code = as.integer(zip_code))

zip_demos <- get_basic_census_stats(geo = "zcta", year = 2017) %>% 
  mutate(GEOID = as.integer(GEOID))


cces <- left_join(cces, zip_demos, by = c("zip_code" = "GEOID"))
#### read in and geocode florida and nc

db <- dbConnect(SQLite(), "D:/rolls.db")

nc_hist <- dbGetQuery(db, "select ncid, voting_method from nc_history_0219 where election_lbl == '11/08/2016'")

nc_hist2 <- dbGetQuery(db, "select ncid, election_lbl, voting_method from nc_history_0219")

nc_hist <- nc_hist2[nc_hist2$ncid %in% nc_hist$ncid, ] ## keep only people who voted in 2016
rm(nc_hist2)


nc_hist_18 <- filter(nc_hist, election_lbl == "11/06/2018")

nc_hist_early <- nc_hist %>% 
  mutate(election_lbl = as.Date(election_lbl, "%m/%d/%Y")) %>% 
  filter(election_lbl < "2016-01-01")

nc <- dbGetQuery(db, "select ncid, voter_status_desc, last_name,
                 res_street_address,
                 res_city_desc, zip_code, race_code, ethnic_code, party_cd, gender_code,
                 birth_year, cong_dist_abbrv
                 from nc_roll_0219 where voter_status_desc != 'REMOVED' and
                 state_cd == 'NC'") %>% 
  mutate(street = res_street_address,
         city = res_city_desc,
         zip = zip_code,
         state = "NC",
         last_name = toupper(last_name)) %>% 
  select(-res_street_address, -res_city_desc, -zip_code)

nc <- geocode(nc)

nc$voted_2016 <- nc$ncid %in% nc_hist$ncid
nc$voted_2018 <- nc$ncid %in% nc_hist_18$ncid
nc$voted_before_16 <- nc$ncid %in% nc_hist_early$ncid

nc <- left_join(nc, cces, by = c("zip" = "zip_code")) %>% 
  mutate(obs = ifelse(is.na(obs), 0, obs))

nc$w2 <- nc$wait ^ 2
nc$long_wait <- nc$wait > 30

nc <- nc %>% 
  group_by(latitude, longitude, last_name) %>% 
  mutate(fam_voted_before_2016 = max(voted_before_16))

saveRDS(nc, "./temp/nc.rds")

small_samp <- nc[nc$voted_2016 == 1 & nc$voted_before_16 == 0 & nc$fam_voted_before_2016 == 0, ]

model <- glm(voted_2018 ~ wait +
               as.factor(race_code) + as.factor(ethnic_code) + as.factor(party_cd) + median_income +
               as.factor(cong_dist_abbrv) + as.factor(gender_code) + birth_year,
             data = filter(nc, voted_2016 == 1, obs > 1, voted_before_16 == 0), family = "binomial")

model2 <- glm(voted_2018 ~ w2 +
               as.factor(race_code) + as.factor(ethnic_code) + as.factor(party_cd) + median_income +
               as.factor(cong_dist_abbrv) + as.factor(gender_code) + birth_year,
             data = filter(small_samp, obs > 0), family = "binomial")

model3 <- glm(voted_2018 ~ long_wait +
               as.factor(race_code) + as.factor(ethnic_code) + as.factor(party_cd) + median_income +
               as.factor(cong_dist_abbrv) + as.factor(gender_code) + birth_year,
             data = filter(small_samp, obs > 0), family = "binomial")

save(model, model2, model3, file = "./temp/nc_first_models.rdata")

### florida
florida <- dbGetQuery(db, "select Voter_ID, Residence_Address_Line_1, Residence_City,
                           Gender, Race, Birth_Date, Party_Affiliation, Residence_Zipcode
                           from fl_roll_0319") %>% 
  mutate(street = Residence_Address_Line_1,
         city = Residence_City,
         zip = Residence_Zipcode,
         state = "FL") %>% 
  select(street, city, zip, state, Voter_ID, Gender, Race, Birth_Date, Party_Affiliation)

florida <- geocode(florida)