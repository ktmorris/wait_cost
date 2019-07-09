

cces_2016 <- readstata13::read.dta13("./raw_data/cces/CCES16_Common_OUTPUT_Feb2018_VV.dta")

## set up conversion from answer to minutes
times <- data.frame("resp" = c("Not at all", "Less than 10 minutes", "10 - 30 minutes", "31 minutes - 1 hour"),
                    "minutes" = c(0, 5, 20, 45))

long <- fread("./raw_data/misc/free_wait_16.csv")


### pull people who voted at polls
voted_at_polls <- cces_2016 %>% 
  filter(CC16_403 %in% c("In person on election day"),
         !is.na(CC16_403),
         CC16_404 != "Don't know") %>% 
  select(weight = commonweight_vv_post,
         wait_cat = CC16_404,
         wait_free = CC16_404_t,
         birthyr,
         gender,
         educ,
         race,
         race_other,
         hispanic,
         marital = marstat,
         zip_code = lookupzip,
         county_fips = countyfips_post,
         state = inputstate_post,
         family_income = faminc,
         party = pid3,
         vote_type = CC16_403)


voted_at_polls <- left_join(voted_at_polls, times, by = c("wait_cat" = "resp"))

voted_at_polls <- left_join(voted_at_polls, long, by = "wait_free") %>% 
  mutate(minutes = ifelse(is.na(time), minutes, time)) %>% 
  select(-time, -wait_free)

saveRDS(voted_at_polls, "./temp/waits_2016.rds")

### just nc
pop <- get_acs("zcta", variables = "B01001_001") %>% 
  select(GEOID, estimate)

nc <- voted_at_polls %>% 
  filter(state == "North Carolina") %>% 
  group_by(zip_code) %>% 
  summarize(obs = n())

nc <- left_join(nc, pop, by = c("zip_code" = "GEOID")) %>% 
  group_by(obs) %>% 
  summarize(zip_count = n(),
            pop = sum(estimate, na.rm = T)) %>% 
  arrange(-obs) %>% 
  mutate(cum_share = cumsum(pop) / sum(pop))

####

keep_zips <- voted_at_polls %>% 
  filter(state %in% c("North Carolina", "Florida")) %>% 
  group_by(zip_code) %>% 
  summarize(obs = n(),
            wait = weighted.mean(minutes, weight))

saveRDS(keep_zips, "./temp/keep_zips.rds")