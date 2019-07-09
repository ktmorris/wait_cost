# cces <- readRDS("./temp/cces_zips_census.rds")
# house_results <- readRDS("./temp/house_results.rds")
# # #### read in and geocode florida and nc
# 
# db <- dbConnect(SQLite(), "D:/rolls.db")
# 
# inp_2016 <- dbGetQuery(db, "select ncid, voting_method from nc_history_0219
#                                    where election_lbl == '11/08/2016'")
# 
# nc_all <- dbGetQuery(db, "select ncid, election_lbl, voting_method from nc_history_0219")
# 
# nc_all_16 <- nc_all[nc_all$ncid %in% inp_2016$ncid, ] ## keep only people who voted in 2016
# 
# nc_hist_18 <- filter(nc_all_16, election_lbl == "11/06/2018") ## keep people who voted in 16, 18
# 
# nc_hist_early <- nc_all %>%
#   mutate(election_lbl = as.Date(election_lbl, "%m/%d/%Y")) %>%
#   filter(election_lbl < "2016-01-01")
# 
# nc <- dbGetQuery(db, "select ncid, voter_status_desc, last_name, registr_dt
#                  res_street_address,
#                  res_city_desc, zip_code, race_code, ethnic_code, party_cd, gender_code,
#                  birth_year, cong_dist_abbrv
#                  from nc_roll_0219 where voter_status_desc != 'REMOVED' and
#                  state_cd == 'NC'") %>% 
#   mutate(street = res_street_address,
#          city = res_city_desc,
#          zip = zip_code,
#          state = "NC",
#          last_name = toupper(last_name)) %>% 
#   select(-res_street_address, -res_city_desc, -zip_code)
# 
# nc <- geocode(nc)
# saveRDS(nc, "./temp/nc_geo.rds")
# 
#nc <- readRDS("./temp/nc_geo.rds")
# 
# ## merge in 2018 results
# nc <- left_join(mutate(nc, state = 37, cong_dist_abbrv = as.character(cong_dist_abbrv)),
#                 house_results,
#                 by = c("state", "cong_dist_abbrv" = "district")) %>%
#   mutate(share = ifelse(is.na(share), 1, share))
# ####
# nc$white <- nc$race_code == "W" & nc$ethnic_code == "NL"
# 
# 
# nc$voted_2016 <- nc$ncid %in% inp_2016$ncid
# nc$voted_2016_person <- nc$ncid %in% filter(inp_2016, voting_method == "IN-PERSON")$ncid
# nc$voted_2018 <- nc$ncid %in% nc_hist_18$ncid
# nc$voted_before_16 <- nc$ncid %in% nc_hist_early$ncid
# 
# nc$reg_14_16 <- as.Date(nc$registr_dt, "%m/%d/%Y") > "2014-11-04" &
#   as.Date(nc$registr_dt, "%m/%d/%Y") < "2016-11-08"
# 
# nc <- left_join(nc, cces, by = c("zip" = "zip_code")) %>%
#   mutate(obs = ifelse(is.na(obs), 0, obs))
# 
# nc$w2 <- nc$wait ^ 2
# 
# nc <- nc %>%
#   group_by(latitude, longitude, last_name) %>%
#   mutate(fam_voted_before_2016 = max(voted_before_16)) %>%
#   ungroup()
# 
# nc <- nc[nc$voted_2016 == 1 & (nc$reg_14_16 == 1 | nc$voted_before_16 == 0), ]
# 
# saveRDS(nc, "./temp/nc_pre_reg.rds")

nc <- readRDS("./temp/nc_pre_reg.rds")

nc$long_wait <- nc$wait >= 20

small_samp <- nc[nc$voted_2016_person == 1 & 
                   nc$reg_14_16 == 1, ]

model1 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ wait +
        white + as.factor(party_cd) + median_income +
        share + as.factor(gender_code) + birth_year,
      data = filter(small_samp, obs > n), family = "binomial")
})

model2 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ w2 + wait +
    white + as.factor(party_cd) + median_income +
    share + as.factor(gender_code) + birth_year,
  data = filter(small_samp, obs > n), family = "binomial")
})

model3 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ long_wait +
        white + as.factor(party_cd) + median_income +
        share + as.factor(gender_code) + birth_year,
      data = filter(small_samp, obs > n), family = "binomial")
})
####
model1_p <- rbindlist(lapply(c(1:4), function(m){
  filter(broom::tidy(model1[[m]]), term == "wait") %>% 
    mutate(term = paste0("Model ", m, ": Wait Time (Minutes)"))
}))

dw_plot(model1_p) +
  geom_vline(xintercept = 0) + 
  ggtitle("Voters Registered between 2014 and 2016") +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(caption = "95% confidence interval shown
  Model number corresponds to minimum number
       of CCES respondents in zipcode.")

ggsave("./output/model1_new_regs_nc.png")

#################
model2_p <- rbindlist(lapply(c(1:4), function(m){
  filter(broom::tidy(model2[[m]]), term == "w2") %>% 
    mutate(term = paste0("Model ", m, ": Wait Time (Minutes)^2"))
}))

dw_plot(model2_p) +
  geom_vline(xintercept = 0) + 
  ggtitle("Voters Registered between 2014 and 2016") +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(caption = "95% confidence interval shown
  Model number corresponds to minimum number
       of CCES respondents in zipcode.")

ggsave("./output/model2_new_regs_nc.png")

#################
model3_p <- rbindlist(lapply(c(1:4), function(m){
  filter(broom::tidy(model3[[m]]), term == "long_waitTRUE") %>% 
    mutate(term = paste0("Model ", m, ": D(Long Wait)"))
}))

dw_plot(model3_p) +
  geom_vline(xintercept = 0) + 
  ggtitle("Voters Registered between 2014 and 2016") +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(caption = "95% confidence interval shown
  Model number corresponds to minimum number
       of CCES respondents in zipcode.")

ggsave("./output/model3_new_regs_nc.png")

###
save(model1, model2, model3,
     file = "./temp/nc_models_new_reg.rdata")
##########################################################################
small_samp <- nc[nc$voted_2016 == 1 & 
                   nc$voted_before_16 == 0, ]

model1 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ wait +
        white + as.factor(party_cd) + median_income +
        share + as.factor(gender_code) + birth_year,
      data = filter(small_samp, obs > n), family = "binomial")
})

model2 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ w2 + wait +
        white + as.factor(party_cd) + median_income +
        share + as.factor(gender_code) + birth_year,
      data = filter(small_samp, obs > n), family = "binomial")
})

model3 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ long_wait +
        white + as.factor(party_cd) + median_income +
        share + as.factor(gender_code) + birth_year,
      data = filter(small_samp, obs > n), family = "binomial")
})
####
model1_p <- rbindlist(lapply(c(1:4), function(m){
  filter(broom::tidy(model1[[m]]), term == "wait") %>% 
    mutate(term = paste0("Model ", m, ": Wait Time (Minutes)"))
}))

dw_plot(model1_p) +
  geom_vline(xintercept = 0) + 
  ggtitle("First-Time Voters") +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(caption = "95% confidence interval shown
  Model number corresponds to minimum number
       of CCES respondents in zipcode.")

ggsave("./output/model1_brand_new_nc.png")

#################
model2_p <- rbindlist(lapply(c(1:4), function(m){
  filter(broom::tidy(model2[[m]]), term == "w2") %>% 
    mutate(term = paste0("Model ", m, ": Wait Time (Minutes)^2"))
}))

dw_plot(model2_p) +
  geom_vline(xintercept = 0) + 
  ggtitle("First-Time Voters") +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(caption = "95% confidence interval shown
  Model number corresponds to minimum number
       of CCES respondents in zipcode.")

ggsave("./output/model2_brand_new_nc.png")

#################
model3_p <- rbindlist(lapply(c(1:4), function(m){
  filter(broom::tidy(model3[[m]]), term == "long_waitTRUE") %>% 
    mutate(term = paste0("Model ", m, ": D(Long Wait)"))
}))

dw_plot(model3_p) +
  geom_vline(xintercept = 0) + 
  ggtitle("First-Time Voters") +
  theme(plot.title = element_text(hjust = 0.3)) +
  labs(caption = "95% confidence interval shown
  Model number corresponds to minimum number
       of CCES respondents in zipcode.")

ggsave("./output/model3_brand_new_nc.png")

###
save(model1, model2, model3,
     file = "./temp/nc_models_brand_new.rdata")