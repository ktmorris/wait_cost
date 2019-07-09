# cces <- readRDS("./temp/cces_zips_census.rds")
# house_results <- readRDS("./temp/house_results.rds")
# #### read in and geocode florida and nc
# ### florida
# 
# db <- dbConnect(SQLite(), "D:/rolls.db")
# 
# inp_2016 <- dbGetQuery(db, "select Voter_ID from fl_history_0319
#                       where election_date == '11/08/2016' and history_code == 'Y'")
# 
# fl_all <- dbGetQuery(db, "select Voter_ID, election_date, history_code from fl_history_0319")
# 
# fl_all_16 <- fl_all[fl_all$Voter_ID %in% inp_2016$Voter_ID, ] ## keep only people who voted in 2016
# rm(fl_hist2)
# 
# fl_hist_18 <- filter(fl_all_16, election_date == "11/06/2018")
# 
# fl_hist_early <- fl_all %>% 
#   mutate(election_date = as.Date(election_date, "%m/%d/%Y")) %>% 
#   filter(election_date < "2016-01-01")
# 
# 
# florida <- dbGetQuery(db, "select Voter_ID, Residence_Address_Line_1, Residence_City, Name_Last, Registration_Date,
#                            Gender, Race, Birth_Date, Party_Affiliation, Residence_Zipcode, Congressional_District
#                            from fl_roll_0319") %>% 
#   mutate(street = Residence_Address_Line_1,
#          city = Residence_City,
#          zip = Residence_Zipcode,
#          state = "FL") %>% 
#   select(street, city, zip, state, Voter_ID, Gender, Race, Birth_Date, Party_Affiliation)
# 
# florida <- geocode(florida)
# 
# saveRDS(florida, "./temp/fl_geo.rds")
# 
# 
# fl <- readRDS("./temp/fl_geo.rds")
# 
### two defitions of "new voter"
### either never voted before, or has a last registration date
### between fed elections of 2014 and 2016
# 
# fl$voted_2016 <- fl$Voter_ID %in% fl_all_16$Voter_ID
# fl$voted_2018 <- fl$Voter_ID %in% fl_hist_18$Voter_ID
# fl$voted_before_16 <- fl$Voter_ID %in% fl_hist_early$Voter_ID
# 
# fl$reg_14_16 <- as.Date(fl$Registration_Date, "%m/%d/%Y") > "2014-11-04" &
#   as.Date(fl$Registration_Date, "%m/%d/%Y") < "2016-11-08"
# 
# 
# ## merge in 2018 results
# fl <- left_join(mutate(fl, state = 12),
#                 house_results,
#                 by = c("state", "Congressional_District" = "district")) %>% 
#   mutate(share = ifelse(is.na(share), 1, share))
# ####
# 
# fl$white <- fl$Race == 5
# 
# fl <- left_join(mutate(fl, zip = as.integer(zip)), cces, by = c("zip" = "zip_code")) %>% 
#   mutate(obs = ifelse(is.na(obs), 0, obs))
# 
# fl$w2 <- fl$wait ^ 2
# 
# fl <- fl %>% 
#   group_by(latitude, longitude, Name_Last) %>% 
#   mutate(fam_voted_before_2016 = max(voted_before_16)) %>% 
#   ungroup()
# 
# 
# ### keep only people who voted in 2016 AND had either never voted before
# ### or last registered to vote between 2014 and 2016
# fl <- fl[fl$voted_2016 == 1 & (fl$reg_14_16 == 1 | fl$voted_before_16 == 0), ]
# 
# saveRDS(fl, "./temp/fl_pre_reg.rds")

fl <- readRDS("./temp/fl_pre_reg.rds")

fl$long_wait <- fl$wait >= 20

small_samp <- fl[fl$voted_2016 == 1 & 
                   fl$match != "No Match" &
                   fl$reg_14_16 == 1, ]

model1 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ wait +
        white + as.factor(Party_Affiliation) + median_income +
        share + as.factor(Gender) + as.integer(substring(Birth_Date, 7)),
      data = filter(small_samp, obs > n), family = "binomial")
})


model2 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ w2 + wait +
        white + as.factor(Party_Affiliation) + median_income +
        share + as.factor(Gender) + as.integer(substring(Birth_Date, 7)),
      data = filter(small_samp, obs > n), family = "binomial")
})

model3 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ long_wait +
        white + as.factor(Party_Affiliation) + median_income +
        share + as.factor(Gender) + as.integer(substring(Birth_Date, 7)),
      data = filter(small_samp, obs > n), family = "binomial")
})


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

ggsave("./output/model1_new_regs.png")

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

ggsave("./output/model2_new_regs.png")

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

ggsave("./output/model3_new_regs.png")

###
save(model1, model2, model3,
     file = "./temp/fl_models_new_reg.rdata")

######################################################################################
small_samp <- fl[fl$voted_2016 == 1 & 
                   fl$match != "No Match" &
                   fl$voted_before_16 == 0, ]

model1 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ wait +
        white + as.factor(Party_Affiliation) + median_income +
        share + as.factor(Gender) + as.integer(substring(Birth_Date, 7)),
      data = filter(small_samp, obs > n), family = "binomial")
})


model2 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ w2 + wait +
        white + as.factor(Party_Affiliation) + median_income +
        share + as.factor(Gender) + as.integer(substring(Birth_Date, 7)),
      data = filter(small_samp, obs > n), family = "binomial")
})

model3 <- lapply(c(0:3), function(n){
  glm(voted_2018 ~ long_wait +
        white + as.factor(Party_Affiliation) + median_income +
        share + as.factor(Gender) + as.integer(substring(Birth_Date, 7)),
      data = filter(small_samp, obs > n), family = "binomial")
})


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
ggsave("./output/model1_brand_new.png")
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
ggsave("./output/model2_brand_new.png")
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
ggsave("./output/model3_brand_new.png")
###
save(model1, model2, model3,
     file = "./temp/fl_models_no_history.rdata")