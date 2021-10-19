
### Get Second Wave data

datapath <- "1_input/D-P21-13185 HU Berlin Impfskeptiker_Welle 2_Finaler_Datensatz.sav"

df_2 <- read_sav(datapath)%>%
  dplyr::rename(
    treatment_video = c_0014,
    vignette1 = c_0031,
    vignette2 = c_0032,
    vignette3 = c_0033,
    vignette4 = c_0034,
    vignette5 = c_0035,
    vignette6 = c_0036,
    outcome_donation = v_282,
    conjoint_choice1 = v_328,
    conjoint_choice2 = v_329,
    conjoint_choice3 = v_333,
    conjoint_rating1 = v_321,
    conjoint_rating2 = v_322,
    conjoint_rating3 = v_331,
    conjoint_rating4 = v_332,
    conjoint_rating5 = v_335,
    conjoint_rating6 = v_336,
    sign_letter =  v_241,
    incumbent_voting = v_327,
    post.performance= v_228,
    mc.performance.germany = v_224,
    mc.performance.uk = v_225,
    mc.performance.au = v_226,
    satisvaction.CDU= v_230,
    satisvaction.CSU= v_231,
    satisvaction.SPD= v_232,
    satisvaction.Merkel= v_233,
    satisvaction.Spahn= v_236,
    satisvaction.PM= v_237,
    satisvaction.vdLeyen= v_238,
    satisvaction.mayor= v_239
  ) %>%
  # rename covariates
  rename_at(vars(var_list_2$var_name[var_list_2$transformed==0]), ~  var_list_2$new_name[var_list_2$transformed==0]) %>%
  dplyr::mutate(
    # 1: the highest vaccination rate, 3: the lowest vaccination rate)
    mc.performance.germany = dplyr::recode(mc.performance.germany,
                                           "1" = "3",
                                           "2" = "2",
                                           "3" = "1"),
    mc.performance.uk = dplyr::recode(mc.performance.uk,
                                      "1" = "3",
                                      "2" = "2",
                                      "3" = "1"),
    mc.performance.au = dplyr::recode(mc.performance.au,
                                      "1" = "3",
                                      "2" = "2",
                                      "3" = "1"),
    incumbent_voting = dplyr::recode(incumbent_voting,
                                     "1" = "1",
                                     "2" = "0"),
    letter = dplyr::recode(v_240,
                           "1" = "1",
                           "2" = "-1",
                           "3" = "0"),
    status = ifelse(
      is.na(vaccination.intent),
      "Vaccinated",
      paste(vaccination.intent)
    ),
    status = dplyr::recode(
      status,
      "1" = "Acceptant",
      "2" = "Hesitant",
      "3" = "Undecided"
    ),
    # covariates
    covid.media = 1-(covid.media-1)/4,
    vaccinated = 1*(vaccination == 1),
    network.vaccinated = (network.vaccinated-1)/4,
    covid.infection = 1*(covid.infection == 1),
    covid.infection.proximity = (covid.infection.proximity-1)/2,
    covid.information = 1- (covid.information-1)/4,
    covid.rules.mask = 1- (covid.rules.mask-1)/5,
    covid.rules.distance = 1- (covid.rules.distance-1)/5,
    support.distance = 1-(support.distance-1)/4,
    covid.income = 1-(covid.income-1)/4,
    political.interest = 1 - (political.interest-1)/3,
    left.right = (left.right-1)/10,
    CDU.CSU = 1*(party.id <= 2),
    CDU.CSU = ifelse(is.na(CDU.CSU), 0, CDU.CSU),
    SPD = 1*(party.id ==3),
    SPD = ifelse(is.na(SPD), 0, SPD),
    AfD = 1*(party.id ==4),
    AfD = ifelse(is.na(AfD), 0, AfD),
    Greens = 1*(party.id ==5),
    Greens = ifelse(is.na(Greens), 0, Greens),
    FDP = 1*(party.id ==6),
    FDP = ifelse(is.na(FDP), 0, FDP),
    Left = 1*(party.id ==7),
    Left = ifelse(is.na(Left), 0, Left),
    No.party = 1*(party.id == 9),
    No.party = ifelse(is.na(No.party), 0, No.party),
    solidarity = solidarity/10,
    international.solidarity = international.solidarity/10,
    EU.support = EU.support/10,
    migration.support = migration.support/10,
    trust = trust/10,
    trust.government = trust.government/4,
    trust.experts = trust.experts/4,
    trust.country = trust.country/4,
    trust.media =  trust.media/4,
    trust.healthcare = trust.healthcare/4,
    acceptant = 1*(vaccination.intent == 1),
    acceptant = ifelse(is.na(vaccination.intent), 1, acceptant),
    hesitant = vaccination.intent == 2,
    hesitant = ifelse(is.na(vaccination.intent), 0, hesitant),
    undecided = 1*(vaccination.intent == 3),
    undecided = ifelse(is.na(vaccination.intent), 0, undecided),
    health = (health - 1)/6,
    health2 = 1*(health2 == 1),
    # conjoint
    solidarity_bahaviour = (6 - as.numeric(outcome_donation))/5,  # now higher values are more solidarity (check)
    treatment_video=1*(treatment_video ==2), # 2=no video
    prioritize_germany = international.solidarity/10,
    solidarity_attitude = dplyr::recode(v_351,
                                        "1" = 1,
                                        "2" = 0),
    factor = dplyr::recode(c_0015,
                           "1" = "Negative Sociotropic Benchmark",
                           "2" = "Positive Sociotropic Benchmark",
                           "3" = "Negative Egotropic Benchmark",
                           "4" = "Positive Egotropic Benchmark",
                           "5" = "Sociotropic",
                           "6" = "Egotropic",
                           "7" = "Two-way Benchmark Sociotropic",
                           "8" = "Two-way Benchmark Egotropic",
                           "9" = "Control",#  groups 9 and 10 are control
                           "10" = "Control"),
    positive =ifelse(factor== "Positive Sociotropic Benchmark" |
                       factor== "Positive Egotropic Benchmark"
                     , 1, 0),
    negative =ifelse(factor== "Negative Sociotropic Benchmark" |
                       factor== "Negative Egotropic Benchmark"
                     , 1, 0),
    egotropic =ifelse(factor== "Egotropic" |
                        factor== "Positive Egotropic Benchmark" |
                        factor== "Two-way Benchmark Egotropic" |
                        factor== "Negative Egotropic Benchmark"
                      , 1, 0),
    sociotropic =ifelse(factor== "Sociotropic" |
                          factor== "Positive Sociotropic Benchmark" |
                          factor== "Two-way Benchmark Sociotropic" |
                          factor== "Negative Sociotropic Benchmark"
                        , 1, 0)
  )

