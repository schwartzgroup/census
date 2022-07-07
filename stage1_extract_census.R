#!/usr/bin/env Rscript

source("util.R")

# Formulas ----------------------------------------------------------------

## SF1 (2000) ----

all_formulas_sf1_2000 <- list(
  
  # Universe: Total population
  total_population = c(
    population ~ P001001,
    
    # Race + ethnicity
    pct_female ~ P012026 / P001001,
    pct_white ~ P003003 / P001001,
    pct_black ~ P003004 / P001001,
    pct_native ~ P003005 / P001001,
    pct_asian ~ P003006 / P001001,
    pct_two_or_more_races ~ P003008 / P001001,
    pct_hispanic_white ~ P008011 / P001001,
    pct_hispanic_black ~ P008012 / P001001,
    pct_hispanic_native ~ P008013 / P001001,
    pct_hispanic_asian ~ P008014 / P001001,
    pct_hispanic_two_or_more_races ~ P008017 / P001001,
    pct_non_hispanic_white ~ P008003 / P001001,
    pct_non_hispanic_black ~ P008004 / P001001,
    pct_non_hispanic_native ~ P008005 / P001001,
    pct_non_hispanic_asian ~ P008006 / P001001,
    pct_non_hispanic_two_or_more_races ~ P008009 / P001001,
    pct_hispanic ~ P008010 / P001001,
    # pct_foreign_born ~ B05006_001 / P001001,
    
    # Compatibility with 1990 Decennial Census
    pct_asian_pacific_islander ~ (P003006 + P003007) / P001001,
    pct_hispanic_asian_pacific_islander ~ (P008014 + P008015) / P001001,
    pct_non_hispanic_asian_pacific_islander ~ (P008006 + P008007) / P001001,

    # Age
    pct_age_under_5 ~ (P012003 + P012027) / P001001,
    pct_age_5_to_9 ~ (P012004 + P012028) / P001001,
    pct_age_10_to_14 ~ (P012005 + P012029) / P001001,
    pct_age_15_to_17 ~ (P012006 + P012030) / P001001,
    pct_age_18_to_19 ~ (P012007 + P012031) / P001001,
    pct_age_20 ~ (P012008 + P012032) / P001001,
    pct_age_21 ~ (P012009 + P012033) / P001001,
    pct_age_22_to_24 ~ (P012010 + P012034) / P001001,
    pct_age_25_to_29 ~ (P012011 + P012035) / P001001,
    pct_age_30_to_34 ~ (P012012 + P012036) / P001001,
    pct_age_35_to_39 ~ (P012013 + P012037) / P001001,
    pct_age_40_to_44 ~ (P012014 + P012038) / P001001,
    pct_age_45_to_49 ~ (P012015 + P012039) / P001001,
    pct_age_50_to_54 ~ (P012016 + P012040) / P001001,
    pct_age_55_to_59 ~ (P012017 + P012041) / P001001,
    pct_age_60_to_61 ~ (P012018 + P012042) / P001001,
    pct_age_62_to_64 ~ (P012019 + P012043) / P001001,
    pct_age_65_to_66 ~ (P012020 + P012044) / P001001,
    pct_age_67_to_69 ~ (P012021 + P012045) / P001001,
    pct_age_70_to_74 ~ (P012022 + P012046) / P001001,
    pct_age_75_to_79 ~ (P012023 + P012047) / P001001,
    pct_age_80_to_84 ~ (P012024 + P012048) / P001001,
    pct_age_over_85 ~ (P012025 + P012049) / P001001,
    
    # Compatibility with 1990 Decennial Census
    pct_age_65_to_69 ~ (P012020 + P012021 + P012044 + P012045) / P001001
  ),
  
  # Universe: Households
  households = c(
    n_households ~ P015001,
    mean_household_size ~ P017001,
    pct_households_single_father ~ P018003 / P015001,
    pct_households_single_mother ~ P018004 / P015001
    # pct_public_assistance ~ SF3 only
  ),
  
  # Universe: Occupied housing units
  occupied_housing_units = c(
    n_occupied_housing_units ~ H010001,
    pct_renting ~ H004003 / H010001
    # pct_heating_utility_gas ~ SF3 only
    # pct_heating_gas_tank ~ SF3 only
    # pct_heating_electricity ~ SF3 only
    # pct_heating_oil ~ SF3 only
    # pct_heating_coal ~ SF3 only
    # pct_heating_wood ~ SF3 only
    # pct_heating_solar ~ SF3 only
    # pct_heating_other ~ SF3 only
    # pct_heating_none ~ SF3 only
  ),
  
  # Universe: Housing units
  housing_units = c(
    n_housing_units ~ H001001
    # pct_housing_standalone ~ SF3 only
    # pct_housing_1_unit ~ SF3 only
    # pct_housing_2_units ~ SF3 only
    # pct_housing_3_to_4_units ~ SF3 only
    # pct_housing_5_to_9_units ~ SF3 only
    # pct_housing_10_to_19_units ~ SF3 only
    # pct_housing_20_to_49_units ~ SF3 only
    # pct_housing_gt_50_units ~ SF3 only
    # pct_housing_mobile_home ~ SF3 only
    # pct_housing_vehicle ~ SF3 only
    # pct_built_2014_onwards ~ SF3 only
    # pct_built_2010_to_2013 ~ SF3 only
    # pct_built_2000_to_2009 ~ SF3 only
    # pct_built_1990_to_1999 ~ SF3 only
    # pct_built_1980_to_1989 ~ SF3 only
    # pct_built_1970_to_1979 ~ SF3 only
    # pct_built_1960_to_1969 ~ SF3 only
    # pct_built_1950_to_1959 ~ SF3 only
    # pct_built_1940_to_1949 ~ SF3 only
    # pct_built_before_1939 ~ SF3 only
    # med_year_built ~ SF3 only
  ),
  
  # Universe: Population 16 years and over (employment)
  # SF3 only
  
  # Universe: Population 18 years and over (education)
  # SF3 only
  
  # Universe: Workers 16 years and over (transportation)
  # SF3 only
  
  # Universe: Workers 16 years and over who did not work at home
  # SF3 only
  
  # Currency-based variables
  # SF3 only
  
  indices = c(
    # Ethnic fractionalization
    #
    # Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R.
    # (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194.
    # https://doi.org/10.1023/A:1024471506938
    #
    # ethnic_fractionalization = 1 - (
    #   (non_hispanic_white / population)^2 +
    #   (non_hispanic_black / population)^2 +
    #   (non_hispanic_asian / population)^2 +
    #   (hispanic / population)^2
    # )
    ethnic_fractionalization ~ 1 - (
      (P008003 / P001001)^2 +
      (P008004 / P001001)^2 +
      (P008006 / P001001)^2 +
      (P008010 / P001001)^2
    )
    
    # Townsend Index
    # SF3 only
    
  )
  
)

## SF3 (2000) ----

all_formulas_sf3 <- list(
  
  # Universe: Total population
  total_population = c(
    population ~ P001001,
    
    # Race + ethnicity
    pct_female ~ P008041 / P001001,
    pct_white ~ P006002 / P001001,
    pct_black ~ P006003 / P001001,
    pct_native ~ P006004 / P001001,
    pct_asian ~ P006005 / P001001,
    pct_two_or_more_races ~ P006008 / P001001,
    pct_hispanic_white ~ P007011 / P001001,
    pct_hispanic_black ~ P007012 / P001001,
    pct_hispanic_native ~ P007013 / P001001,
    pct_hispanic_asian ~ P007014 / P001001,
    pct_hispanic_two_or_more_races ~ P007017 / P001001,
    pct_non_hispanic_white ~ P007003 / P001001,
    pct_non_hispanic_black ~ P007004 / P001001,
    pct_non_hispanic_native ~ P007005 / P001001,
    pct_non_hispanic_asian ~ P007006 / P001001,
    pct_non_hispanic_two_or_more_races ~ P007009 / P001001,
    pct_hispanic ~ P007010 / P001001,
    pct_foreign_born ~ PCT019001 / P001001,
    
    # Compatibility with 1990 Decennial Census
    pct_asian_pacific_islander ~ (P006005 + P006006) / P001001,
    pct_hispanic_asian_pacific_islander ~ (P007014 + P007015) / P001001,
    pct_non_hispanic_asian_pacific_islander ~ (P007006 + P007007) / P001001,

    # Age
    pct_age_under_5 ~ (
      P008003 + P008004 + P008005 + P008006 + P008007 +
      P008042 + P008043 + P008044 + P008045 + P008046
    ) / P001001,
    pct_age_5_to_9 ~ (
      P008008 + P008009 + P008010 + P008011 + P008012 +
      P008047 + P008048 + P008049 + P008050 + P008051
    ) / P001001,
    pct_age_10_to_14 ~ (
      P008013 + P008014 + P008015 + P008016 + P008017 +
      P008052 + P008053 + P008054 + P008055 + P008056
    ) / P001001,
    pct_age_15_to_17 ~ (
      P008018 + P008019 + P008020 +
      P008057 + P008058 + P008059
    ) / P001001,
    pct_age_18_to_19 ~ (P008021 + P008022 + P008060 + P008061) / P001001,
    pct_age_20 ~ (P008023 + P008062) / P001001,
    pct_age_21 ~ (P008024 + P008063) / P001001,
    pct_age_22_to_24 ~ (P008025 + P008064) / P001001,
    pct_age_25_to_29 ~ (P008026 + P008065) / P001001,
    pct_age_30_to_34 ~ (P008027 + P008066) / P001001,
    pct_age_35_to_39 ~ (P008027 + P008067) / P001001,
    pct_age_40_to_44 ~ (P008029 + P008068) / P001001,
    pct_age_45_to_49 ~ (P008030 + P008069) / P001001,
    pct_age_50_to_54 ~ (P008031 + P008070) / P001001,
    pct_age_55_to_59 ~ (P008032 + P008071) / P001001,
    pct_age_60_to_61 ~ (P008033 + P008072) / P001001,
    pct_age_62_to_64 ~ (P008034 + P008073) / P001001,
    pct_age_65_to_66 ~ (P008035 + P008074) / P001001,
    pct_age_67_to_69 ~ (P008036 + P008075) / P001001,
    pct_age_70_to_74 ~ (P008037 + P008076) / P001001,
    pct_age_75_to_79 ~ (P008038 + P008077) / P001001,
    pct_age_80_to_84 ~ (P008039 + P008078) / P001001,
    pct_age_over_85 ~ (P008040 + P008079) / P001001,
    
    # Compatibility with 1990 Decennial Census
    pct_age_65_to_69 ~ (P008035 + P008036 + P008074 + P008075) / P001001
  ),
  
  # Universe: Households
  households = c(
    n_households ~ P014001,
    # mean_household_size ~ moved to Occupied housing units universe for SF3
    # pct_households_single_father ~ moved to Occupied housing units universe for SF3
    # pct_households_single_mother ~ moved to Occupied housing units universe for SF3
    pct_public_assistance ~ P064002 / P014001
  ),
  
  # Universe: Occupied housing units
  occupied_housing_units = c(
    n_occupied_housing_units ~ H007001,
    mean_household_size ~ H018001,
    pct_households_single_father ~ (H019013 + H019075) / H007001,
    pct_households_single_mother ~ (H019021 + H019083) / H007001,
    pct_renting ~ H007003 / H007001,
    pct_heating_utility_gas ~ H040002 / H007001,
    pct_heating_gas_tank ~ H040003 / H007001,
    pct_heating_electricity ~ H040004 / H007001,
    pct_heating_oil ~ H040005 / H007001,
    pct_heating_coal ~ H040006 / H007001,
    pct_heating_wood ~ H040007 / H007001,
    pct_heating_solar ~ H040008 / H007001,
    pct_heating_other ~ H040009 / H007001,
    pct_heating_none ~ H040010 / H007001
  ),
  
  # Universe: Housing units
  housing_units = c(
    n_housing_units ~ H030001,
    pct_housing_standalone ~ H030002 / H030001,
    pct_housing_1_unit ~ H030003 / H030001,
    pct_housing_2_units ~ H030004 / H030001,
    pct_housing_3_to_4_units ~ H030005 / H030001,
    pct_housing_5_to_9_units ~ H030006 / H030001,
    pct_housing_10_to_19_units ~ H030007 / H030001,
    pct_housing_20_to_49_units ~ H030008 / H030001,
    pct_housing_gt_50_units ~ H030009 / H030001,
    pct_housing_mobile_home ~ H030010 / H030001,
    pct_housing_vehicle ~ H030011 / H030001,
    # pct_built_2014_onwards ~ not available in SF3 (2000)
    # pct_built_2010_to_2013 ~ not available in SF3 (2000)
    # pct_built_2000_to_2009 ~ not available in SF3 (2000)
    pct_built_1999_to_2000 ~ H034002 / H030001,
    pct_built_1990_to_1998 ~ (H034003 + H034004) / H030001,
    pct_built_1980_to_1989 ~ H034005 / H030001,
    pct_built_1970_to_1979 ~ H034006 / H030001,
    pct_built_1960_to_1969 ~ H034007 / H030001,
    pct_built_1950_to_1959 ~ H034008 / H030001,
    pct_built_1940_to_1949 ~ H034009 / H030001,
    pct_built_before_1939 ~ H034010 / H030001,
    med_year_built ~ H035001,
    
    # Compatibility with ACS - estimate 1990-1999 as:
    # ([1990-1994] + [1995-1998] + ([1999-2000] / 2)) / Total
    pct_built_1990_to_1998 ~ (H034004 + H034003 + (H034002 / 2)) / H030001
  ),
  
  # Universe: Population 16 years and over (employment)
  pop_16y_and_over = c(
    # (Unemployed by sex) / (Labor force by sex)
    pct_unemployed ~ (P043007 + P043014) / (P043003 + P043010)
  ),
  
  # Universe: Population 18 years and over (education)
  pop_18y_and_over = c(
    pct_edu_lt_9th_grade ~ (
      PCT025004 + PCT025012 + PCT025020 + PCT025028 + PCT025036 +
      PCT025045 + PCT025053 + PCT025061 + PCT025069 + PCT025077
    ) / PCT025001,
    pct_edu_9th_to_12th_grade ~ (
      PCT025005 + PCT025013 + PCT025021 + PCT025029 + PCT025037 +
      PCT025046 + PCT025054 + PCT025062 + PCT025070 + PCT025078
    ) / PCT025001,
    pct_edu_high_school ~ (
      PCT025006 + PCT025014 + PCT025022 + PCT025030 + PCT025038 +
      PCT025047 + PCT025055 + PCT025063 + PCT025071 + PCT025079
    ) / PCT025001,
    pct_edu_some_college ~ (
      PCT025007 + PCT025015 + PCT025023 + PCT025031 + PCT025039 +
      PCT025048 + PCT025056 + PCT025064 + PCT025072 + PCT025080
    ) / PCT025001,
    pct_edu_associate ~ (
      PCT025008 + PCT025016 + PCT025024 + PCT025032 + PCT025040 +
      PCT025049 + PCT025057 + PCT025065 + PCT025073 + PCT025081
    ) / PCT025001,
    pct_edu_bachelors ~ (
      PCT025009 + PCT025017 + PCT025025 + PCT025033 + PCT025041 +
      PCT025050 + PCT025058 + PCT025066 + PCT025074 + PCT025082
    ) / PCT025001,
    pct_edu_graduate_or_professional ~ (
      PCT025010 + PCT025018 + PCT025026 + PCT025034 + PCT025042 +
      PCT025051 + PCT025059 + PCT025067 + PCT025075 + PCT025083
    ) / PCT025001
  ),
  
  # Universe: Workers 16 years and over (transportation)
  workers_16y_and_over = c(
    pct_transport_auto ~ P030002 / P030001,
    pct_transport_auto_alone ~ P030003 / P030001,
    pct_transport_auto_carpool ~ P030004 / P030001,
    pct_transport_public_transit ~ P030005 / P030001,
    pct_transport_bus ~ P030005 / P030001,
    pct_transport_streetcar ~ P030007 / P030001,
    pct_transport_subway ~ P030008 / P030001,
    pct_transport_rail ~ P030009 / P030001,
    pct_transport_ferry ~ P030010 / P030001,
    pct_transport_taxi ~ P030011 / P030001,
    pct_transport_motorcycle ~ P030012 / P030001,
    pct_transport_bicycle ~ P030013 / P030001,
    pct_transport_walk ~ P030014 / P030001,
    pct_transport_other ~ P030015 / P030001,
    pct_transport_wfh ~ P030016 / P030001
  ),
  
  # Universe: Workers 16 years and over who did not work at home
  #
  # NOTE: Variable is actually located in the "Workers 16 years and over"
  # universe, but all of these variables are nested under the "Did not work at
  # home" category (P031002)
  workers_16y_and_over_not_wfh = c(
    pct_travel_lt_5_min ~ P031003 / P031002,
    pct_travel_5_to_9_min ~ P031004 / P031002,
    pct_travel_10_to_14_min ~ P031005 / P031001,
    pct_travel_15_to_19_min ~ P031006 / P031002,
    pct_travel_20_to_24_min ~ P031007 / P031002,
    pct_travel_25_to_29_min ~ P031008 / P031002,
    pct_travel_30_to_34_min ~ P031009 / P031002,
    pct_travel_35_to_39_min ~ P031010 / P031002,
    pct_travel_40_to_44_min ~ P031011 / P031002,
    pct_travel_45_to_59_min ~ P031012 / P031002,
    pct_travel_60_to_89_min ~ P031013 / P031002,
    pct_travel_gt_90_min ~ P031014 / P031002
  ),
  
  # Currency-based variables
  currency = c(
    med_household_income ~ P053001,
    med_family_income ~ P077001,
    med_property_value ~ H085001
  ),
  
  indices = c(
    # Ethnic fractionalization
    #
    # Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R.
    # (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194.
    # https://doi.org/10.1023/A:1024471506938
    #
    # ethnic_fractionalization = 1 - (
    #   (non_hispanic_white / population)^2 +
    #   (non_hispanic_black / population)^2 +
    #   (non_hispanic_asian / population)^2 +
    #   (hispanic / population)^2
    # )
    ethnic_fractionalization ~ 1 - (
      (P007003 / P001001)^2 +
      (P007004 / P001001)^2 +
      (P007006 / P001001)^2 +
      (P007001 / P001001)^2
    ),
    
    # Townsend Index
    #
    # Townsend, P., Phillimore, P., & Beattie, A. (1988). Health and
    # deprivation: inequality and the North. Routledge.
    # 
    # Formula provided in:
    # Rice, L., Jiang, C., Wilson, S., Burwell-Naney, K., Samantapudi, A., &
    # Zhang, H. (2014). Use of Segregation Indices, Townsend Index, and Air
    # Toxics Data to Assess Lifetime Cancer Risk Disparities in Metropolitan
    # Charleston, South Carolina, USA. International Journal of Environmental
    # Research and Public Health, 11(5), 5510–5526.
    # https://doi.org/10.3390/ijerph110505510
    #
    # townsend_index = (
    #   z-score(log(percent unemployed + 1))
    #   z-score(log(percent households with more than two people per room + 1)) +
    #   z-score(percent households with no vehicle) +
    #   z-score(percent households who rent)
    # )
    townsend_index ~ (
      # Log percent unemployed
      # (Universe: Population 16 years and over)
      scale(log(
        # (Unemployed by sex) / (Labor force by sex)
        (P043007 + P043014) / (P043003 + P043010) + 1
      )) +
      # Log percent of households with more than two people per room
      # (Universe: Occupied housing units)
      scale(log(
        ((H020007 + H020013) / H007001) + 1
      )) +
      # Percent of households with no vehicle
      # (Universe: Occupied housing units)
      scale((H044003 + H044010) / H007001) +
      # Percent of households renter occupied
      # (Universe: Occupied housing units)
      scale(H007003 / H007001)
    ),
    
    # Individual Townsend Index components for area-specific scaling (may be
    # duplicative with other variables; see above for citations)
    #
    # Component 1: Log percent unemployed
    townsend_log_pct_unemployed ~ log(
      # (Unemployed by sex) / (Labor force by sex)
      (P043007 + P043014) / (P043003 + P043010) + 1
    ),
    # Component 2: Log percent of households with more than two people per room
    townsend_pct_crowded ~ log(
      ((H020007 + H020013) / H007001) + 1
    ),
    # Component 3: Percent of households with no vehicle
    townsend_pct_no_vehicle ~ (H044003 + H044010) / H007001,
    # Component 4: Percent of households renter occupied
    townsend_pct_renting ~ H007003 / H007001
  
  )
  
)

## SF1 (2010) ----

all_formulas_sf1_2010 <- list(
  
  # Universe: Total population
  total_population = c(
    population ~ P001001,
    
    # Race + ethnicity
    pct_female ~ P012026 / P001001,
    pct_white ~ P003002 / P001001,
    pct_black ~ P003003 / P001001,
    pct_native ~ P003004 / P001001,
    pct_asian ~ P003005 / P001001,
    pct_two_or_more_races ~ P003008 / P001001,
    pct_hispanic_white ~ P005011 / P001001,
    pct_hispanic_black ~ P005012 / P001001,
    pct_hispanic_native ~ P005013 / P001001,
    pct_hispanic_asian ~ P005014 / P001001,
    pct_hispanic_two_or_more_races ~ P005017 / P001001,
    pct_non_hispanic_white ~ P005003 / P001001,
    pct_non_hispanic_black ~ P005004 / P001001,
    pct_non_hispanic_native ~ P005005 / P001001,
    pct_non_hispanic_asian ~ P005006 / P001001,
    pct_non_hispanic_two_or_more_races ~ P005009 / P001001,
    pct_hispanic ~ P005010 / P001001,
    # pct_foreign_born ~ B05006_001 / P001001,
    
    # Compatibility with 1990 Decennial Census
    pct_asian_pacific_islander ~ (P003005 + P003006) / P001001,
    pct_hispanic_asian_pacific_islander ~ (P005014 + P005015) / P001001,
    pct_non_hispanic_asian_pacific_islander ~ (P005006 + P005007) / P001001,

    # Age
    pct_age_under_5 ~ (P012003 + P012027) / P001001,
    pct_age_5_to_9 ~ (P012004 + P012028) / P001001,
    pct_age_10_to_14 ~ (P012005 + P012029) / P001001,
    pct_age_15_to_17 ~ (P012006 + P012030) / P001001,
    pct_age_18_to_19 ~ (P012007 + P012031) / P001001,
    pct_age_20 ~ (P012008 + P012032) / P001001,
    pct_age_21 ~ (P012009 + P012033) / P001001,
    pct_age_22_to_24 ~ (P012010 + P012034) / P001001,
    pct_age_25_to_29 ~ (P012011 + P012035) / P001001,
    pct_age_30_to_34 ~ (P012012 + P012036) / P001001,
    pct_age_35_to_39 ~ (P012013 + P012037) / P001001,
    pct_age_40_to_44 ~ (P012014 + P012038) / P001001,
    pct_age_45_to_49 ~ (P012015 + P012039) / P001001,
    pct_age_50_to_54 ~ (P012016 + P012040) / P001001,
    pct_age_55_to_59 ~ (P012017 + P012041) / P001001,
    pct_age_60_to_61 ~ (P012018 + P012042) / P001001,
    pct_age_62_to_64 ~ (P012019 + P012043) / P001001,
    pct_age_65_to_66 ~ (P012020 + P012044) / P001001,
    pct_age_67_to_69 ~ (P012021 + P012045) / P001001,
    pct_age_70_to_74 ~ (P012022 + P012046) / P001001,
    pct_age_75_to_79 ~ (P012023 + P012047) / P001001,
    pct_age_80_to_84 ~ (P012024 + P012048) / P001001,
    pct_age_over_85 ~ (P012025 + P012049) / P001001,
    
    # Compatibility with 1990 Decennial Census
    pct_age_65_to_69 ~ (P012020 + P012021 + P012044 + P012045) / P001001
  ),
  
  # Universe: Households
  households = c(
    n_households ~ P018001,
    mean_household_size ~ P017001,
    pct_households_single_father ~ P018005 / P018001,
    pct_households_single_mother ~ P018006 / P018001
    # pct_public_assistance ~ ACS only
  ),
  
  # Universe: Occupied housing units
  occupied_housing_units = c(
    n_occupied_housing_units ~ H004001,
    pct_renting ~ H004004 / H010001
    # pct_heating_utility_gas ~ ACS only
    # pct_heating_gas_tank ~ ACS only
    # pct_heating_electricity ~ ACS only
    # pct_heating_oil ~ ACS only
    # pct_heating_coal ~ ACS only
    # pct_heating_wood ~ ACS only
    # pct_heating_solar ~ ACS only
    # pct_heating_other ~ ACS only
    # pct_heating_none ~ ACS only
  ),
  
  # Universe: Housing units
  housing_units = c(
    n_housing_units ~ H001001
    # pct_housing_standalone ~ ACS only
    # pct_housing_1_unit ~ ACS only
    # pct_housing_2_units ~ ACS only
    # pct_housing_3_to_4_units ~ ACS only
    # pct_housing_5_to_9_units ~ ACS only
    # pct_housing_10_to_19_units ~ ACS only
    # pct_housing_20_to_49_units ~ ACS only
    # pct_housing_gt_50_units ~ ACS only
    # pct_housing_mobile_home ~ ACS only
    # pct_housing_vehicle ~ ACS only
    # pct_built_2014_onwards ~ ACS only
    # pct_built_2010_to_2013 ~ ACS only
    # pct_built_2000_to_2009 ~ ACS only
    # pct_built_1990_to_1999 ~ ACS only
    # pct_built_1980_to_1989 ~ ACS only
    # pct_built_1970_to_1979 ~ ACS only
    # pct_built_1960_to_1969 ~ ACS only
    # pct_built_1950_to_1959 ~ ACS only
    # pct_built_1940_to_1949 ~ ACS only
    # pct_built_before_1939 ~ ACS only
    # med_year_built ~ ACS only
  ),
  
  # Universe: Population 16 years and over (employment)
  # ACS only
  
  # Universe: Population 18 years and over (education)
  # ACS only
  
  # Universe: Workers 16 years and over (transportation)
  # ACS only
  
  # Universe: Workers 16 years and over who did not work at home
  # ACS only
  
  # Currency-based variables
  # ACS only
  
  indices = c(
    # Ethnic fractionalization
    #
    # Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R.
    # (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194.
    # https://doi.org/10.1023/A:1024471506938
    #
    # ethnic_fractionalization = 1 - (
    #   (non_hispanic_white / population)^2 +
    #   (non_hispanic_black / population)^2 +
    #   (non_hispanic_asian / population)^2 +
    #   (hispanic / population)^2
    # )
    ethnic_fractionalization ~ 1 - (
      (P005003 / P001001)^2 +
      (P005004 / P001001)^2 +
      (P005006 / P001001)^2 +
      (P005010 / P001001)^2
    )
    
    # Townsend Index
    # ACS only
    
  )
  
)

## PL 94-171 (2020) ----

all_formulas_pl <- list(
  
  # Universe: Total population
  total_population = c(
    population ~ P1_001N,
    
    # Race + ethnicity
    # pct_female ~ ACS only
    pct_white ~ P1_003N / P1_001N,
    pct_black ~ P1_004N / P1_001N,
    pct_native ~ P1_005N / P1_001N,
    pct_asian ~ P1_006N / P1_001N,
    pct_two_or_more_races ~ P1_009N / P1_001N,
    pct_hispanic_white ~ (P1_003N - P2_005N) / P1_001N,
    pct_hispanic_black ~ (P1_004N - P2_006N) / P1_001N,
    pct_hispanic_native ~ (P1_005N - P2_007N) / P1_001N,
    pct_hispanic_asian ~ (P1_006N - P2_008N) / P1_001N,
    pct_hispanic_two_or_more_races ~ (P1_009N - P2_011N) / P1_001N,
    pct_non_hispanic_white ~ P2_005N / P1_001N,
    pct_non_hispanic_black ~ P2_006N / P1_001N,
    pct_non_hispanic_native ~ P2_007N / P1_001N,
    pct_non_hispanic_asian ~ P2_008N / P1_001N,
    pct_non_hispanic_two_or_more_races ~ P2_011N / P1_001N,
    pct_hispanic ~ P2_002N / P1_001N,
    # pct_foreign_born ~ B05006_001 / P1_001N,
    
    # Compatibility with 1990 Decennial Census
    pct_asian_pacific_islander ~ (P1_006N + P1_007N) / P1_001N,
    pct_hispanic_asian_pacific_islander ~ (
      P1_006N - P2_008N + P1_007N - P2_009N
    ) / P1_001N,
    pct_non_hispanic_asian_pacific_islander ~ (P2_008N + P2_009N) / P1_001N

    # Age
    # ACS only
  ),
  
  # Universe: Households
  # ACS only
  
  # Universe: Housing units
  housing_units = c(
    n_housing_units ~ H1_001N,
    n_occupied_housing_units ~ H1_002N
    # pct_renting ~ ACS only
    # pct_heating_utility_gas ~ ACS only
    # pct_heating_gas_tank ~ ACS only
    # pct_heating_electricity ~ ACS only
    # pct_heating_oil ~ ACS only
    # pct_heating_coal ~ ACS only
    # pct_heating_wood ~ ACS only
    # pct_heating_solar ~ ACS only
    # pct_heating_other ~ ACS only
    # pct_heating_none ~ ACS only
    # pct_housing_standalone ~ ACS only
    # pct_housing_1_unit ~ ACS only
    # pct_housing_2_units ~ ACS only
    # pct_housing_3_to_4_units ~ ACS only
    # pct_housing_5_to_9_units ~ ACS only
    # pct_housing_10_to_19_units ~ ACS only
    # pct_housing_20_to_49_units ~ ACS only
    # pct_housing_gt_50_units ~ ACS only
    # pct_housing_mobile_home ~ ACS only
    # pct_housing_vehicle ~ ACS only
    # pct_built_2014_onwards ~ ACS only
    # pct_built_2010_to_2013 ~ ACS only
    # pct_built_2000_to_2009 ~ ACS only
    # pct_built_1990_to_1999 ~ ACS only
    # pct_built_1980_to_1989 ~ ACS only
    # pct_built_1970_to_1979 ~ ACS only
    # pct_built_1960_to_1969 ~ ACS only
    # pct_built_1950_to_1959 ~ ACS only
    # pct_built_1940_to_1949 ~ ACS only
    # pct_built_before_1939 ~ ACS only
    # med_year_built ~ ACS only
  ),
  
  # Universe: Population 16 years and over (employment)
  # ACS only
  
  # Universe: Population 18 years and over (education)
  # ACS only
  
  # Universe: Workers 16 years and over (transportation)
  # ACS only
  
  # Universe: Workers 16 years and over who did not work at home
  # ACS only
  
  # Currency-based variables
  # ACS only
  
  indices = c(
    # Ethnic fractionalization
    #
    # Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R.
    # (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194.
    # https://doi.org/10.1023/A:1024471506938
    #
    # ethnic_fractionalization = 1 - (
    #   (non_hispanic_white / population)^2 +
    #   (non_hispanic_black / population)^2 +
    #   (non_hispanic_asian / population)^2 +
    #   (hispanic / population)^2
    # )
    ethnic_fractionalization ~ 1 - (
      (P2_005N / P1_001N)^2 +
      (P2_006N / P1_001N)^2 +
      (P2_008N / P1_001N)^2 +
      (P2_002N / P1_001N)^2
    )
    
    # Townsend Index
    # ACS only
    
  )
  
)

## ACS (2005-2020) ----
# NOTE: 2009-2020 ACS5 are unavailable from tidycensus

all_formulas_acs <- list(
  
  # Universe: Total population
  total_population = c(
    population ~ B01001_001,
    
    # Race + ethnicity
    pct_female ~ B01001_026 / B01001_001,
    pct_white ~ B02001_002 / B01001_001,
    pct_black ~ B02001_003 / B01001_001,
    pct_native ~ B02001_004 / B01001_001,
    pct_asian ~ B02001_005 / B01001_001,
    pct_two_or_more_races ~ B02001_008 / B01001_001,
    pct_hispanic_white ~ B03002_013 / B01001_001,
    pct_hispanic_black ~ B03002_014 / B01001_001,
    pct_hispanic_native ~ B03002_015 / B01001_001,
    pct_hispanic_asian ~ B03002_016 / B01001_001,
    pct_hispanic_two_or_more_races ~ B03002_019 / B01001_001,
    pct_non_hispanic_white ~ B03002_003 / B01001_001,
    pct_non_hispanic_black ~ B03002_004 / B01001_001,
    pct_non_hispanic_native ~ B03002_005 / B01001_001,
    pct_non_hispanic_asian ~ B03002_006 / B01001_001,
    pct_non_hispanic_two_or_more_races ~ B03002_009 / B01001_001,
    pct_hispanic ~ B03002_012 / B01001_001,
    pct_foreign_born ~ B05006_001 / B01001_001,
    
    # Compatibility with 1990 Decennial Census
    pct_asian_pacific_islander ~ (B02001_005 + B02001_006) / B01001_001,
    pct_hispanic_asian_pacific_islander ~ (B03002_016 + B03002_017) / B01001_001,
    pct_non_hispanic_asian_pacific_islander ~ (B03002_006 + B03002_007) / B01001_001,

    # Age
    pct_age_under_5 ~ (B01001_003 + B01001_027) / B01001_001,
    pct_age_5_to_9 ~ (B01001_004 + B01001_028) / B01001_001,
    pct_age_10_to_14 ~ (B01001_005 + B01001_029) / B01001_001,
    pct_age_15_to_17 ~ (B01001_006 + B01001_030) / B01001_001,
    pct_age_18_to_19 ~ (B01001_007 + B01001_031) / B01001_001,
    pct_age_20 ~ (B01001_008 + B01001_032) / B01001_001,
    pct_age_21 ~ (B01001_009 + B01001_033) / B01001_001,
    pct_age_22_to_24 ~ (B01001_010 + B01001_034) / B01001_001,
    pct_age_25_to_29 ~ (B01001_011 + B01001_035) / B01001_001,
    pct_age_30_to_34 ~ (B01001_012 + B01001_036) / B01001_001,
    pct_age_35_to_39 ~ (B01001_013 + B01001_037) / B01001_001,
    pct_age_40_to_44 ~ (B01001_014 + B01001_038) / B01001_001,
    pct_age_45_to_49 ~ (B01001_015 + B01001_039) / B01001_001,
    pct_age_50_to_54 ~ (B01001_016 + B01001_040) / B01001_001,
    pct_age_55_to_59 ~ (B01001_017 + B01001_041) / B01001_001,
    pct_age_60_to_61 ~ (B01001_018 + B01001_042) / B01001_001,
    pct_age_62_to_64 ~ (B01001_019 + B01001_043) / B01001_001,
    pct_age_65_to_66 ~ (B01001_020 + B01001_044) / B01001_001,
    pct_age_67_to_69 ~ (B01001_021 + B01001_045) / B01001_001,
    pct_age_70_to_74 ~ (B01001_022 + B01001_046) / B01001_001,
    pct_age_75_to_79 ~ (B01001_023 + B01001_047) / B01001_001,
    pct_age_80_to_84 ~ (B01001_024 + B01001_048) / B01001_001,
    pct_age_over_85 ~ (B01001_025 + B01001_049) / B01001_001,
    
    # Compatibility with 1990 Decennial Census
    pct_age_65_to_69 ~ (
      B01001_020 + B01001_044 + B01001_021 + B01001_045
    ) / B01001_001
  ),
  
  # Universe: Households
  households = c(
    n_households ~ B11001_001,
    mean_household_size ~ B25010_001,
    pct_households_single_father ~ B11001_005 / B11001_001,
    pct_households_single_mother ~ B11001_006 / B11001_001,
    pct_public_assistance ~ B19057_002 / B11001_001
  ),
  
  # Universe: Occupied housing units
  occupied_housing_units = c(
    n_occupied_housing_units ~ B25003_001,
    pct_renting ~ B25003_003 / B25003_001,
    pct_heating_utility_gas ~ B25040_002 / B25003_001,
    pct_heating_gas_tank ~ B25040_003 / B25003_001,
    pct_heating_electricity ~ B25040_004 / B25003_001,
    pct_heating_oil ~ B25040_005 / B25003_001,
    pct_heating_coal ~ B25040_006 / B25003_001,
    pct_heating_wood ~ B25040_007 / B25003_001,
    pct_heating_solar ~ B25040_008 / B25003_001,
    pct_heating_other ~ B25040_009 / B25003_001,
    pct_heating_none ~ B25040_001 / B25003_001
  ),
  
  # Universe: Housing units
  housing_units = c(
    n_housing_units ~ B25024_001,
    pct_housing_standalone ~ B25024_002 / B25024_001,
    pct_housing_1_unit ~ B25024_003 / B25024_001,
    pct_housing_2_units ~ B25024_004 / B25024_001,
    pct_housing_3_to_4_units ~ B25024_005 / B25024_001,
    pct_housing_5_to_9_units ~ B25024_006 / B25024_001,
    pct_housing_10_to_19_units ~ B25024_007 / B25024_001,
    pct_housing_20_to_49_units ~ B25024_008 / B25024_001,
    pct_housing_gt_50_units ~ B25024_009 / B25024_001,
    pct_housing_mobile_home ~ B25024_010 / B25024_001,
    pct_housing_vehicle ~ B25024_011 / B25024_001,
    # pct_built_2014_onwards ~ B25034_002 / B25024_001,
    # pct_built_2010_to_2013 ~ B25034_003 / B25024_001,
    # pct_built_2000_to_2009 ~ B25034_004 / B25024_001,
    # pct_built_1990_to_1999 ~ B25034_005 / B25024_001,
    # pct_built_1980_to_1989 ~ B25034_006 / B25024_001,
    # pct_built_1970_to_1979 ~ B25034_007 / B25024_001,
    # pct_built_1960_to_1969 ~ B25034_008 / B25024_001,
    # pct_built_1950_to_1959 ~ B25034_009 / B25024_001,
    # pct_built_1940_to_1949 ~ B25034_010 / B25024_001,
    # pct_built_before_1939 ~ B25034_011 / B25024_001,
    med_year_built ~ B25035_001
  ),
  
  # Universe: Population 16 years and over (employment)
  pop_16y_and_over = c(
    # Long formulation for 2009 + 2010 ACS compatibility
    pct_unemployed ~ (
      # Unemployed by age
      B23001_008 + B23001_015 + B23001_022 + B23001_029 + B23001_036 +
      B23001_043 + B23001_050 + B23001_057 + B23001_064 + B23001_071 +
      B23001_076 + B23001_081 + B23001_086 + B23001_094 + B23001_101 +
      B23001_108 + B23001_115 + B23001_122 + B23001_129 + B23001_136 +
      B23001_143 + B23001_150 + B23001_157 + B23001_162 + B23001_167 +
      B23001_172
    ) / (
      # Labor force by age
      B23001_004 + B23001_011 + B23001_018 + B23001_025 + B23001_032 +
      B23001_039 + B23001_046 + B23001_053 + B23001_060 + B23001_067 +
      B23001_074 + B23001_079 + B23001_084 + B23001_090 + B23001_097 +
      B23001_104 + B23001_111 + B23001_118 + B23001_125 + B23001_132 +
      B23001_139 + B23001_146 + B23001_153 + B23001_160 + B23001_165 +
      B23001_170
    )
  ),
  
  # Universe: Population 18 years and over (education)
  pop_18y_and_over = c(
    pct_edu_lt_9th_grade ~ (
      B15001_004 + B15001_012 + B15001_020 + B15001_028 + B15001_036 +
      B15001_045 + B15001_053 + B15001_061 + B15001_069 + B15001_077
    ) / B15001_001,
    pct_edu_9th_to_12th_grade ~ (
      B15001_005 + B15001_013 + B15001_021 + B15001_029 + B15001_037 +
      B15001_046 + B15001_054 + B15001_062 + B15001_070 + B15001_078
    ) / B15001_001,
    pct_edu_high_school ~ (
      B15001_006 + B15001_014 + B15001_022 + B15001_030 + B15001_038 +
      B15001_047 + B15001_055 + B15001_063 + B15001_071 + B15001_079
    ) / B15001_001,
    pct_edu_some_college ~ (
      B15001_007 + B15001_015 + B15001_023 + B15001_031 + B15001_039 +
      B15001_048 + B15001_056 + B15001_064 + B15001_072 + B15001_080
    ) / B15001_001,
    pct_edu_associate ~ (
      B15001_008 + B15001_016 + B15001_024 + B15001_032 + B15001_040 +
      B15001_049 + B15001_057 + B15001_065 + B15001_073 + B15001_081
    ) / B15001_001,
    pct_edu_bachelors ~ (
      B15001_009 + B15001_017 + B15001_025 + B15001_033 + B15001_041 +
      B15001_050 + B15001_058 + B15001_066 + B15001_074 + B15001_082
    ) / B15001_001,
    pct_edu_graduate_or_professional ~ (
      B15001_010 + B15001_018 + B15001_026 + B15001_034 + B15001_042 +
      B15001_051 + B15001_059 + B15001_067 + B15001_075 + B15001_083
    ) / B15001_001
  ),
  
  # Universe: Workers 16 years and over (transportation)
  workers_16y_and_over = c(
    pct_transport_auto ~ B08301_002 / B08301_001,
    pct_transport_auto_alone ~ B08301_003 / B08301_001,
    pct_transport_auto_carpool ~ B08301_004 / B08301_001,
    pct_transport_public_transit ~ B08301_010 / B08301_001,
    pct_transport_bus ~ B08301_011 / B08301_001,
    pct_transport_streetcar ~ B08301_012 / B08301_001,
    pct_transport_subway ~ B08301_013 / B08301_001,
    pct_transport_rail ~ B08301_014 / B08301_001,
    pct_transport_ferry ~ B08301_015 / B08301_001,
    pct_transport_taxi ~ B08301_016 / B08301_001,
    pct_transport_motorcycle ~ B08301_017 / B08301_001,
    pct_transport_bicycle ~ B08301_018 / B08301_001,
    pct_transport_walk ~ B08301_019 / B08301_001,
    pct_transport_other ~ B08301_020 / B08301_001,
    pct_transport_wfh ~ B08301_021 / B08301_001
  ),
  
  # Universe: Workers 16 years and over who did not work at home
  workers_16y_and_over_not_wfh = c(
    pct_travel_lt_5_min ~ B08303_002 / B08303_001,
    pct_travel_5_to_9_min ~ B08303_003 / B08303_001,
    pct_travel_10_to_14_min ~ B08303_004 / B08303_001,
    pct_travel_15_to_19_min ~ B08303_005 / B08303_001,
    pct_travel_20_to_24_min ~ B08303_006 / B08303_001,
    pct_travel_25_to_29_min ~ B08303_007 / B08303_001,
    pct_travel_30_to_34_min ~ B08303_008 / B08303_001,
    pct_travel_35_to_39_min ~ B08303_009 / B08303_001,
    pct_travel_40_to_44_min ~ B08303_010 / B08303_001,
    pct_travel_45_to_59_min ~ B08303_011 / B08303_001,
    pct_travel_60_to_89_min ~ B08303_012 / B08303_001,
    pct_travel_gt_90_min ~ B08303_013 / B08303_001
  ),
  
  # Currency-based variables
  currency = c(
    med_household_income ~ B19013_001,
    med_family_income ~ B19113_001,
    med_property_value ~ B25077_001
  ),
  
  indices = c(
    # Ethnic fractionalization
    #
    # Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R.
    # (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194.
    # https://doi.org/10.1023/A:1024471506938
    #
    # ethnic_fractionalization = 1 - (
    #   (non_hispanic_white / population)^2 +
    #   (non_hispanic_black / population)^2 +
    #   (non_hispanic_asian / population)^2 +
    #   (hispanic / population)^2
    # )
    ethnic_fractionalization ~ 1 - (
      (B03002_003 / B01001_001)^2 +
      (B03002_004 / B01001_001)^2 +
      (B03002_006 / B01001_001)^2 +
      (B03002_012 / B01001_001)^2
    ),
    
    # Townsend Index
    #
    # Townsend, P., Phillimore, P., & Beattie, A. (1988). Health and
    # deprivation: inequality and the North. Routledge.
    # 
    # Formula provided in:
    # Rice, L., Jiang, C., Wilson, S., Burwell-Naney, K., Samantapudi, A., &
    # Zhang, H. (2014). Use of Segregation Indices, Townsend Index, and Air
    # Toxics Data to Assess Lifetime Cancer Risk Disparities in Metropolitan
    # Charleston, South Carolina, USA. International Journal of Environmental
    # Research and Public Health, 11(5), 5510–5526.
    # https://doi.org/10.3390/ijerph110505510
    #
    # townsend_index = (
    #   z-score(log(percent unemployed + 1))
    #   z-score(log(percent households with more than two people per room + 1)) +
    #   z-score(percent households with no vehicle) +
    #   z-score(percent households who rent)
    # )
    townsend_index ~ (
      # Log percent unemployed
      # (Universe: Population 16 years and over)
      scale(log(
        (
          # Unemployed by age
          B23001_008 + B23001_015 + B23001_022 + B23001_029 + B23001_036 +
          B23001_043 + B23001_050 + B23001_057 + B23001_064 + B23001_071 +
          B23001_076 + B23001_081 + B23001_086 + B23001_094 + B23001_101 +
          B23001_108 + B23001_115 + B23001_122 + B23001_129 + B23001_136 +
          B23001_143 + B23001_150 + B23001_157 + B23001_162 + B23001_167 +
          B23001_172
        ) / (
          # Labor force by age
          B23001_004 + B23001_011 + B23001_018 + B23001_025 + B23001_032 +
          B23001_039 + B23001_046 + B23001_053 + B23001_060 + B23001_067 +
          B23001_074 + B23001_079 + B23001_084 + B23001_090 + B23001_097 +
          B23001_104 + B23001_111 + B23001_118 + B23001_125 + B23001_132 +
          B23001_139 + B23001_146 + B23001_153 + B23001_160 + B23001_165 +
          B23001_170
        ) + 1
      )) +
      # Log percent of households with more than two people per room
      # (Universe: Occupied housing units)
      scale(log(
        ((B25014_007 + B25014_013) / B25003_001) + 1
      )) +
      # Percent of households with no vehicle
      # (Universe: Occupied housing units)
      scale((B25044_003 + B25044_010) / B25003_001) +
      # Percent of households renter occupied
      # (Universe: Occupied housing units)
      scale(B25009_010 / B25003_001)
    ),
    
    # Individual Townsend Index components for area-specific scaling (may be
    # duplicative with other variables; see above for citations)
    #
    # Component 1: Log percent unemployed
    townsend_log_pct_unemployed ~ log(
      (
        B23001_008 + B23001_015 + B23001_022 + B23001_029 + B23001_036 +
        B23001_043 + B23001_050 + B23001_057 + B23001_064 + B23001_071 +
        B23001_076 + B23001_081 + B23001_086 + B23001_094 + B23001_101 +
        B23001_108 + B23001_115 + B23001_122 + B23001_129 + B23001_136 +
        B23001_143 + B23001_150 + B23001_157 + B23001_162 + B23001_167 +
        B23001_172
      ) / (
        B23001_004 + B23001_011 + B23001_018 + B23001_025 + B23001_032 +
        B23001_039 + B23001_046 + B23001_053 + B23001_060 + B23001_067 +
        B23001_074 + B23001_079 + B23001_084 + B23001_090 + B23001_097 +
        B23001_104 + B23001_111 + B23001_118 + B23001_125 + B23001_132 +
        B23001_139 + B23001_146 + B23001_153 + B23001_160 + B23001_165 +
        B23001_170
      ) + 1
    ),
    # Component 2: Log percent of households with more than two people per room
    townsend_pct_crowded ~ log(
      ((B25014_007 + B25014_013) / B25003_001) + 1
    ),
    # Component 3: Percent of households with no vehicle
    townsend_pct_no_vehicle ~ (B25044_003 + B25044_010) / B25003_001,
    # Component 4: Percent of households renter occupied
    townsend_pct_renting ~ B25009_010 / B25003_001
  
  )
  
)

# Pre-fetch all -----------------------------------------------------------

get_census_multiple <- function(geographies,
                                years,
                                datasets,
                                formulas,
                                ...,
                                discard = FALSE
                                ) {
  grid <- expand.grid(
    year = years,
    geography = geographies,
    dataset = datasets
  )
  all_results <- apply(
    grid,
    1,
    function(row) {
      message(sprintf(
        "==== Fetching %s %s %s",
        row[["year"]], row[["dataset"]], row[["geography"]]
      ))
      result <- get_census(
        geography = row[["geography"]],
        dataset = row[["dataset"]],
        year = as.numeric(row[["year"]]), # Gets type converted in the apply()
        formulas = formulas,
        ...
      )
      if (discard) {
        rm(result)
        gc()
      } else {
        return(result)
      }
    }
  )
  if (discard) {
    rm(all_results)
    gc()
  } else {
    return(list(
      results = all_results,
      grid = grid
    ))
  }
}

## SF1 (2000) ----

get_census_multiple(
  geographies = MAIN_GEOGRAPHIES,
  years = 2000,
  datasets = "sf1",
  formulas = unlist(all_formulas_sf1_2000),
  discard = TRUE
)

# get_census_multiple(
#   geographies = "block",
#   years = 2000,
#   datasets = "sf1",
#   formulas = unlist(all_formulas_sf1_2000),
#   discard = TRUE
# )

# invisible(lapply(
#   unique(unlist(lapply(
#     unlist(all_formulas_sf1_2000),
#     rhs_variables
#   ))),
#   function(variable) {
#     message(variable)
#     tryCatch(
#       get_tidycensus_cached("block", 2000, "sf1", variable),
#       error = function(e) TRUE,
#       finally = NULL
#     )
#   }
# ))

## SF3 (2000) ----

get_census_multiple(
  geographies = setdiff(MAIN_GEOGRAPHIES, "block"),
  years = 2000,
  datasets = "sf3",
  formulas = unlist(all_formulas_sf3),
  discard = TRUE
)

## SF1 (2010) ----

get_census_multiple(
  geographies = MAIN_GEOGRAPHIES,
  years = 2010,
  datasets = "sf1",
  formulas = unlist(all_formulas_sf1_2010),
  discard = TRUE
)

## PL 94-171 (2020) ----

get_census_multiple(
  geographies = setdiff(MAIN_GEOGRAPHIES, "zip code tabulation area"),
  years = 2020,
  datasets = "pl",
  formulas = unlist(all_formulas_pl),
  discard = TRUE
)

## ACS5 (2009-2012) ----

get_census_multiple(
  # Only need to retrieve tracts and ZCTAs because totalcensus retrieves all
  # geographies at once. Tracts triggers downloading all states individually and
  # ZCTAs triggers downloading whole-US tables.
  geographies = c("tract", "zip code tabulation area"), 
  years = 2009:2012,
  datasets = "acs5",
  formulas = unlist(all_formulas_acs),
  discard = TRUE,
  census_fetch_function = get_totalcensus
)

## ACS5 (2013-2020) ----

get_census_multiple(
  geographies = setdiff(MAIN_GEOGRAPHIES, "block"),
  years = 2013:2020,
  datasets = "acs5",
  formulas = unlist(all_formulas_acs),
  discard = TRUE
)

## ACS3 (2007-2013) ----

get_census_multiple(
  geographies = c("state", "county"),
  years = c(2007:2009, 2011:2013), # TODO: 2010 is broken
  datasets = "acs3",
  formulas = unlist(all_formulas_acs),
  discard = TRUE
)

## ACS1 (2005) ----
# B08301 + B08303 are not available in 2005

get_census_multiple(
  geographies = c("state", "county"),
  years = 2005,
  datasets = "acs1",
  formulas = Filter(
    function(f) {
      return(!any(grepl("B083", rhs_variables(f))))
    },
    unlist(all_formulas_acs)
  ),
  discard = TRUE
)

## ACS1 (2006-2019) ----

get_census_multiple(
  geographies = c("state", "county"),
  years = 2019:2006,
  datasets = "acs1",
  formulas = unlist(all_formulas_acs),
  discard = TRUE
)

# Export ------------------------------------------------------------------

export_census_multiple <- function(geographies,
                                   years,
                                   datasets,
                                   formulas,
                                   ...,
                                   output_directory
                                   ) {
  dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)
  grid <- expand.grid(
    year = years,
    geography = geographies,
    dataset = datasets
  )
  apply(
    grid,
    1,
    function(row) {
      output_path <- file.path(
        output_directory,
        sprintf(
          "%s_%s_%s.csv.gz",
          row[["year"]],
          gsub(" ", "_", row[["geography"]]),
          row[["dataset"]]
        )
      )
      if (file.exists(output_path)) {
        message(sprintf(
          "==== Skipping %s %s %s (%s exists)",
          row[["year"]], row[["dataset"]], row[["geography"]], output_path
        ))
      } else {
        temp_path <- sprintf("%s-temp.csv.gz", output_path)
        message(sprintf(
          "==== Exporting %s %s %s to %s",
          row[["year"]], row[["dataset"]], row[["geography"]], output_path
        ))
        result <- get_census(
          geography = row[["geography"]],
          dataset = row[["dataset"]],
          year = as.numeric(row[["year"]]), # Gets type converted in the apply()
          formulas = formulas,
          ...
        )
        fwrite(result, temp_path)
        rm(result)
        gc()
        file.rename(temp_path, output_path)
      }
    }
  )
}

## SF1 (2000) ----

export_census_multiple(
  geographies = MAIN_GEOGRAPHIES,
  years = 2000,
  datasets = "sf1",
  formulas = unlist(all_formulas_sf1_2000),
  output_directory = "output/tables/decennial"
)

## SF3 (2000) ----

export_census_multiple(
  geographies = setdiff(MAIN_GEOGRAPHIES, "block"),
  years = 2000,
  datasets = "sf3",
  formulas = unlist(all_formulas_sf3),
  output_directory = "output/tables/decennial"
)

## SF1 (2010) ----

export_census_multiple(
  geographies = MAIN_GEOGRAPHIES,
  years = 2010,
  datasets = "sf1",
  formulas = unlist(all_formulas_sf1_2010),
  output_directory = "output/tables/decennial"
)

## PL 94-171 (2020) ----

export_census_multiple(
  geographies = setdiff(MAIN_GEOGRAPHIES, "zip code tabulation area"),
  years = 2020,
  datasets = "pl",
  formulas = unlist(all_formulas_pl),
  output_directory = "output/tables/decennial"
)

## ACS5 (2009-2012) ----

export_census_multiple(
  geographies = setdiff(MAIN_GEOGRAPHIES, "block"),
  years = 2009:2012,
  datasets = "acs5",
  formulas = unlist(all_formulas_acs),
  output_directory = "output/tables/acs5",
  census_fetch_function = get_totalcensus
)

## ACS5 (2013-2020) ----

export_census_multiple(
  geographies = setdiff(MAIN_GEOGRAPHIES, "block"),
  years = 2013:2020,
  datasets = "acs5",
  formulas = unlist(all_formulas_acs),
  output_directory = "output/tables/acs5"
)

## ACS3 (2007-2013) ----

export_census_multiple(
  geographies = c("state", "county"),
  years = c(2007:2009, 2011:2013), # TODO: 2010 is broken
  datasets = "acs3",
  formulas = unlist(all_formulas_acs),
  output_directory = "output/tables/acs3"
)

## ACS1 (2005) ----
# B08301 + B08303 are not available in 2005

export_census_multiple(
  geographies = c("state", "county"),
  years = 2005,
  datasets = "acs1",
  formulas = Filter(
    function(f) {
      return(!any(grepl("B083", rhs_variables(f))))
    },
    unlist(all_formulas_acs)
  ),
  output_directory = "output/tables/acs1"
)

## ACS1 (2006-2019) ----

export_census_multiple(
  geographies = c("state", "county"),
  years = 2019:2006,
  datasets = "acs1",
  formulas = unlist(all_formulas_acs),
  output_directory = "output/tables/acs1"
)
