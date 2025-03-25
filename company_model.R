# FAVRI and the companies
# selling point of safe vegetable

# test model with a function to do one random draw of the model
# make sure to clear in Enviroment
source("functions/make_variables.R")
source("functions/estimate_read_csv.R")
make_variables(estimate_read_csv(paste("data/company_input.csv", sep="")))

data <- read.csv("data/company_input.csv")  # replace with your actual CSV file name
var_list <- unique(data$variable)  # extract the variable names you're checking
r_script_lines <- readLines("company_model.R")  # replace with your actual R file
r_script_text <- paste(r_script_lines, collapse = " ")
# Check which variables from CSV are NOT found in the R script text
missing_vars <- var_list[!sapply(var_list, function(v) grepl(v, r_script_text))]

# value varier function to add variability to values
source("functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("functions/chance_event.R")

# discount values for NPV (time value for money)
source("functions/discount.R")

# Model ####
# set.seed(1234) # to keep the same collection of results each time
# company_function <- function(x, varnames){
  
  # Cost####
  
 # establishment costs####

   one_time_establishment_costs <- cost_of_establishment_capital +
                       cost_establishment_cleaning_establishment +
                       cost_establishment_human_resource_staff +
                       cost_establishment_allowance_extended_participants+
                       cost_establishment_promotion +
                       cost_establishment_stakeholder_engagement +
                       cost_establishment_communication +
                       cost_establishment_legal_permission +
                       cost_establishment_overhead_cost +
                       cost_establishment_equipment
                       
                       
 # maintaining costs####
   
    annual_running_costs <- cost_of_maintaining_monitoring_and_evaluation+
               cost_of_maintaining_depreciation_of_equipment+
               cost_of_maintaining_capital+
               cost_of_maintaining_cleaning+
               cost_of_maintaining_human_resource_staff+
               cost_of_maintaining_allowance_extended_partipants+
               cost_of_maintaining_promotion
    
  
  # costs over five years####
  total_costs <-  vv(annual_running_costs, var_CV = CV_value, 
                     relative_trend = inflation_rate,
                     n = number_of_years)
  
  
  #assumning that in the first year we establishment costs and running costs
  total_costs[1] <- total_costs[1] + one_time_establishment_costs
  
  # Benefits ####
  
  baseline_health_conditions <- 100 # vulnerable urban communities have underlying health effects
  
  # By the beginning of 2020, according to central standards,
  # Hanoi city has 4,112 poor households, accounting for 0.2% and 3,939
  # near-poor households, accounting for 0.19% of the total number of
  # households. The poverty rate according to central standards decreased from
  # 2.97% at the beginning of the period to 0.2%.
  
  # According to the city's standards, Hanoi city has 8,692 poor households,
  # accounting for 0.42% and 41,937 near-poor households, accounting for 2.01%
  # of the total number of households. The poverty rate according to the city
  # standard decreased from 3.64% to 0.42% (down 52,212 households).
  
  # This is the official statistics of Vietnam (no poverty
  # rate of Hanoi) https://www.gso.gov.vn/en/?s=poverty+rate+hanoi&lang=en
  # Newspaper (quite reliable): Toàn thành phố Hà Nội chỉ còn 0,2% tỷ lệ hộ nghèo
  # (laodongthudo.vn) 
  
  # Risks to access ####
  
  ## if_accessible  
  
  disease_outbreak <-chance_event(disease_outbreak,value_if=1,
                                  value_if_not=0)
  
  if( disease_outbreak == 1) {
    if_accessible <- if_accessible - access_reduction_disease
  } else {
    if_accessible <- if_accessible}
  
  change_of_source_undesirable <-chance_event(change_of_source_undesirable,
                                              value_if=1,value_if_not=0)
  
  if( change_of_source_undesirable == 1) {
    if_accessible <- if_accessible - access_reduction_change_of_source
  } else {
    if_accessible <- if_accessible}
  
  affordability_low <-chance_event(chance_low_affordability,value_if=1,
                                   value_if_not=0)

  if( affordability_low == 1) {
    if_accessible <- if_accessible - access_reduction_affordability
  } else {
    if_accessible <- if_accessible}
  
  organization_poor <-chance_event(chance_poorly_organized,value_if=1,
                                   value_if_not=0)
  
  inconvenient <-chance_event(chance_farmstand_inconvenient,value_if=1,
                              value_if_not=0)
  
  if(organization_poor == 1 | inconvenient == 1) {
    if_accessible <- if_accessible - access_reduction_organization_inconvenient
  } else {
    if_accessible <- if_accessible}
  
  
  not_preferred <-chance_event(chance_not_meet_preference,value_if=1,
                               value_if_not=0)
  
  if(not_preferred == 1) {
    if_accessible <- if_accessible - access_reduction_preference
  } else {
    if_accessible <- if_accessible}
  
  # change negative values to zero
  # we do not expect a negative effect on accessibility
  if_accessible[if_accessible<0]<-0
  
  # benefits from nutrition####
 
  # worker's health benefit from nutrition
  # calculated as a cost savings to the company 
  # annual benefits to work productivity
  # due to better nutrition for the workers (less health care costs, more efficient)
  
  # benefit safety_of_food
  
  work_benefits_from_safety_of_food <-benefit_safety_of_food *
                                              (1-reduction_in_veg_quality_if_monopoly)*
                                              (1+ farming_practice_increase_quality)
  
  # CW NEED TO WORK HERE ####
  work_benefits_from_accessibility <-benefit_accessibility_of_food*if_accessible*
                                              (1-reduction_in_veg_quality_if_monopoly)*
                                              (1+ farming_practice_increase_quality)
  # CW NEED TO WORK HERE ####
  work_benefits_from_food_prep <-benefit_prep_of_food *
    (1-reduction_in_veg_quality_if_monopoly)*
    (1+ farming_practice_increase_quality)
  
  # CW NEED TO WORK HERE ####
  work_benefits_from_nutrition <-benefit_nutrition_of_food *
                                           (1-reduction_in_veg_quality_if_monopoly)*
                                           (1+ farming_practice_increase_quality)
  
  # benefits to workers health ####
  benefit_worker_health <-(work_benefits_from_accessibility + 
                              work_benefits_from_food_prep + 
                              work_benefits_from_safety_of_food + 
                              work_benefits_from_nutrition) *if_accessible
  
  # Risk of poor relationship ####
  ## if_relationship
  
  conflict_farmer_buyer <-chance_event(chance_conflict_farmer_buyer,value_if=1,
                                       value_if_not=0)
  
  conflict_farmer_company <-chance_event(chance_conflict_farmer_company,
                                         value_if=1,
                                         value_if_not=0)
  
  rumor <-chance_event(chance_rumor,value_if=1,
                       value_if_not=0)
  
  # if factory has a good reputation then work ethic goes up a bit
  # if not it goes down a bit
  if(conflict_farmer_buyer == 1|
     conflict_farmer_company == 1|
     rumor == 1) {
    factory_reputation <- benefit_factory_reputation - relationship_reduction
  } else {
    factory_reputation <- benefit_factory_reputation}
  
  # calculate the work efficiency effect of the safe vegetables####
   benefit_work_efficiency <- (benefit_worker_health +
    (benefit_worker_time_savings * # 1-2 hours a week saved on shopping (max)
                            if_accessible)) * benefit_factory_reputation

 
  diversity_risk <-chance_event(chance_vegetable_diversity,value_if=1,
                                value_if_not=0)
  
  vegetable_quality_risk <- chance_event(chance_vegetable_quality,value_if=1,
                                         value_if_not=0)
  
  
  if(diversity_risk == 1| vegetable_quality_risk == 1){
    work_benefits_from_nutrition <- work_benefits_from_nutrition * #VND value
      (1 - reduction_nutrition_low_diverse_and_quality) #reduction percentage
  } else {
    work_benefits_from_nutrition <- work_benefits_from_nutrition
  }
  
  
  
   # calculate company revenue####
  company_revenue_annual <- 
    benefit_work_efficiency +
    benefit_farmer_revenue #shared with company
  
  company_revenue_through_intervention <- vv(company_revenue_annual, 
                                             var_CV = CV_value, 
                                             relative_trend = inflation_rate,
                                             n = number_of_years)
  

  
  conflicting_interest <- chance_event(conflicting_business_interests) 
  
  if(conflicting_interest == 1) {
    company_revenue_through_intervention <- company_revenue_through_intervention - 
      revenue_reduction_if_conflicting_interest
  }else{
    company_revenue_through_intervention <- company_revenue_through_intervention 
  }
  
  longterm_benefits_farmstand <- company_revenue_through_intervention + 
    benefit_enviroment_on_campus
  
  # Alternative ####
  # what might the company do instead 
  # just connect farmers and consumers and allow them to handle logistics
 
  expected_cost_of_connectFarm <- vv(cost_establishment_human_resource_staff * 
                                  percent_meetings_no_intervention, # ten percent of the meetings, but each year
                                  var_CV = CV_value, 
                                  n = number_of_years)
  
  expected_benefit_of_connectFarm <- vv(longterm_benefits_farmstand*
                                        percent_veg_benefit_no_intervention,
                                        var_CV = CV_value, 
                                        n = number_of_years)
                                  
                                  
                                  
  connectFarm_result <- expected_benefit_of_connectFarm - expected_cost_of_connectFarm
  
  # The difference is inherent in our calculation so we do not need the 
  # comparative NPV here, just discount the intervention result
  # # = (1 + Real Discount Rate)(1 + Inflation Rate) – 1 ≈ Real Discount Rate + Inflation Rate
  # Real Discount Rate ≈ Nominal Discount Rate - Inflation Rate
  # Nominal discount rate is often considered as nominal bank interest rate. If we have already considered inflation in the cost and benefit calculation, then for calculating NPV, we should consider
  # 
  # Nominal interest rate
  # Company's choice of nominal interest rate (or nominal discount rate).

  NPV_farmstand <-
    discount(longterm_benefits_farmstand, 
             discount_rate, calculate_NPV = TRUE) #consider inflation rate???
  
 NPV_connectFarm <-
    discount(connectFarm_result, 
             discount_rate, calculate_NPV = TRUE)
  
  # Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
#   
#   return(list(NPV_farmstand = NPV_farmstand,
#               NPV_connectFarm = NPV_connectFarm,
#               NPV_decision = NPV_farmstand - NPV_connectFarm, #the decision
#               cashflow_farmstand = longterm_benefits_farmstand))
# }


