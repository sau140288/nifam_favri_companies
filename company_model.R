# FAVRI and the companies
# selling point of safe vegetable

# test model with a function to do one random draw of the model
# make sure to clear in Enviroment
source("functions/make_variables.R")
source("functions/estimate_read_csv.R")
make_variables(estimate_read_csv(paste("company_input.csv", sep="")))

# value varier function to add variability to values
source("functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("functions/chance_event.R")

# discount values for NPV (time value for money)
source("functions/discount.R")

# Model ####
# set.seed(1234) # to keep the same collection of results each time
company_function <- function(x, varnames){
  
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
  
  # Risks to access ####
  
  ## if_accessible  
  
  chance_disease_outbreak <-chance_event(disease_outbreak,value_if=1,
                                  value_if_not=0)
  
  if( chance_disease_outbreak == 1) {
    if_accessible <- if_accessible - access_reduction_disease
  } else {
    if_accessible <- if_accessible}
  
  
  change_of_source_undesirable <-chance_event(change_of_source_undesirable,
                                              value_if=1,value_if_not=0)
  
  if( change_of_source_undesirable == 1) {
    if_accessible <- if_accessible - access_reduction_change_of_source
  } else {
    if_accessible <- if_accessible}
  
  
  chance_affordability_low <-chance_event(affordability,value_if=1,
                                   value_if_not=0)
  
  if( chance_affordability_low == 1) {
    if_accessible <- if_accessible - access_reduction_affordability
  } else {
    if_accessible <- if_accessible}
  
  chance_organization_inconvenient <-chance_event(organization_inconvenient,value_if=1,
                                   value_if_not=0)
  
 
  if(chance_organization_inconvenient== 1 | organization_inconvenient == 1) {
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
  
  
  # worker's health benefit ####
 

  # benefit safety_of_food 
  
  benefit_safety_of_food_risk <- ((benefit_safety_of_food * total_worker_company) *
                                (1-reduction_in_veg_quality_if_monopoly)*
                                (1+ farming_practice_increase_quality))* if_accessible
 
  ## if_nutrition
  
  # calculate if_nutrition with risk from diversity
  
  nutrition_diversity_risk <-chance_event(diversity,value_if=1,
                                value_if_not=0)
  
   if(nutrition_diversity_risk == 1){
    if_nutrition <- if_nutrition *
                    (1 - reduction_nutrition_low_diverse)*
                    (1+  farming_practice_increase_diversity)
  } else {
    if_nutrition <- if_nutrition
  }
  
  # calculate if_nutrition with risk from vegetable quality
  
nutrition_quality_risk <- chance_event(vegetable_quality_nutrition,value_if=1,
                                         value_if_not=0)
  
  if(nutrition_quality_risk == 1){
    if_nutrition <- if_nutrition *
      (1 - reduction_nutrition_low_quality)*
      (1+  farming_practice_increase_quality)
  } else {
    if_nutrition  <- if_nutrition
  }
   
 # calculate total worker health

  total_worker_health_benefit <- (benefit_nutrition *if_accessible* if_nutrition* total_worker_company)+
                                  benefit_safety_of_food_risk+
                                  (benefit_food_prep_cooking_and_food_waste *if_accessible*total_worker_company)
  
   
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
  # this is the
  if(conflict_farmer_buyer == 1|
     conflict_farmer_company == 1|
     rumor == 1) {
    factory_reputation <- benefit_factory_reputation - relationship_reduction
  } else {
    factory_reputation <- benefit_factory_reputation}
  
  # calculate the work efficiency ####
   benefit_work_efficiency <- (benefit_worker_time_savings * total_worker_company * if_accessible)+ # 1-2 hours a week saved on shopping (max)
                              total_worker_health_benefit +                        
                              benefit_factory_reputation

  
   # calculate company revenue####
  company_revenue_annual <- (benefit_work_efficiency + 
                             benefit_farmer_revenue) #shared with company
  
  company_revenue_through_intervention <- vv(company_revenue_annual, 
                                             var_CV = CV_value, 
                                             relative_trend = inflation_rate,
                                             n = number_of_years)
  
  #  calculate conflicting business interests risk effects on company revenue annual
  
  
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
  
 
  expected_cost_of_altervative <- vv(cost_establishment_human_resource_staff * 
                                  percent_meetings_no_intervention, # ten percent of the meetings, but each year
                                  var_CV = CV_value, 
                                  n = number_of_years)
  
  expected_benefit_of_altervative <- vv(longterm_benefits_farmstand*
                                        percent_veg_benefit_no_intervention,
                                        var_CV = CV_value, 
                                        n = number_of_years)
                             
                                  
  connectFarm_result <- expected_benefit_of_altervative - expected_cost_of_altervative
  
  # The difference is inherent in our calculation so we do not need the 
  # comparative NPV here, just discount the intervention result
  # # = (1 + Real Discount Rate)(1 + Inflation Rate) – 1 ≈ Real Discount Rate + Inflation Rate
  # Real Discount Rate ≈ Nominal Discount Rate - Inflation Rate
  # Nominal discount rate is often considered as nominal bank interest rate. If we have already considered inflation in the cost and benefit calculation, then for calculating NPV, we should consider
  # 
  # Nominal interest rate
  # Company's choice of nominal interest rate (or nominal discount rate).
  # We suggest the discount rate from 3-15%  

  
  NPV_farmstand <-
    discount(longterm_benefits_farmstand, 
             discount_rate, calculate_NPV = TRUE) #consider inflation rate???
  
 NPV_connectFarm <-
    discount(connectFarm_result, 
             discount_rate, calculate_NPV = TRUE)
  
 
  # Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  
  return(list(NPV_farmstand = NPV_farmstand,
              NPV_connectFarm = NPV_connectFarm,
              decision = NPV_farmstand - NPV_connectFarm, #the decision is in the trade off
              cashflow_farmstand = longterm_benefits_farmstand))
}


# Monte Carlo simulation 
source("functions/mcSimulation.R")

company_results <- mcSimulation(estimate = estimate_read_csv("company_input.csv"),
  model_function = company_function,
  numberOfModelRuns = 1000, #run 1,000 times
  functionSyntax = "plainNames"
)

# plot distributions for the two options
source("functions/plot_distributions.R")
plot_distributions(mcSimulation_object = company_results, 
                   vars = c("NPV_farmstand","NPV_connectFarm"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

plot_distributions(mcSimulation_object = company_results, 
                   vars = c("NPV_farmstand","NPV_connectFarm"),
                   method = 'boxplot')

plot_distributions(mcSimulation_object = company_results, 
                   vars = "decision",
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

# Cashflow of the company option
source("functions/plot_cashflow.R")
plot_cashflow(mcSimulation_object = company_results, 
              cashflow_var_name = "cashflow_farmstand")

# PLS
source("functions/pls_model.R")
pls_result <- pls_model(object = company_results,
                                resultName = names(company_results$y)[3], 
                                ncomp = 1)

input_table <- read.csv("company_input.csv")
source("functions/plot_pls.R")
plot_pls(pls_result, input_table = input_table, threshold = 0)

# EVPI 
source("functions/multi_EVPI.R")
#here we subset the outputs from the mcSimulation function (y) 
# by selecting the correct variables
# be sure to run the multi_EVPI only on the variables that the we want
# check names
names(company_results$x) #input table
names(company_results$y) #return
mcSimulation_table <- data.frame(company_results$x, 
                                 company_results$y[1:3]) #choose NPV only

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_farmstand")

source("functions/plot_evpi.R")
plot_evpi(evpi, decision_vars = "decision")

# Get summary statistics
summary(company_results$y$decision)



