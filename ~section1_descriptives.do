* =============================================================================
* SECTION 1: DESCRIPTIVE STATISTICS – Tourism in Europe
* =============================================================================

* -----------------------------------------------------------------------------
* 1. Set working directory
* -----------------------------------------------------------------------------
cd "./"

capture log close
log using "section1_output.smcl", replace

use "stata exam.dta", clear

* -----------------------------------------------------------------------------
* 2. Construct dummy variable for Central and Eastern Europe (CEE)
* -----------------------------------------------------------------------------
capture drop cee
capture label drop label99

gen cee = country == "Albania" | ///
          country == "Armenia" | ///
          country == "Belarus" | ///
          country == "Bosnia and Herzegovina" | ///
          country == "Bulgaria" | ///
          country == "Croatia" | ///
          country == "Czechia" | ///
          country == "Estonia" | ///
          country == "Georgia" | ///
          country == "Hungary" | ///
          country == "Kosovo (under United Nations Security Council Resolution 1244/99)" | ///
          country == "Latvia" | ///
          country == "Lithuania" | ///
          country == "Moldova" | ///
          country == "Montenegro" | ///
          country == "North Macedonia" | ///
          country == "Poland" | ///
          country == "Romania" | ///
          country == "Russia" | ///
          country == "Serbia" | ///
          country == "Slovakia" | ///
          country == "Slovenia" | ///
          country == "Ukraine"

label define label99 0 "Non-CEE" 1 "CEE"
label values cee label99
label variable cee "Central and Eastern Europe"

tab cee

* -----------------------------------------------------------------------------
* 3. Description of the dataset
* -----------------------------------------------------------------------------
describe
codebook
summarize

* -----------------------------------------------------------------------------
* 4. Descriptive statistics the dependent variable: nights
* -----------------------------------------------------------------------------
summarize nights, detail

* -----------------------------------------------------------------------------
* 5. Plot the distribution of nights for all groups
* -----------------------------------------------------------------------------

* All countries
histogram nights, normal title("Nights - All Countries")
graph export "hist_nights_all.png", replace

* CEE countries only
histogram nights if cee == 1, normal title("Nights - CEE Countries")
graph export "hist_nights_cee.png", replace

* Non-CEE countries only
histogram nights if cee == 0, normal title("Nights - Non-CEE Countries")
graph export "hist_nights_noncee.png", replace

* Log transformation of nights
capture drop ln_nights
gen ln_nights = ln(nights)
label variable ln_nights "Log of Nights Spent"

* Histogram of log-nights
histogram ln_nights, normal title("Log(Nights) - All Countries")
graph export "hist_ln_nights_all.png", replace

* Groups means
mean ln_nights, over(cee)

* T-test
ttest ln_nights, by(cee)

* -----------------------------------------------------------------------------
* 6. Relationship between GDP and Tourist Arrivals
* -----------------------------------------------------------------------------

* Scatter plot
scatter arrivals GDP, title("Arrivals vs GDP") ///
    ytitle("Tourist Arrivals") xtitle("GDP (million euros)") ///
    name(raw_scatter, replace)
graph export "scatter_arrivals_gdp.png", replace

* Correlation
correlate arrivals GDP

* Scatter plot log-transformed
scatter ln_arrivals ln_GDP, title("Log(Arrivals) vs Log(GDP)") ///
    ytitle("Log Tourist Arrivals") xtitle("Log GDP") ///
    name(log_scatter, replace)
graph export "scatter_ln_arrivals_gdp.png", replace

* Correlation log-transformes
correlate ln_arrivals ln_GDP

* -----------------------------------------------------------------------------
* 7. Estimate effect of tourist arrivals on GDP (linear and log-log models)
* -----------------------------------------------------------------------------

* Linear-linear model
regress GDP arrivals

predict gdp_hat_linear if e(sample), xb

twoway (scatter GDP arrivals) ///
       (line gdp_hat_linear arrivals, sort lcolor(red)), ///
       title("Linear Model: GDP vs Arrivals") ///
       xtitle("Tourist Arrivals") ytitle("GDP (million euros)") ///
       name(linear_plot, replace)
graph export "regression_linear.png", replace

* Log-log model
regress ln_GDP ln_arrivals

predict ln_gdp_hat if e(sample), xb

twoway (scatter ln_GDP ln_arrivals) ///
       (line ln_gdp_hat ln_arrivals, sort lcolor(red)), ///
       title("Log-Log Model: ln(GDP) vs ln(Arrivals)") ///
       xtitle("Log Tourist Arrivals") ytitle("Log GDP") ///
       name(log_plot, replace)
graph export "regression_loglog.png", replace

* Combine both regressions
graph combine linear_plot log_plot, col(2)
graph export "regression_models_side_by_side.png", replace

* -----------------------------------------------------------------------------
* 8. Test for Nonlinear Effect: Add Quadratic Form of Arrivals
* -----------------------------------------------------------------------------

* Generate quadratic
capture drop arrivals_sq
gen arrivals_sq = arrivals^2
label variable arrivals_sq "Squared Tourist Arrivals"

* Run quadratic regression
regress GDP arrivals arrivals_sq

predict gdp_hat_quad if e(sample), xb

twoway (scatter GDP arrivals) ///
       (line gdp_hat_quad arrivals, sort lcolor(red)), ///
       title("Quadratic Model: GDP vs Arrivals²") ///
       xtitle("Tourist Arrivals") ///
       ytitle("GDP (million euros)") ///
       name(quad_plot, replace)

graph export "regression_quadratic.png", replace

* =============================================================================
* SECTION 2: The Regression Model – Determinants of Tourism Demand
* =============================================================================

* -----------------------------------------------------------------------------
* 1. Estimate full log-log model for tourism demand
* -----------------------------------------------------------------------------

* Multiple linear regression: log-log specification
reg ln_nights ln_GDP ln_pop ln_airports ln_offer ln_exp_culture ln_env ln_homicide ln_rape

capture drop ln_nights_hat
predict ln_nights_hat if e(sample), xb
label variable ln_nights_hat "Fitted Values: ln(Nights)"

estimates store model1

vif

* To investigate the determinants of tourism demand across European countries, we estimated a multiple linear regression model in log-log form using the dependent variable ln_nights, which captures the logarithm of total nights spent at tourist accommodation establishments. The explanatory variables include macroeconomic indicators (ln_GDP, ln_pop), tourism infrastructure (ln_airports, ln_offer), government and environmental factors (ln_exp_culture, ln_env), and public safety indicators (ln_homicide, ln_rape).

* The model demonstrates a strong overall fit with an R-squared of 0.774, indicating that approximately 77% of the variation in tourism demand is explained by the included variables. Among the predictors, ln_offer (availability of accommodations), ln_airports (accessibility), and ln_homicide (safety concerns) emerged as statistically significant. In contrast, variables such as ln_GDP, ln_pop, ln_exp_culture, ln_env, and ln_rape were not statistically significant.

* Additionally, a multicollinearity check using Variance Inflation Factors (VIFs) revealed particularly high values for ln_GDP (22.57) and ln_pop (20.91), suggesting these variables may distort estimates due to strong linear dependencies with other regressors. Based on these results, the next step involves refining the model by removing or transforming highly collinear and non-significant variables to improve model robustness and interpretability.

* Drop ln_GDP and ln_pop due to multicollinearity.
* Remove ln_exp_culture, ln_env, and ln_rape, since they are statistically insignificant and have weak contribution.


* -----------------------------------------------------------------------------
* 2. Estimate refined log-log model
* -----------------------------------------------------------------------------

* Run reduced regression which refined model based on significant variables
reg ln_nights ln_airports ln_offer ln_homicide

capture drop ln_nights_hat_refined
predict ln_nights_hat_refined if e(sample), xb
label variable ln_nights_hat_refined "Fitted Values (Refined)"

estimates store model2

* -----------------------------------------------------------------------------
* 3. Export regression comparison table
* -----------------------------------------------------------------------------

* Make sure about outreg2 is installed
cap which outreg2
if _rc {
    ssc install outreg2, replace
}

* Export both models side by side to Word file
outreg2 [model1 model2] using "regression_models.doc", replace ///
    ctitle("Full Model" "Reduced Model") ///
    dec(3) se addstat(R-squared, e(r2))

* -----------------------------------------------------------------------------
* 4. Mean comparison of ln_offer between CEE and Non-CEE countries
* -----------------------------------------------------------------------------

* 1. Descriptive stats by group
mean ln_offer, over(cee)

* 2. Two-sample t-test
ttest ln_offer, by(cee)

* -----------------------------------------------------------------------------
* 5. Inspect correlation matrix with ln_nights
* -----------------------------------------------------------------------------
pwcorr ln_nights ln_GDP ln_pop ln_airports ln_offer ln_env ln_homicide, sig

* -----------------------------------------------------------------------------
* 6. Plot scatter graphs to visually explore relationships
* -----------------------------------------------------------------------------
scatter ln_nights ln_offer, title("ln(Nights) vs ln(Offer)")
graph export "scatter_ln_nights_offer.png", replace

scatter ln_nights ln_airports, title("ln(Nights) vs ln(Airports)")
graph export "scatter_ln_nights_airports.png", replace

scatter ln_nights ln_GDP, title("ln(Nights) vs ln(GDP)")
graph export "scatter_ln_nights_GDP.png", replace

* -----------------------------------------------------------------------------
* 7. Estimate log-log linear regression model
* -----------------------------------------------------------------------------
reg ln_nights ln_offer ln_airports ln_GDP ln_homicide

capture drop ln_nights_hat_main
predict ln_nights_hat_main if e(sample), xb
label variable ln_nights_hat_main "Fitted Values - Main Model"

* -----------------------------------------------------------------------------
* 8. Plot actual vs fitted values
* -----------------------------------------------------------------------------
twoway (scatter ln_nights ln_nights_hat_main, msymbol(circle) mcolor(blue)) ///
       (line ln_nights_hat_main ln_nights_hat_main, sort lcolor(red)), ///
       title("Fitted vs Actual: ln(Nights)") ///
       xtitle("Fitted ln(Nights)") ytitle("Actual ln(Nights)")
graph export "fitted_vs_actual_ln_nights.png", replace

* -----------------------------------------------------------------------------
* 9. Check for residuals and assumptions
* -----------------------------------------------------------------------------

* Residual plot
predict resid_main, residuals
rvfplot, yline(0) title("Residual vs Fitted")
graph export "residuals_plot_ln_nights.png", replace

* Normality of residuals
histogram resid_main, normal title("Histogram of Residuals")
graph export "resid_histogram.png", replace

kdensity resid_main, normal title("Density of Residuals")
graph export "resid_density.png", replace

* Multicollinearity check
vif

estimates store main_model


* -----------------------------------------------------------------------------
* 10. Drop one covariate: ln_GDP
* -----------------------------------------------------------------------------

* Re-run the regression without ln_GDP
reg ln_nights ln_offer ln_airports ln_homicide

estimates store model_nogdp

* Predict fitted values
capture drop ln_nights_hat_nogdp
predict ln_nights_hat_nogdp if e(sample), xb
label variable ln_nights_hat_nogdp "Fitted Values (No ln_GDP)"

* -----------------------------------------------------------------------------
* 11. Compare both models: with and without ln_GDP
* -----------------------------------------------------------------------------

* Basic comparison table
estimates table main_model model_nogdp, b se stats(r2 N)

* -----------------------------------------------------------------------------
* 12. Drop ln_offer and re-estimate the model
* -----------------------------------------------------------------------------

* Run regression without ln_offer
reg ln_nights ln_airports ln_GDP ln_homicide

estimates store model_nooffer

* Predict fitted values
capture drop ln_nights_hat_nooffer
predict ln_nights_hat_nooffer if e(sample), xb
label variable ln_nights_hat_nooffer "Fitted Values (No ln_offer)"

* Compare all three models: full model, no GDP, no offer
estimates table main_model model_nogdp model_nooffer, b se stats(r2 N)

* =============================================================================
* 13. Subsample Regression: CEE Countries Only
* =============================================================================

* Run the same main regression using only CEE countries
reg ln_nights ln_offer ln_airports ln_GDP ln_homicide if cee == 1

estimates store cee_model

* Predict fitted values
capture drop ln_nights_hat_cee
predict ln_nights_hat_cee if e(sample), xb
label variable ln_nights_hat_cee "Fitted Values - CEE Only"

* =============================================================================
* 14. Compare Full Sample vs CEE Sample
* =============================================================================

* Compare main model and CEE-only model
estimates table main_model cee_model, b se stats(r2 N)

* Predict fitted values again if needed
capture drop ln_nights_hat_cee
predict ln_nights_hat_cee if e(sample), xb
label variable ln_nights_hat_cee "Fitted Values - CEE Only"

* Plot actual vs predicted for CEE
twoway (scatter ln_nights ln_nights_hat_cee if cee == 1, ///
        msymbol(circle) mcolor(blue)) ///
       (line ln_nights_hat_cee ln_nights_hat_cee if cee == 1, ///
        sort lcolor(red)), ///
       title("CEE Countries: Actual vs Fitted ln(Nights)") ///
       xtitle("Fitted ln(Nights)") ytitle("Actual ln(Nights)")

graph export "scatter_fitted_actual_cee.png", replace


* -----------------------------------------------------------------------------
* End log
* -----------------------------------------------------------------------------
log close
