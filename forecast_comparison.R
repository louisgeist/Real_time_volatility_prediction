# compare of optimal forecast & Monte Carlo with bootstrap forecast

x = GM_nfci

h = 2

opt_for = point_optimal_forecast(x,h)
boot_for = bootstrap_forecast(x,h)

opt_for
boot_for

# I should use intraday RV to validate a model -> QLIKE