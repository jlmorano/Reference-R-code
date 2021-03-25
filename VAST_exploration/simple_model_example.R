# Simple example
# from https://github.com/James-Thorson-NOAA/VAST/wiki/Simple-example
# Updated March 4, 2021

# Example of using VAST with high-level wrapper functions

# Download latest release number; its useful for reproducibility to use a specific release number
# devtools::install_github("James-Thorson-NOAA/VAST")

# Set local working directory (change for your machine)
setwd( "/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/simple model output" )

# Load package
library(VAST)
sessionInfo()
# R version 4.0.4 (2021-02-15)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# FishStatsUtils_2.8.0 
# VAST_3.6.1           
# TMB_1.7.19   

### After installing earlier versions of TMB and Matrix
# [1] FishStatsUtils_2.9.0 VAST_3.7.0           TMB_1.7.18          
# [4] Matrix_1.2-8

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example = load_example( data_set="EBS_pollock" )

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 100, 
                          Region = example$Region, 
                          purpose = "index2", 
                          strata.limits = example$strata.limits, 
                          bias.correct = FALSE )

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = example$sampling_data[,'Lat'], 
                 Lon_i = example$sampling_data[,'Lon'], 
                 t_i = example$sampling_data[,'Year'], 
                 c_i = rep(0,nrow(example$sampling_data)), 
                 b_i = example$sampling_data[,'Catch_KG'], 
                 a_i = example$sampling_data[,'AreaSwept_km2'], 
                 v_i = example$sampling_data[,'Vessel'] )

# Plot results
plot( fit )


# Output from March 5, 2021
### Running `plot_results`

### Creating plots in directory /Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/simple model output/

### Making plots of data availability and knots
# non finite transformation detected:
            # [,1]        [,2] [,3] [,4]
  # [1,] -72.50068 -2.39501953  Inf  Inf
  # [2,] -72.58672 -2.36513672  Inf  Inf
  # [3,] -72.62534 -2.35166016  Inf  Inf
  # [4,] -72.66016 -2.36103516  Inf  Inf
  # [5,] -72.71416 -2.39218750  Inf  Inf
  # [6,] -72.81123 -2.40546875  Inf  Inf
  # [7,] -72.88716 -2.40849609  Inf  Inf
  # [8,] -72.94111 -2.39404297  Inf  Inf
  # [9,] -72.98965 -2.33974609  Inf  Inf
 # [10,] -73.06816 -2.31201172  Inf  Inf
 # [11,] -73.15449 -2.27822266  Inf  Inf
 # [12,] -73.17266 -2.20839844  Inf  Inf
 # [13,] -73.16021 -2.15634766  Inf  Inf
 # [14,] -73.12651 -2.08105469  Inf  Inf
 # [15,] -73.14521 -2.00332031  Inf  Inf
 # [16,] -73.18149 -1.88037109  Inf  Inf
 # [17,] -73.19697 -1.83027344  Inf  Inf
 # [18,] -73.22397 -1.78769531  Inf  Inf
 # [19,] -73.26646 -1.77226563  Inf  Inf
 # [20,] -73.34951 -1.78388672  Inf  Inf
 # [21,] -73.44028 -1.73740234  Inf  Inf
 # [22,] -73.49629 -1.69306641  Inf  Inf
 # [23,] -73.52524 -1.63886719  Inf  Inf
 # [24,] -73.49434 -1.53662109  Inf  Inf
 # [25,] -73.52139 -1.44970703  Inf  Inf
 # [26,] -73.57549 -1.40136719  Inf  Inf
 # [27,] -73.61025 -1.31640625  Inf  Inf
 # [28,] -73.66431 -1.24882812  Inf  Inf
 # [29,] -73.73574 -1.21416016  Inf  Inf
 # [30,] -73.80718 -1.21796875  Inf  Inf
 # [31,] -73.86318 -1.19667969  Inf  Inf
 # [32,] -73.92695 -1.12519531  Inf  Inf
 # [33,] -73.98682 -1.09814453  Inf  Inf
 # [34,] -74.05439 -1.02861328  Inf  Inf
 # [35,] -74.18076 -0.99775391  Inf  Inf
 # [36,] -74.24639 -0.97060547  Inf  Inf
 # [37,] -74.28389 -0.92783203  Inf  Inf
 # [38,] -74.33442 -0.85087891  Inf  Inf
 # [39,] -74.32861 -0.80839844  Inf  Inf
 # [40,] -74.35312 -0.76660156  Inf  Inf
 # [41,] -74.37490 -0.69140625  Inf  Inf
 # [42,] -74.41787 -0.58066406  Inf  Inf
 # [43,] -74.46519 -0.51767578  Inf  Inf
 # [44,] -74.51387 -0.47011719  Inf  Inf
 # [45,] -74.55508 -0.42988281  Inf  Inf
 # [46,] -74.61636 -0.37001953  Inf  Inf
 # [47,] -74.69165 -0.33525391  Inf  Inf
 # [48,] -74.75537 -0.29863281  Inf  Inf
 # [49,] -74.78047 -0.24453125  Inf  Inf
 # [50,] -74.80176 -0.20009766  Inf  Inf
 # [51,] -74.83750 -0.20332031  Inf  Inf
 # [52,] -74.88882 -0.19941406  Inf  Inf
 # [53,] -74.94531 -0.18818359  Inf  Inf
 # [54,] -75.00498 -0.15585938  Inf  Inf
 # [55,] -75.05469 -0.11669922  Inf  Inf
 # [56,] -75.13838 -0.05048828  Inf  Inf
 # [57,] -75.18408 -0.04174805  Inf  Inf
 # [58,] -75.22461 -0.04174805  Inf  Inf
 # [59,] -75.28447 -0.10654297  Inf  Inf
 # [60,] -75.46396 -0.03842773  Inf  Inf
 # [61,] -75.61733  0.06289063  Inf  Inf
 # [62,] -75.77666  0.08925781  Inf  Inf
 # [63,] -75.87979  0.15097656  Inf  Inf
 # [64,] -75.97485  0.24775391  Inf  Inf
 # [65,] -76.02617  0.31308594  Inf  Inf
 # [66,] -76.06792  0.34555664  Inf  Inf
 # [67,] -76.27061  0.43940430  Inf  Inf
 # [68,] -76.31104  0.44848633  Inf  Inf
 # [69,] -76.38818  0.40498047  Inf  Inf
 # [70,] -76.41338  0.37885742  Inf  Inf
 # [71,] -76.41797  0.30390625  Inf  Inf
 # [72,] -76.42729  0.26123047  Inf  Inf
 # [73,] -76.49463  0.23544922  Inf  Inf
 # [74,] -76.60303  0.24096680  Inf  Inf
 # [75,] -76.67852  0.26816406  Inf  Inf
 # [76,] -76.72900  0.27211914  Inf  Inf
 # [77,] -76.73931  0.25083008  Inf  Inf
 # [78,] -76.76772  0.24165039  Inf  Inf
 # [79,] -76.82935  0.24775391  Inf  Inf
 # [80,] -76.92012  0.26850586  Inf  Inf
 # [81,] -77.00244  0.29624023  Inf  Inf
 # [82,] -77.11411  0.35507812  Inf  Inf
 # [83,] -77.16572  0.34775391  Inf  Inf
 # [84,] -77.29268  0.36040039  Inf  Inf
 # [85,] -77.39634  0.39389648  Inf  Inf
 # [86,] -77.42275  0.42485352  Inf  Inf
 # [87,] -77.52612  0.66035156  Inf  Inf
 # [88,] -77.60132  0.68950195  Inf  Inf
 # [89,] -77.64863  0.72363281  Inf  Inf
 # [90,] -77.67319  0.78222656  Inf  Inf
 # [91,] -77.70288  0.83784180  Inf  Inf
 # [92,] -77.82954  0.82539063  Inf  Inf
 # [93,] -78.03701  0.89873047  Inf  Inf
 # [94,] -78.18066  0.96855469  Inf  Inf
 # [95,] -78.31211  1.04609375  Inf  Inf
 # [96,] -78.51152  1.19882812  Inf  Inf
 # [97,] -78.58765  1.23666992  Inf  Inf
 # [98,] -78.68164  1.28344727  Inf  Inf
 # [99,] -78.73711  1.35869141  Inf  Inf
# [100,] -78.82886  1.43466797  Inf  Inf
# [101,] -78.85967  1.45537109  Inf  Inf
# [102,] -78.88848  1.52407227  Inf  Inf
# [103,] -79.02544  1.62368164  Inf  Inf
# [104,] -78.95767  1.75219727  Inf  Inf
# [105,] -78.79297  1.84873047  Inf  Inf
# [106,] -78.57690  1.77377930  Inf  Inf
# [107,] -78.55044  1.92363281  Inf  Inf
# [108,] -78.62861  2.05625000  Inf  Inf
# [109,] -78.61704  2.30678711  Inf  Inf
# [110,] -78.59170  2.35664062  Inf  Inf
# [111,] -78.53472  2.42368164  Inf  Inf
# [112,] -78.46045  2.47006836  Inf  Inf
# [113,] -78.41689  2.48349609  Inf  Inf
# [114,] -78.06665  2.50913086  Inf  Inf
# [115,] -78.03018  2.54306641  Inf  Inf
# [116,] -77.98721  2.56899414  Inf  Inf
# [117,] -77.93228  2.62924805  Inf  Inf
# [118,] -77.90078  2.69882812  Inf  Inf
# [119,] -77.87451  2.72587891  Inf  Inf
# [120,] -77.81357  2.71635742  Inf  Inf
# [121,] -77.80796  2.74638672  Inf  Inf
# [122,] -77.77666  2.78730469  Inf  Inf
# [123,] -77.67002  2.87885742  Inf  Inf
# [124,] -77.67109  2.91933594  Inf  Inf
# [125,] -77.70098  3.00756836  Inf  Inf
# [126,] -77.69365  3.03994141  Inf  Inf
# [127,] -77.63203  3.05117187  Inf  Inf
# [128,] -77.55913  3.07597656  Inf  Inf
# [129,] -77.52026  3.16025391  Inf  Inf
# [130,] -77.47222  3.23378906  Inf  Inf
# [131,] -77.41714  3.34179688  Inf  Inf
# [132,] -77.35654  3.34858398  Inf  Inf
# [133,] -77.32441  3.47475586  Inf  Inf
# [134,] -77.24277  3.58535156  Inf  Inf
# [135,] -77.07681  3.91328125  Inf  Inf
# [136,] -77.12686  3.90605469  Inf  Inf
# [137,] -77.16660  3.86225586  Inf  Inf
# [138,] -77.21201  3.86743164  Inf  Inf
# [139,] -77.26353  3.89321289  Inf  Inf
# [140,] -77.24839  4.04096680  Inf  Inf
# [141,] -77.27803  4.05849609  Inf  Inf
# [142,] -77.35820  3.94472656  Inf  Inf
# [143,] -77.42729  4.06044922  Inf  Inf
# [144,] -77.43354  4.13095703  Inf  Inf
# [145,] -77.40449  4.20078125  Inf  Inf
# [146,] -77.40874  4.24775391  Inf  Inf
# [147,] -77.52070  4.21279297  Inf  Inf
# [148,] -77.51553  4.25629883  Inf  Inf
# [149,] -77.44585  4.30102539  Inf  Inf
# [150,] -77.41426  4.34760742  Inf  Inf
# [151,] -77.35352  4.39829102  Inf  Inf
# [152,] -77.32832  4.47500000  Inf  Inf
# [153,] -77.31367  4.59384766  Inf  Inf
# [154,] -77.28633  4.72172852  Inf  Inf
# [155,] -77.30654  4.78466797  Inf  Inf
# [156,] -77.33945  4.83852539  Inf  Inf
# [157,] -77.36675  5.07656250  Inf  Inf
# [158,] -77.35918  5.21518555  Inf  Inf
# [159,] -77.37329  5.32397461  Inf  Inf
# [160,] -77.40176  5.41616211  Inf  Inf
# [161,] -77.53442  5.53710937  Inf  Inf
# [162,] -77.32461  5.67563477  Inf  Inf
# [163,] -77.24927  5.78017578  Inf  Inf
# [164,] -77.34468  5.99536133  Inf  Inf
# [165,] -77.46943  6.17675781  Inf  Inf
# [166,] -77.47305  6.28564453  Inf  Inf
# [167,] -77.44009  6.27172852  Inf  Inf
# [168,] -77.39824  6.27500000  Inf  Inf
# [169,] -77.35986  6.50449219  Inf  Inf
# [170,] -77.36880  6.57558594  Inf  Inf
# [171,] -77.43887  6.69033203  Inf  Inf
# [172,] -77.52598  6.69311523  Inf  Inf
# [173,] -77.60215  6.83730469  Inf  Inf
# [174,] -77.64585  6.86962891  Inf  Inf
# [175,] -77.68096  6.96040039  Inf  Inf
# [176,] -77.80371  7.13725586  Inf  Inf
# [177,] -77.90117  7.22934570  Inf  Inf
# Error in .spTransform_Polygon(input[[i]], to_args = to_args, from_args = from_args,  : 
  # failure in Polygons 48 Polygon 2 points 330:331:332:333:334:335:336:337:338:339:340:341:342:343:344:345:346:347:348:349:350:351:352:353:354:355:356:357:358:359:360:361:362:363:364:365:366:367:368:369:370:371:372:373:374:375:376:377:378:379:380:381:382:383:384:385:386:387:388:389:390:391:392:393:394:395:396:397:398:399:400:401:402:403:404:405:406:407:408:409:410:411:412:413:414:415:418:419:420:421:422:423:424:425:426:427:428:429:430:431:432:433:434:435:436:437:438:439:440:441:442:443:444:448:449:450:451:452:453:454:455:456:457:458:459:460:461:462:463:464:465:466:467:468:469:470:471:472:473:474:475:476:477:478:479:480:481:482:483:484:485:486:487:488:489:490:491:492:493:494:495:496:497:498:499:500:501:502:503:504:505:506:507:508:509:510:511
# In addition: Warning messages:
# 1: In spTransform(xSP, CRSobj, ...) :
  # NULL source CRS comment, falling back to PROJ string
# 2: In wkt(obj) : CRS object has no comment
# 3: In .spTransform_Polygon(input[[i]], to_args = to_args, from_args = from_args,  :
  # 177 projected point(s) not finite


# Output from August 2020
## Output
# see also documents
# Aniso.png = Distance at 10% correlation. Decorrelation (geometric anisotropy) distance for different directions. (related to the variogram?) HOW TO INTERPRET?

# center_of_gravity.png = detect shifts in distribution or range expansion/contraction. Center of gravity (COG) indicating shifts in distribution plus/minus 1 SE. 
# Data_and_knots.png = Spatial extent and location of knots
# Data_by_year.png = NOT SURE IT'S SHOWING ANYTHING Spatial distribution of catch-rate data
# Effective_Area.png = Interpret expansion/contraction of range. Effective area occupied indicating range expansion/contraction plus/minus 1 SE
# Index-Biomass.png = Is this: Index of abundance plus/minus 2 SE? The y-axis should be Abundance
# Kmeans-100.RData
# ln_density-predicted.png = Predicted density maps for each year
# parameter_estimates.RData
# parameter_estimates.txt
# quantile_residuals_on_map.png = Pearson residuals for positive catch rates by knot
# quantile_residuals.png = DHARMa residual diagnostics: Q-Q plot residuals; Residuals vs. predicted
# settings.txt = model settings
# Table_for_SS3.csv = ?? Columns: Year, Unit, Fleet, Estimate_metric_tons, SD_log; SD_mt

