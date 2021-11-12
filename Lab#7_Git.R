Mahrukh Jaura
The final lab - Lab#7

#For this lab, we will estimate a variety of models to try to predict if a person got vaxx
#(same data as last week). Compare logit with OLS in terms of prediction and set up the variables 
#to be ready to expand into other models (next week).

#First, defining a subgroup with the Household_Pulse_Data
# I think it would be interesting to observe people who have a household income of less than $25k. 
use_varb <- (Household_Pulse_data$INCOME == "HH income less than $25k")
dat_use <- subset(Household_Pulse_data,use_varb)
attach(dat_use)

summary(dat_use)
"RHISPANIC      RRACE               EEDUC              MS       EGENID_BIRTH      GENID_DESCRIBE
 Not Hispanic:4049   White:3657   less than hs:  50   NA       :  24   male  :1562   NA         :  25  
 Hispanic    : 551   Black: 530   some hs     : 107   married  :1473   female:3038   male       :1525  
                     Asian: 159   HS diploma  : 854   widowed  : 508                 female     :2972  
                     Other: 254   some coll   :1371   divorced :1214                 transgender:  21  
                                  assoc deg   : 669   separated: 135                 other      :  57  
                                  bach deg    :1002   never    :1246                                   
                                  adv deg     : 547                                                    
      SEXUAL_ORIENTATION                      KIDS_LT5Y                        KIDS_5_11Y  
 NA            :  54     NA                        :4248   NA                       :4070  
 gay or lesbian: 185     Yes children under 5 in HH: 352   Yes children 5 - 11 in HH: 530  
 straight      :3981                                                                       
 bisexual      : 218                                                                       
 something else:  90                                                                       
 dont know     :  72                                                                       
                                                                                           
                     KIDS_12_17Y                                ENROLLNONE                 RECVDVACC   
 NA                        :4030   NA                                :4314   NA                 :  14  
 Yes children 12 - 17 in HH: 570   children not in any type of school: 286   yes got vaxx       :3945  
                                                                             no did not get vaxx: 641  
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                      DOSESRV                         GETVACRV   
 NA                       : 670   NA                      :3957  
 yes got all doses        :3726   definitely will get vaxx:  66  
 yes plan to get all doses: 180   probably will get vaxx  :  78  
 no will not get all doses:  24   unsure about vaxx       : 146  
                                  probably not            : 114  
                                  definitely not          : 239  
                                                                 
                               KIDDOSES                       KIDGETVAC   
 NA                                :4037   NA                      :4331  
 Yes kids got or will get all doses: 300   definitely will get vaxx:  45  
 no kids did not or will not       : 263   probably will get vaxx  :  33  
                                           unsure about vaxx       :  69  
                                           probably not            :  43  
                                           definitely not          :  47  
                                           dont know yet           :  32  
                      HADCOVID                     WRKLOSSRV                             ANYWORK    
 NA                       :  20   NA                    :   9   NA                           :  23  
 yes doctor told had covid: 623   yes recent HH job loss: 930   yes employment in last 7 days:2123  
 no did not               :3913   no recent HH job loss :3661   no employment in last 7 days :2454  
 not sure                 :  44                                                                     
                                                                                                    
                                                                                                    
                                                                                                    
                KINDWORK               RSNNOWRKRV                                        CHLDCARE   
 NA                 :2512   NA              :2173   NA                                       :4044  
 work for govt      : 220   retired         :1237   yes impacts to childcare because pandemic: 177  
 work for private co:1249   other           : 384   no                                       : 379  
 work for nonprofit : 229   sick or disabled: 178                                                   
 self employed      : 336   laid off        : 142                                                   
 work in family biz :  54   caring for kids : 115                                                   
                            (Other)         : 371                                                   
                          CURFOODSUF                                                 CHILDFOOD   
 NA                            :  12   NA                                                 :3978  
 had enough food               :2695   often kids not eating enough because couldnt afford:  36  
 had enough but not what wanted:1394   sometimes kids not eating enough                   : 198  
 sometimes not enough food     : 406   kids got enough food                               : 388  
 often not enough food         :  93                                                             
                                                                                                 
                                                                                                 
                                            ANXIOUS    
 NA                                             :  11  
 no anxiety over past 2 wks                     :1661  
 several days anxiety over past 2 wks           :1482  
 more than half the days anxiety over past 2 wks: 582  
 nearly every day anxiety                       : 864  
                                                       
                                                       
                                             WORRY                                TENURE    
 NA                                             :  17   NA                           :  19  
 no worry over past 2 wks                       :1935   housing owned free and clear :1256  
 several days worried over past 2 wks           :1466   housing owned with mortgage  :1478  
 more than half the days worried over past 2 wks: 478   housing rented               :1764  
 nearly every day worry                         : 704   housing occupied without rent:  83  
                                                                                            
                                                                                            
                                LIVQTRRV               RENTCUR                    MORTCUR    
 live in detached 1 family          :2554   NA             :2838   NA                 :3128  
 live in bldg w 5+ apts             : 823   current on rent:1521   current on mortgage:1308  
 live in 1 family attached to others: 402   behind on rent : 241   behind on mortgage : 164  
 live in mobile home                : 320                                                    
 live in building with 3-4 apts     : 266                                                    
 live in bldg w 2 apartments        : 161                                                    
 (Other)                            :  74                                                    
                                        EVICT                                               FORCLOSE   
 NA                                        :4360   NA                                           :4437  
 very likely evicted in next 2 months      :  30   very likely forclosed in next 2 months       :  13  
 somewhat likely evicted in next 2 months  :  79   somewhat likely forclosed in next 2 months   :  29  
 not very likely evicted in next 2 months  :  71   not very likely forclosed in next 2 months   :  63  
 not at all likely evicted in next 2 months:  60   not at all forclosed evicted in next 2 months:  58  
                                                                                                       
                                                                                                       
        EST_ST                       PRIVHLTH                     PUBHLTH           REGION    
 California: 298   has private health ins:2687   has public health ins:2587   Northeast: 608  
 Texas     : 254   no private health ins :1651   no public health ins :1827   South    :1627  
 Florida   : 202   NA                    : 262   NA                   : 186   Midwest  : 934  
 Arizona   : 142                                                              West     :1431  
 Washington: 138                                                                              
 Georgia   : 135                                                                              
 (Other)   :3431                                                                              
                      INCOME     Num_kids_Pub_School Num_kids_Priv_School Num_kids_homeschool
 HH income less than $25k:4600   Min.   :0.000       Min.   :0.000        Min.   :0.000      
 NA                      :   0   1st Qu.:1.000       1st Qu.:0.000        1st Qu.:0.000      
 HH income $25k - $34.9k :   0   Median :1.000       Median :1.000        Median :1.000      
 HH income $35k - 49.9   :   0   Mean   :1.648       Mean   :0.763        Mean   :0.892      
 HH income $50k - 74.9   :   0   3rd Qu.:2.000       3rd Qu.:1.000        3rd Qu.:2.000      
 HH income $75 - 99.9    :   0   Max.   :4.000       Max.   :2.000        Max.   :2.000      
 (Other)                 :   0   NA's   :3895        NA's   :4503         NA's   :4498       
        Works_onsite           works_remote           Shop_in_store                  eat_in_restaurant
 NA           : 102   NA             : 255   NA              : 149   NA                       : 194   
 worked onsite:2148   worked remotely: 686   shopped in store:3500   eat at restaurant indoors:1750   
 no           :2350   no             :3659   no              : 951   no                       :2656   
                                                                                                      
                                                                                                      
                                                                                                      
    vaxx        
 Mode :logical  
 FALSE:641      
 TRUE :3945     
 NA's :14" 

#testing the dataset to observe variations and how widespread it is
summary(Household_Pulse_data$INCOME == "HH income less than $25k")
table(Household_Pulse_data$INCOME == "HH income less than $25k", Household_Pulse_data$EEDUC)
'      less than hs  some hs  HS diploma  some coll  assoc deg  bach deg  adv deg
FALSE          361     829         7003      13225      6839     19073     17184
TRUE            50     107          854      1371       669      1002      547'

dat_use$vaxx <- (dat_use$RECVDVACC == "yes got vaxx")
is.na(dat_use$vaxx) <- which(dat_use$RECVDVACC == "NA") 
summary(dat_use$vaxx)
"Mode      FALSE    TRUE    NAs 
logical     641    3945     14"

#Of the 4600 people in the subset of people with a HH Income of less than $25k, 3,945 are vaccinated, 641 are not vaccinated and 14 are NA. 

summary(as.numeric(dat_use$vaxx))
"Min.    1st Qu.  Median   Mean   3rd Qu.  Max.    NAs 
 0.0000  1.0000  1.0000  0.8602  1.0000  1.0000   14"
#As a percentage, 86.02% of the subgroup is vaccinated. 

#Observing the number of people vaxxed in the dataset differentiated through education level:
summary(dat_use$vaxx[EEDUC =="less than hs"])
'Mode   FALSE    TRUE 
logical    10      40' 
summary(dat_use$vaxx[EEDUC =="some hs"])
'Mode   FALSE    TRUE 
logical    24      83' 
summary(dat_use$vaxx[EEDUC =="HS diploma"])
'Mode   FALSE    TRUE    NAs 
logical  159     693       2'
summary(dat_use$vaxx[EEDUC =="some coll"])
'Mode   FALSE    TRUE    NAs 
logical  223    1144       4'
summary(dat_use$vaxx[EEDUC =="assoc deg"])
'Mode   FALSE    TRUE    NAs 
logical    99     566       4' 
summary(dat_use$vaxx[EEDUC =="bach deg"])
'Mode   FALSE    TRUE    NAs 
logical    83     915   4' 
summary(dat_use$vaxx[EEDUC =="adv deg"])
'Mode   FALSE    TRUE 
logical    43     504' 

d_educ <- data.frame(model.matrix(~ dat_use$EEDUC))
summary(d_educ)
'X.Intercept. dat_use.EEDUCsome.hs dat_use.EEDUCHS.diploma dat_use.EEDUCsome.coll dat_use.EEDUCassoc.deg
 Min.   :1     Min.   :0.00000      Min.   :0.0000          Min.   :0.000          Min.   :0.0000        
 1st Qu.:1     1st Qu.:0.00000      1st Qu.:0.0000          1st Qu.:0.000          1st Qu.:0.0000        
 Median :1     Median :0.00000      Median :0.0000          Median :0.000          Median :0.0000        
 Mean   :1     Mean   :0.02326      Mean   :0.1857          Mean   :0.298          Mean   :0.1454        
 3rd Qu.:1     3rd Qu.:0.00000      3rd Qu.:0.0000          3rd Qu.:1.000          3rd Qu.:0.0000        
 Max.   :1     Max.   :1.00000      Max.   :1.0000          Max.   :1.000          Max.   :1.0000        
 dat_use.EEDUCbach.deg dat_use.EEDUCadv.deg
 Min.   :0.0000        Min.   :0.0000      
 1st Qu.:0.0000        1st Qu.:0.0000      
 Median :0.0000        Median :0.0000      
 Mean   :0.2178        Mean   :0.1189      
 3rd Qu.:0.0000        3rd Qu.:0.0000      
 Max.   :1.0000        Max.   :1.0000'
levels(dat_use$EEDUC)
'[1] "less than hs" "some hs"      "HS diploma"   "some coll"    "assoc deg"    "bach deg"    
[7] "adv deg"'

d_marstat <- data.frame(model.matrix(~ dat_use$MS))
d_race <- data.frame(model.matrix(~ dat_use$RRACE))
d_hispanic <- data.frame(model.matrix(~ dat_use$RHISPANIC))
d_gender <- data.frame(model.matrix(~ dat_use$GENID_DESCRIBE))
d_region <- data.frame(model.matrix(~ dat_use$REGION))

d_vaxx <- data.frame(model.matrix(~ dat_use$vaxx)) # check number of obs to see that this snips off NA values

# note that, depending on your subgroup, this might snip off some columns so make sure to check summary() of each -- don't want Min = Max = 0!

dat_for_analysis_sub <- data.frame(
  d_vaxx[,2],
  d_educ[!is.na(dat_use$vaxx),2:7],
  d_marstat[!is.na(dat_use$vaxx),2:6],
  d_race[!is.na(dat_use$vaxx),2:4],
  d_hispanic[!is.na(dat_use$vaxx),2],
  d_gender[!is.na(dat_use$vaxx),2:5],
  d_region[!is.na(dat_use$vaxx),2:4]) # need [] since model.matrix includes intercept term


# this is just about me being anal-retentive, see difference in names(dat_for_analysis_sub) before and after running this bit
names(dat_for_analysis_sub) <- sub("dat_use.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "vaxx"
names(dat_for_analysis_sub)[2] <- "EEDUCsome.hs"
names(dat_for_analysis_sub)[3] <- "EEDUCHS.diploma"
names(dat_for_analysis_sub)[4] <- "EEDUCsome.coll"
names(dat_for_analysis_sub)[5] <- "EEDUCassoc.deg"
names(dat_for_analysis_sub)[6] <- "EEDUCbach.deg"
names(dat_for_analysis_sub)[7] <- "EEDUCadv.deg"
names(dat_for_analysis_sub)[8] <- "MSmarried"
names(dat_for_analysis_sub)[9] <- "MSwidowed"
names(dat_for_analysis_sub)[10] <- "MSdivorced"
names(dat_for_analysis_sub)[11] <- "MSseparated"
names(dat_for_analysis_sub)[12] <- "MSnever"
names(dat_for_analysis_sub)[13] <- "RRACEBlack"
names(dat_for_analysis_sub)[14] <- "RRACEAsian"
names(dat_for_analysis_sub)[15] <- "RRACEOther"
names(dat_for_analysis_sub)[16] <- "Hispanic"
names(dat_for_analysis_sub)[17] <- "GENID_DESCRIBEmale"
names(dat_for_analysis_sub)[18] <- "GENID_DESCRIBEfemale"
names(dat_for_analysis_sub)[19] <- "GENID_DESCRIBEtransgender"
names(dat_for_analysis_sub)[20] <- "GENID_DESCRIBEother"
names(dat_for_analysis_sub)[21] <- "REGIONSouth"
names(dat_for_analysis_sub)[22] <- "REGIONMidwest"
names(dat_for_analysis_sub)[23] <- "REGIONWest"
summary(dat_for_analysis_sub)

install.packages("standardize")
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$vaxx)
restrict_1 <- (runif(NN) < 0.5) 
summary(restrict_1)
"Mode   FALSE    TRUE 
logical    2307    2279" 
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

sobj <- standardize(vaxx ~  EEDUCsome.hs + EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
                      MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
                      Hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
                      REGIONSouth + REGIONMidwest + REGIONWest
                    , dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)

summary(s_dat_test)
"EEDUCsome.hs EEDUCHS.diploma EEDUCsome.coll EEDUCassoc.deg EEDUCbach.deg EEDUCadv.deg MSmarried MSwidowed
 1:  58       1: 411          1: 716         1: 337         1: 492        1: 264       1: 746    1: 251   
 0:2249       0:1896          0:1591         0:1970         0:1815        0:2043       0:1561    0:2056   
 MSdivorced MSseparated MSnever  RRACEBlack RRACEAsian RRACEOther Hispanic GENID_DESCRIBEmale
 1: 613     1:  66      1: 620   1: 284     1:  72     1: 137     1: 278   1: 738            
 0:1694     0:2241      0:1687   0:2023     0:2235     0:2170     0:2029   0:1569            
 GENID_DESCRIBEfemale GENID_DESCRIBEtransgender GENID_DESCRIBEother REGIONSouth REGIONMidwest REGIONWest
 1:1516               1:  10                    1:  32              1: 823      1: 460        1: 728    
 0: 791               0:2297                    0:2275              0:1484      0:1847        0:1579"

model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
"Call:
lm(formula = sobj$formula, data = sobj$data)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.0254  0.0585  0.1163  0.1595  0.3870 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)                 0.788147   0.252663   3.119 0.001835 ** 
EEDUCsome.hs1              -0.013124   0.043858  -0.299 0.764779    
EEDUCHS.diploma1            0.034311   0.037669   0.911 0.362473    
EEDUCsome.coll1             0.050176   0.037371   1.343 0.179515    
EEDUCassoc.deg1             0.058905   0.037941   1.553 0.120675    
EEDUCbach.deg1              0.077055   0.037624   2.048 0.040673 *  
EEDUCadv.deg1               0.087739   0.038190   2.297 0.021686 *  
MSmarried1                 -0.074696   0.050735  -1.472 0.141081    
MSwidowed1                 -0.032115   0.051464  -0.624 0.532676    
MSdivorced1                -0.065361   0.050840  -1.286 0.198708    
MSseparated1               -0.093259   0.054275  -1.718 0.085883 .  
MSnever1                   -0.073363   0.050807  -1.444 0.148888    
RRACEBlack1                -0.025815   0.011729  -2.201 0.027846 *  
RRACEAsian1                 0.027957   0.018603   1.503 0.133022    
RRACEOther1                 0.002408   0.016062   0.150 0.880829    
Hispanic1                   0.013293   0.011108   1.197 0.231551    
GENID_DESCRIBEmale1        -0.014440   0.050826  -0.284 0.776351    
GENID_DESCRIBEfemale1      -0.034225   0.050676  -0.675 0.499509    
GENID_DESCRIBEtransgender1  0.060239   0.071508   0.842 0.399647    
GENID_DESCRIBEother1       -0.098396   0.060588  -1.624 0.104511    
REGIONSouth1               -0.042335   0.011218  -3.774 0.000165 ***
REGIONMidwest1             -0.031177   0.012254  -2.544 0.011017 *  
REGIONWest1                -0.029612   0.011492  -2.577 0.010035 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3336 on 2256 degrees of freedom
Multiple R-squared:  0.03906,	Adjusted R-squared:  0.02969 
F-statistic: 4.168 on 22 and 2256 DF,  p-value: 3.075e-10"

pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$vaxx)
"true
pred       0    1
FALSE  217  935
TRUE   123 1032"
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
"Call:
  glm(formula = sobj$formula, family = binomial, data = sobj$data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.7556   0.3487   0.4706   0.5842   1.1414  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                -10.91051  680.70045  -0.016 0.987212    
EEDUCsome.hs1               -0.04703    0.31649  -0.149 0.881863    
EEDUCHS.diploma1             0.25704    0.27765   0.926 0.354553    
EEDUCsome.coll1              0.37377    0.27594   1.355 0.175563    
EEDUCassoc.deg1              0.45170    0.28270   1.598 0.110088    
EEDUCbach.deg1               0.64263    0.28199   2.279 0.022669 *  
EEDUCadv.deg1                0.78110    0.29492   2.649 0.008085 ** 
MSmarried1                  -6.91121  215.09780  -0.032 0.974368    
MSwidowed1                  -6.46314  215.09783  -0.030 0.976029    
MSdivorced1                 -6.82640  215.09781  -0.032 0.974682    
MSseparated1                -7.03014  215.09785  -0.033 0.973927    
MSnever1                    -6.89822  215.09781  -0.032 0.974416    
RRACEBlack1                 -0.18423    0.09364  -1.967 0.049140 *  
RRACEAsian1                  0.40372    0.23986   1.683 0.092352 .  
RRACEOther1                  0.02999    0.14652   0.205 0.837804    
Hispanic1                    0.11121    0.10114   1.100 0.271509    
GENID_DESCRIBEmale1         -0.11504    0.53192  -0.216 0.828780    
GENID_DESCRIBEfemale1       -0.30190    0.53031  -0.569 0.569158    
GENID_DESCRIBEtransgender1   6.77827  216.67251   0.031 0.975043    
GENID_DESCRIBEother1        -0.69962    0.57771  -1.211 0.225885    
REGIONSouth1                -0.45791    0.12266  -3.733 0.000189 ***
REGIONMidwest1              -0.36347    0.13163  -2.761 0.005759 ** 
REGIONWest1                 -0.34691    0.12709  -2.730 0.006339 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1779.0  on 2278  degrees of freedom
Residual deviance: 1684.5  on 2256  degrees of freedom
AIC: 1730.5

Number of Fisher Scoring iterations: 14"
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$vaxx)
" true
pred       0    1
  FALSE    1    2
  TRUE   339 1965"


#Adding more variables to the regression. 

d_income <- data.frame(model.matrix(~ dat_use$INCOME))
d_pubhlth <- data.frame(model.matrix(~ dat_use$PUBHLTH))
d_privhlth <- data.frame(model.matrix(~ dat_use$PUBHLTH))
d_sexualorientation <- data.frame(model.matrix(~ dat_use$SEXUAL_ORIENTATION))

levels(dat_use$INCOME)
levels(dat_use$PUBHLTH)
levels(dat_use$PRIVHLTH)
levels(dat_use$SEXUAL_ORIENTATION)

dat_for_analysis_sub <- data.frame(
  d_vaxx[,2],
  d_educ[!is.na(dat_use$vaxx),2:7],
  d_marstat[!is.na(dat_use$vaxx),2:6],
  d_race[!is.na(dat_use$vaxx),2:4],
  d_hispanic[!is.na(dat_use$vaxx),2],
  d_gender[!is.na(dat_use$vaxx),2:5],
  d_region[!is.na(dat_use$vaxx),2:4],
  d_pubhlth[!is.na(dat_use$vaxx),2:3],
  d_sexualorientation[!is.na(dat_use$vaxx),2:6])


names(dat_for_analysis_sub) <- sub("dat_use.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "vaxx"
names(dat_for_analysis_sub)[24] <- "PUBHLTHno.public.health.ins"
names(dat_for_analysis_sub)[25] <- "PUBHLTHNA" 
names(dat_for_analysis_sub)[26] <- "SEXUAL_ORIENTATIONgay.or.lesbian"
names(dat_for_analysis_sub)[27] <- "SEXUALORIENTATIONstraight"
names(dat_for_analysis_sub)[28] <- "SEXUALORIENTATIONbisexual"
names(dat_for_analysis_sub)[29] <- "SEXUALORIENTATIONsomething.else"
names(dat_for_analysis_sub)[30] <- "SEXUALORIENTATIONdont.know"
summary(dat_for_analysis_sub)
"d_vaxx...2.     dat_use.EEDUCsome.hs dat_use.EEDUCHS.diploma dat_use.EEDUCsome.coll
 Min.   :0.0000   Min.   :0.00000      Min.   :0.0000          Min.   :0.0000        
 1st Qu.:1.0000   1st Qu.:0.00000      1st Qu.:0.0000          1st Qu.:0.0000        
 Median :1.0000   Median :0.00000      Median :0.0000          Median :0.0000        
 Mean   :0.8602   Mean   :0.02333      Mean   :0.1858          Mean   :0.2981        
 3rd Qu.:1.0000   3rd Qu.:0.00000      3rd Qu.:0.0000          3rd Qu.:1.0000        
 Max.   :1.0000   Max.   :1.00000      Max.   :1.0000          Max.   :1.0000        
 dat_use.EEDUCassoc.deg dat_use.EEDUCbach.deg dat_use.EEDUCadv.deg dat_use.MSmarried dat_use.MSwidowed
 Min.   :0.000          Min.   :0.0000        Min.   :0.0000       Min.   :0.0000    Min.   :0.0000   
 1st Qu.:0.000          1st Qu.:0.0000        1st Qu.:0.0000       1st Qu.:0.0000    1st Qu.:0.0000   
 Median :0.000          Median :0.0000        Median :0.0000       Median :0.0000    Median :0.0000   
 Mean   :0.145          Mean   :0.2176        Mean   :0.1193       Mean   :0.3201    Mean   :0.1101   
 3rd Qu.:0.000          3rd Qu.:0.0000        3rd Qu.:0.0000       3rd Qu.:1.0000    3rd Qu.:0.0000   
 Max.   :1.000          Max.   :1.0000        Max.   :1.0000       Max.   :1.0000    Max.   :1.0000   
 dat_use.MSdivorced dat_use.MSseparated dat_use.MSnever  dat_use.RRACEBlack dat_use.RRACEAsian
 Min.   :0.0000     Min.   :0.00000     Min.   :0.0000   Min.   :0.0000     Min.   :0.00000   
 1st Qu.:0.0000     1st Qu.:0.00000     1st Qu.:0.0000   1st Qu.:0.0000     1st Qu.:0.00000   
 Median :0.0000     Median :0.00000     Median :0.0000   Median :0.0000     Median :0.00000   
 Mean   :0.2641     Mean   :0.02944     Mean   :0.2715   Mean   :0.1151     Mean   :0.03445   
 3rd Qu.:1.0000     3rd Qu.:0.00000     3rd Qu.:1.0000   3rd Qu.:0.0000     3rd Qu.:0.00000   
 Max.   :1.0000     Max.   :1.00000     Max.   :1.0000   Max.   :1.0000     Max.   :1.00000   
 dat_use.RRACEOther d_hispanic..is.na.dat_use.vaxx...2. dat_use.GENID_DESCRIBEmale
 Min.   :0.00000    Min.   :0.0000                      Min.   :0.0000            
 1st Qu.:0.00000    1st Qu.:0.0000                      1st Qu.:0.0000            
 Median :0.00000    Median :0.0000                      Median :0.0000            
 Mean   :0.05539    Mean   :0.1197                      Mean   :0.3317            
 3rd Qu.:0.00000    3rd Qu.:0.0000                      3rd Qu.:1.0000            
 Max.   :1.00000    Max.   :1.0000                      Max.   :1.0000            
 dat_use.GENID_DESCRIBEfemale dat_use.GENID_DESCRIBEtransgender dat_use.GENID_DESCRIBEother
 Min.   :0.0000               Min.   :0.000000                  Min.   :0.00000            
 1st Qu.:0.0000               1st Qu.:0.000000                  1st Qu.:0.00000            
 Median :1.0000               Median :0.000000                  Median :0.00000            
 Mean   :0.6465               Mean   :0.004579                  Mean   :0.01243            
 3rd Qu.:1.0000               3rd Qu.:0.000000                  3rd Qu.:0.00000            
 Max.   :1.0000               Max.   :1.000000                  Max.   :1.00000            
 dat_use.REGIONSouth dat_use.REGIONMidwest   REGIONWest     PUBHLTHno.public.health.ins   PUBHLTHNA      
 Min.   :0.0000      Min.   :0.0000        Min.   :0.0000   Min.   :0.0000              Min.   :0.00000  
 1st Qu.:0.0000      1st Qu.:0.0000        1st Qu.:0.0000   1st Qu.:0.0000              1st Qu.:0.00000  
 Median :0.0000      Median :0.0000        Median :0.0000   Median :0.0000              Median :0.00000  
 Mean   :0.3539      Mean   :0.2032        Mean   :0.3103   Mean   :0.3973              Mean   :0.04056  
 3rd Qu.:1.0000      3rd Qu.:0.0000        3rd Qu.:1.0000   3rd Qu.:1.0000              3rd Qu.:0.00000  
 Max.   :1.0000      Max.   :1.0000        Max.   :1.0000   Max.   :1.0000              Max.   :1.00000  
 SEXUAL_ORIENTATIONgay.or.lesbian SEXUALORIENTATIONstraight SEXUALORIENTATIONbisexual
 Min.   :0.00000                  Min.   :0.0000            Min.   :0.00000          
 1st Qu.:0.00000                  1st Qu.:1.0000            1st Qu.:0.00000          
 Median :0.00000                  Median :1.0000            Median :0.00000          
 Mean   :0.04012                  Mean   :0.8659            Mean   :0.04754          
 3rd Qu.:0.00000                  3rd Qu.:1.0000            3rd Qu.:0.00000          
 Max.   :1.00000                  Max.   :1.0000            Max.   :1.00000          
 SEXUALORIENTATIONsomething.else SEXUALORIENTATIONdont.know
 Min.   :0.00000                 Min.   :0.0000            
 1st Qu.:0.00000                 1st Qu.:0.0000            
 Median :0.00000                 Median :0.0000            
 Mean   :0.01962                 Mean   :0.0157            
 3rd Qu.:0.00000                 3rd Qu.:0.0000            
 Max.   :1.00000                 Max.   :1.0000"

require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$vaxx)
restrict_1 <- (runif(NN) < 0.5) 
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
sobj <- standardize(vaxx ~  EEDUCsome.hs + EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
                      MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
                      Hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
                      REGIONSouth + REGIONMidwest + REGIONWest + PUBHLTHno.public.health.ins + SEXUAL_ORIENTATIONgay.or.lesbian + SEXUALORIENTATIONstraight + SEXUALORIENTATIONbisexual + SEXUALORIENTATIONsomething.else + SEXUALORIENTATIONdont.know
                    , dat_train, family = binomial)
s_dat_test <- predict(sobj, dat_test)
summary(s_dat_test)
" EEDUCsome.hs EEDUCHS.diploma EEDUCsome.coll EEDUCassoc.deg EEDUCbach.deg EEDUCadv.deg MSmarried MSwidowed
 1:  58       1: 411          1: 716         1: 337         1: 492        1: 264       1: 746    1: 251   
 0:2249       0:1896          0:1591         0:1970         0:1815        0:2043       0:1561    0:2056   
 MSdivorced MSseparated MSnever  RRACEBlack RRACEAsian RRACEOther Hispanic GENID_DESCRIBEmale
 1: 613     1:  66      1: 620   1: 284     1:  72     1: 137     1: 278   1: 738            
 0:1694     0:2241      0:1687   0:2023     0:2235     0:2170     0:2029   0:1569            
 GENID_DESCRIBEfemale GENID_DESCRIBEtransgender GENID_DESCRIBEother REGIONSouth REGIONMidwest REGIONWest
 1:1516               1:  10                    1:  32              1: 823      1: 460        1: 728    
 0: 791               0:2297                    0:2275              0:1484      0:1847        0:1579    
 PUBHLTHno.public.health.ins SEXUAL_ORIENTATIONgay.or.lesbian SEXUALORIENTATIONstraight
 1: 900                      1:  95                           1:1993                   
 0:1407                      0:2212                           0: 314                   
 SEXUALORIENTATIONbisexual SEXUALORIENTATIONsomething.else SEXUALORIENTATIONdont.know
 1: 104                    1:  55                          1:  35                    
 0:2203                    0:2252                          0:2272"

model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
"Call:
lm(formula = sobj$formula, data = sobj$data)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.02851  0.04895  0.11242  0.16342  0.40916 

Coefficients:
                                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)                        0.7438627  0.2707337   2.748  0.00605 ** 
EEDUCsome.hs1                     -0.0076705  0.0439919  -0.174  0.86160    
EEDUCHS.diploma1                   0.0391757  0.0378245   1.036  0.30044    
EEDUCsome.coll1                    0.0525730  0.0375077   1.402  0.16116    
EEDUCassoc.deg1                    0.0616161  0.0381009   1.617  0.10598    
EEDUCbach.deg1                     0.0805911  0.0377423   2.135  0.03284 *  
EEDUCadv.deg1                      0.0886282  0.0383226   2.313  0.02083 *  
MSmarried1                        -0.0778373  0.0506284  -1.537  0.12433    
MSwidowed1                        -0.0398392  0.0513894  -0.775  0.43828    
MSdivorced1                       -0.0683128  0.0507290  -1.347  0.17824    
MSseparated1                      -0.0967707  0.0541682  -1.786  0.07416 .  
MSnever1                          -0.0748884  0.0507632  -1.475  0.14029    
RRACEBlack1                       -0.0258914  0.0117371  -2.206  0.02749 *  
RRACEAsian1                        0.0288104  0.0185669   1.552  0.12087    
RRACEOther1                        0.0027452  0.0160416   0.171  0.86413    
Hispanic1                          0.0152178  0.0111207   1.368  0.17132    
GENID_DESCRIBEmale1               -0.0074878  0.0508091  -0.147  0.88285    
GENID_DESCRIBEfemale1             -0.0268439  0.0506663  -0.530  0.59629    
GENID_DESCRIBEtransgender1         0.0526377  0.0721733   0.729  0.46588    
GENID_DESCRIBEother1              -0.0941008  0.0610970  -1.540  0.12366    
REGIONSouth1                      -0.0390227  0.0112291  -3.475  0.00052 ***
REGIONMidwest1                    -0.0296134  0.0122409  -2.419  0.01563 *  
REGIONWest1                       -0.0299477  0.0114768  -2.609  0.00913 ** 
PUBHLTHno.public.health.ins1      -0.0239483  0.0074604  -3.210  0.00135 ** 
SEXUAL_ORIENTATIONgay.or.lesbian1  0.0004653  0.0375905   0.012  0.99013    
SEXUALORIENTATIONstraight1        -0.0322725  0.0331216  -0.974  0.32998    
SEXUALORIENTATIONbisexual1        -0.0131323  0.0367139  -0.358  0.72061    
SEXUALORIENTATIONsomething.else1  -0.0016297  0.0441547  -0.037  0.97056    
SEXUALORIENTATIONdont.know1       -0.0546284  0.0430517  -1.269  0.20461    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3328 on 2250 degrees of freedom
Multiple R-squared:  0.04614,	Adjusted R-squared:  0.03427 
F-statistic: 3.887 on 28 and 2250 DF,  p-value: 3.762e-11"

pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$vaxx)
"true
pred       0    1
FALSE  222  956
TRUE   118 1011"
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
"Call:
glm(formula = sobj$formula, family = binomial, data = sobj$data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7655   0.3277   0.4600   0.5879   1.2294  

Coefficients:
                                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)                       -11.78927  684.13168  -0.017 0.986251    
EEDUCsome.hs1                       0.01910    0.32008   0.060 0.952426    
EEDUCHS.diploma1                    0.32450    0.28135   1.153 0.248770    
EEDUCsome.coll1                     0.41726    0.27947   1.493 0.135430    
EEDUCassoc.deg1                     0.50243    0.28682   1.752 0.079820 .  
EEDUCbach.deg1                      0.70255    0.28556   2.460 0.013884 *  
EEDUCadv.deg1                       0.81687    0.29864   2.735 0.006232 ** 
MSmarried1                         -6.92131  216.31906  -0.032 0.974475    
MSwidowed1                         -6.51478  216.31909  -0.030 0.975974    
MSdivorced1                        -6.83101  216.31907  -0.032 0.974808    
MSseparated1                       -7.04819  216.31911  -0.033 0.974008    
MSnever1                           -6.89434  216.31907  -0.032 0.974575    
RRACEBlack1                        -0.18781    0.09436  -1.990 0.046559 *  
RRACEAsian1                         0.40220    0.23834   1.687 0.091512 .  
RRACEOther1                         0.03627    0.14786   0.245 0.806254    
Hispanic1                           0.13138    0.10209   1.287 0.198150    
GENID_DESCRIBEmale1                -0.03236    0.53340  -0.061 0.951627    
GENID_DESCRIBEfemale1              -0.21965    0.53167  -0.413 0.679515    
GENID_DESCRIBEtransgender1          6.67890  216.53120   0.031 0.975393    
GENID_DESCRIBEother1               -0.66142    0.58414  -1.132 0.257512    
REGIONSouth1                       -0.42847    0.12327  -3.476 0.000509 ***
REGIONMidwest1                     -0.34947    0.13211  -2.645 0.008161 ** 
REGIONWest1                        -0.35364    0.12757  -2.772 0.005568 ** 
PUBHLTHno.public.health.ins1       -0.21133    0.06621  -3.192 0.001414 ** 
SEXUAL_ORIENTATIONgay.or.lesbian1  -0.12233    0.56795  -0.215 0.829461    
SEXUALORIENTATIONstraight1         -0.54935    0.51773  -1.061 0.288658    
SEXUALORIENTATIONbisexual1         -0.38056    0.53947  -0.705 0.480543    
SEXUALORIENTATIONsomething.else1   -0.23659    0.59500  -0.398 0.690901    
SEXUALORIENTATIONdont.know1        -0.74538    0.56160  -1.327 0.184429    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1779.0  on 2278  degrees of freedom
Residual deviance: 1666.4  on 2250  degrees of freedom
AIC: 1724.4

Number of Fisher Scoring iterations: 14"

pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$vaxx)
"true
pred       0    1
FALSE    2    1
TRUE   338 1966"
