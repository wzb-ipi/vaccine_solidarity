---
title: "Paper: Solidarity and Performance Voting'"
author:
 - Heike Klüver, Humboldt
 - Felix Hartmann, Humboldt
 - Macartan Humphreys, WZB and Columbia
 - Ferdinand Geißler, Humboldt
 - Johannes Giesecke, Humboldt
date: "May 2021"
output:
  html_document:
    number_sections: yes
    toc: yes
    toc_float: yes
    toc_depth: 2
    self_contained: yes
    code_folding: hide
  pdf_document:
    toc: yes
  word_document:
    toc: yes
    toc_depth: '2'
keywords: vaccine hesitancy, covid, transaction costs
theme: null
abstract: "Respondents support high levels of cash and vaccine transfers to help poorer nations. They are more supportive when health and economic risks of failing to provide support are high; but for the most part support does not depend on these calculations. Preferences over German policy are largely independent of what other countries are doing. Median support for vaccines (90 million) and cash (2 billion) corresponds closely to actual  (100 million doses and 2.2 billion cash)."
bibliography: references.bib  
editor_options:
  chunk_output_type: console
---


# Setup


```r
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE, comment=NA)
options(qwraps2_markup = "markdown")

## Packages
if (!require(pacman)) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(
  ggpubr,      # for ggarrange
  ggridges,    # plot
  cregg,       # conjoint
  readxl,      # read excel
  gtrendsR,    # google trends
  aod,         # hypothesis testing (1.3.1)
  car,         # linear hypothesis testing for causal tree (3.0-2)
  corrplot,    # Correlations (0.84)
  DeclareDesign, 
  dplyr,       # Data manipulation (0.8.0.1)
  evtree,      # evolutionary learning of globally optimal trees (1.0-7)
  fastDummies,
  fBasics,     # Summary statistics (3042.89)
  ggplot2,
  grf,         # Generalized random forests (0.10.2)
  haven,       # load sav
  kableExtra,  # Prettier RMarkdown (1.0.1)
  knitr,
  labelled,
  psych,       # Correlation p-values (1.8.12)
  purrr,
  rpart,       # Classification and regression trees, or CART (4.1-13)
  reshape2,
  rpart.plot,  # Plotting trees (3.0.6)
  readr,       # Reading csv files (1.3.1)
  sjlabelled,
  stats,
  summarytools,
  texreg,
  tidyverse,
  tidyselect,
  tidyr,       # Database operations (0.8.3)
  treeClust,   # Predicting leaf position for causal trees (1.1-7)
  tibble)      # Modern alternative to data frames (2.1.1)


# Set seed for reproducibility
set.seed(201911) 
```



```r
deal_labels = c("No deal"=0, "20 give 20 bn"=1, "40 give 20b"=2,
                "20 give 40 bn"=3, "40 give 40 bn"=4)

# c('Control: kein Abkommen'=0,
#   'Treatment: Abkommen, 20 Länder, 20 Milliarden'=1,
#   'Treatment: Abkommen, 40 Länder, 20 Milliarden'=2,
#   'Treatment: Abkommen, 20 Länder, 40 Milliarden'=3,
#   'Treatment: Abkommen, 40 Länder, 40 Milliarden'=4)

main_df <- read_csv('1_input/combined.csv')

w4 <- main_df %>% dplyr::filter(wave==4)

# labs <- var_label(w4) %>% unlist
# data.frame(names(labs), labs) %>% kable()

# creating vignettes

vign_values <- 
  expand.grid(trading_importance = 0:1, risk=0:1, deal=0:4) %>% 
  arrange(trading_importance, risk) %>% 
  set_value_labels(
    trading_importance = c('Control: Wirtschaft bleibt unberührt'=0,
                                        'Treatment: Wirtschaft schrumpft um 5%'=1),
                   risk=c('Control: Kein Risiko durch Mutationen'=0,
                          'Treatment: Erhöhtes Risiko durch Mutationen'=1),
                   deal= deal_labels) %>%
  mutate(vignr=1:20) %>% as_tibble()

df <- 
  list(
    w4 %>%
      mutate(vignr = as.numeric(run1_exp8)) %>% left_join(vign_values) %>%
      mutate(round = 1,
         cash = 	exp7_money1,
         doses = exp7_doses1) %>%
      select(group, round, cash, doses, trading_importance, risk, deal, ID, perspective_fed_indian, perspective_fed_german, vaccinated),
    
    w4 %>%
      mutate(vignr = as.numeric(run2_exp8)) %>% left_join(vign_values) %>%
      mutate(round = 2,
         cash = 	exp7_money2,
         doses =  exp7_doses2) %>%
      select(group, round, cash, doses, trading_importance, risk, deal, ID, perspective_fed_indian, perspective_fed_german, vaccinated)) %>% 
  bind_rows %>%
  dplyr::filter(group !=3) %>% 
  mutate(id = ID,
         others_number = (deal==1 | deal == 3)*2 + (deal==2 | deal == 4)*4,
         others_giving = (deal==1 | deal == 2)*2 + (deal==3 | deal == 4)*4,
         others_average = others_giving/others_number, 
         others_average = ifelse(others_number==0, 0, others_average), 
         deal = factor(deal, deal_labels, names(deal_labels)),
         risk_factor = factor(risk, 0:1, c("Low", "High")),
         trading_factor = factor(trading_importance, 0:1, c("Low", "High")),
         risk = risk - mean(risk),
         trading_importance = trading_importance - mean(trading_importance),
         others_number_norm = others_number - mean(others_number),
         others_giving_norm = others_giving - mean(others_giving),
         cash_billions = 	cash*1000,
         cash_billions_log = 	log(1 + cash_billions))
```

# Raw patterns

## Coarsened histograms


```r
cash_histogram <- 
  df %>% mutate(coarsened_cash = ifelse(cash_billions > 12, 12, cash_billions)) %>% ggplot(aes(coarsened_cash)) + geom_histogram() + 
  xlab("Cash (censored at 12 billion") 

cash_histogram  
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
# Support sizes

amounts <- seq(0, 22, .5)

support <- function(df)
  data.frame(
    amounts = seq(0, 22, .5),
    support = sapply(amounts, function(j) 
      mean(df$cash_billions >= j, na.rm = TRUE)))

s1 <- list(
  Low = support(
    df %>% dplyr::filter(risk_factor == "Low" & trading_factor == "Low")),
  High  =  support(
    df %>% dplyr::filter(risk_factor == "High" & trading_factor == "High"))) %>% 
  bind_rows(.id = "Costs")

s2 <- list(
  Low = support(
    df %>% dplyr::filter(deal == "No deal")),
  High  =  support(
    df %>% dplyr::filter(deal == "40 give 40 bn"))) %>% 
  bind_rows(.id = "Mulilateralism")

supports <- ggarrange(
s1 %>% ggplot(aes(amounts, support, color = Costs)) + geom_line() + ylim(0,1) + theme_bw() + xlab("German contribution (bn)") + ylab("Share supporting")  + theme(legend.position="bottom"),
s2 %>% ggplot(aes(amounts, support, color = Mulilateralism)) + geom_line() + ylim(0,1) + theme_bw() + xlab("German contribution (bn)") + ylab("Share supporting")   + theme(legend.position="bottom"), nrow = 2)

pdf("2_output/cumulative.pdf", height = 5, width = 8)
 supports 
dev.off()
```

```
png 
  2 
```


```r
raw  <- 
  df %>% 
  mutate(costs = ((risk_factor == "High") + 2*(trading_factor=="High"))) %>%
  group_by(trading_factor, risk, deal, costs) %>% 
  summarize(cash_bn = median(cash_billions, na.rm = TRUE),
            doses = median(doses, na.rm = TRUE))  %>%
  gather("outcome","value",  -risk,  -deal, -trading_factor, -costs)

medians_plot <- 
raw %>% ggplot(aes(risk, value, color = trading_factor)) + 
  facet_grid(outcome ~ deal, scales = "free_y") + geom_point()  + geom_line() + ylim(c(0, NA)) + theme_bw() + ylab(" ")+
  scale_x_continuous(breaks=c(-.45,.45), labels = c("Low", "High")) 

pdf("2_output/medians.pdf", height = 5, width = 8)
 medians_plot 
dev.off()
```

```
png 
  2 
```

```r
# Note: use bootstrapping to add standard errors on the median?



observation_plot <- function(data = df)
data %>% dplyr::select(risk_factor, deal, trading_factor, cash_billions, doses, vaccinated) %>%
  mutate(cash_billions = ifelse(cash_billions>20, 20, cash_billions)) %>%
  gather("outcome","value",  -risk_factor,  -deal, -trading_factor, - vaccinated) %>%
  mutate(outcome = factor(outcome, c("cash_billions", "doses"), c("Billion Euros", "Million doses"))) %>% 
  ggplot(aes(risk_factor, value, color = trading_factor)) + facet_grid(outcome ~ deal, scales = "free_y") + geom_boxplot() + ylim(c(0, NA)) + theme_bw() + 
  scale_y_continuous(trans='sqrt') + ylab(" ") + xlab("Mutation risk") 


observation_plot()
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-3-1.png" width="672" />


Basic variation by vaccination status


```r
df %>% group_by(vaccinated) %>% 
  summarize(median_cash = median(cash_billions),
            mean_cash = mean(cash_billions),
            median_doses = median(doses),
            mean_doses = mean(doses),
            )
```

```
# A tibble: 2 x 5
  vaccinated median_cash mean_cash median_doses mean_doses
       <dbl>       <dbl>     <dbl>        <dbl>      <dbl>
1          0           1      8.39           50       64.9
2          1           2      8.72          100       80.5
```


```r
main_results <- 
  list(
  cash = lm_robust(cash ~ trading_importance*risk*others_number*others_giving  + round, fixed_effects = ~id,  se_type = "stata", data = df) %>% tidy,
  doses = lm_robust(doses ~ trading_importance*risk*others_number*others_giving  + round, fixed_effects = ~id,  se_type = "stata", data = df) %>% tidy) %>% bind_rows()


 treatments <- c("trading_importance", "risk", "others_number", "others_giving")
 treatment_labels <- c("Trading importance", "Risk", "Number of others giving (10s)", "Amount given by others\n(10s of billions)")
```

# Descriptives

The average proposal is: 
0.01 billion Euros and 
78 million doses. 

The median proposal is: 
2 billion Euros and 
90 million doses. 
 
These are close to actual [https://www.auswaertiges-amt.de/en/aussenpolitik/themen/gesundheit/covax/2396914](pledges).


The median in risk conditions is proposal is: 
2 billion Euros and 
100 million doses. 


```r
figure_1 <-
  
  main_results %>% 
  dplyr::filter(term %in% treatments) %>% 
  mutate(Treatment = factor(term, treatments, treatment_labels)) %>%
  
  ggplot(aes(estimate, Treatment)) + geom_point()+
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = .1)+
  geom_vline(xintercept=0, linetype="longdash", lwd=0.35, colour = "#B55555") + 
  theme_bw() + facet_grid(~outcome, scales = "free_x")+
  ylab("")


df_mean <- df %>% 
  group_by(risk_factor) %>% 
  summarize(average = mean(cash, na.rm = TRUE)) %>%
  ungroup() 

figure_2 <-
  df %>% 
  ggplot(aes(risk_factor, cash)) + 
  geom_boxplot() + ylab("Billions of Euros")   + xlab("Risk")  + scale_y_sqrt() +
  geom_point(data = df_mean,
             mapping = aes(x = risk_factor, y = average),
             color="red") 

figure_3 <-
  df %>% 
  ggplot(aes(risk_factor, doses)) + 
  geom_boxplot() + ylab("Millions of doses") + xlab("Risk")

figure_2
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-6-1.png" width="50%" />

```r
figure_3
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-6-2.png" width="50%" />

Note that the doses options was limited to between 0 and 200 m. 


Offers are responsive to risk and economic impacts but not much to coordination considerations.


```r
figure_1
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-7-1.png" width="672" />


Trading importance and risk are substitutes for cash but not doses.

Without demeaning:


```r
lm_robust(cash ~ trading_factor*risk_factor+ round, fixed_effects = ~id,  se_type = "stata", data = df) %>% tidy  %>% 
  kbl(digit =2) %>%
  kable_minimal()
```

<table class=" lightable-minimal" style='font-family: "Trebuchet MS", verdana, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> conf.low </th>
   <th style="text-align:right;"> conf.high </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:left;"> outcome </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> trading_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3.54 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 6.94 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> round </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_factorHigh:risk_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -2.01 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
</tbody>
</table>

Logging helps and substitution not as strong:


```r
lm_robust(log(cash+1) ~ trading_factor*risk_factor+ round, fixed_effects = ~id,  se_type = "stata", data = df) %>% tidy  %>% 
  kbl(digit =2) %>%
  kable_minimal()
```

<table class=" lightable-minimal" style='font-family: "Trebuchet MS", verdana, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> conf.low </th>
   <th style="text-align:right;"> conf.high </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:left;"> outcome </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> trading_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3.58 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 6.99 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> round </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_factorHigh:risk_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -2.01 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10521 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
</tbody>
</table>

## Full results from saturated models


```r
main_results %>% 
  kbl(digit =2) %>%
  kable_minimal()
```

<table class=" lightable-minimal" style='font-family: "Trebuchet MS", verdana, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> conf.low </th>
   <th style="text-align:right;"> conf.high </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:left;"> outcome </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> trading_importance </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.97 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 5.50 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> others_number </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.48 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 3.13 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> round </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -1.51 </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:others_number </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.51 </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk:others_number </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -1.77 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.13 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk:others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -2.16 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> others_number:others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -0.43 </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk:others_number </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk:others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.58 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:others_number:others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -1.38 </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk:others_number:others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 2.04 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk:others_number:others_giving </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -1.68 </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> cash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.24 </td>
   <td style="text-align:right;"> 0.51 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> -1.80 </td>
   <td style="text-align:right;"> 3.06 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk </td>
   <td style="text-align:right;"> 7.42 </td>
   <td style="text-align:right;"> 1.24 </td>
   <td style="text-align:right;"> 5.99 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 4.99 </td>
   <td style="text-align:right;"> 9.85 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> others_number </td>
   <td style="text-align:right;"> 1.09 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 3.15 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.41 </td>
   <td style="text-align:right;"> 1.77 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> others_giving </td>
   <td style="text-align:right;"> 1.90 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 5.43 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.22 </td>
   <td style="text-align:right;"> 2.59 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> round </td>
   <td style="text-align:right;"> 1.80 </td>
   <td style="text-align:right;"> 0.38 </td>
   <td style="text-align:right;"> 4.72 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.05 </td>
   <td style="text-align:right;"> 2.55 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk </td>
   <td style="text-align:right;"> -6.57 </td>
   <td style="text-align:right;"> 2.54 </td>
   <td style="text-align:right;"> -2.59 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> -11.55 </td>
   <td style="text-align:right;"> -1.59 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:others_number </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 1.33 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -0.43 </td>
   <td style="text-align:right;"> 2.21 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk:others_number </td>
   <td style="text-align:right;"> -0.38 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> -0.54 </td>
   <td style="text-align:right;"> 0.59 </td>
   <td style="text-align:right;"> -1.77 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:others_giving </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.69 </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 0.39 </td>
   <td style="text-align:right;"> -0.77 </td>
   <td style="text-align:right;"> 1.96 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk:others_giving </td>
   <td style="text-align:right;"> -1.44 </td>
   <td style="text-align:right;"> 0.68 </td>
   <td style="text-align:right;"> -2.13 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> -2.77 </td>
   <td style="text-align:right;"> -0.12 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> others_number:others_giving </td>
   <td style="text-align:right;"> -0.38 </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> -2.68 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> -0.65 </td>
   <td style="text-align:right;"> -0.10 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk:others_number </td>
   <td style="text-align:right;"> 2.12 </td>
   <td style="text-align:right;"> 1.36 </td>
   <td style="text-align:right;"> 1.56 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> -0.54 </td>
   <td style="text-align:right;"> 4.79 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk:others_giving </td>
   <td style="text-align:right;"> 2.37 </td>
   <td style="text-align:right;"> 1.39 </td>
   <td style="text-align:right;"> 1.70 </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> -0.36 </td>
   <td style="text-align:right;"> 5.10 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:others_number:others_giving </td>
   <td style="text-align:right;"> -0.24 </td>
   <td style="text-align:right;"> 0.27 </td>
   <td style="text-align:right;"> -0.91 </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> -0.77 </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk:others_number:others_giving </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:right;"> 1.28 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_importance:risk:others_number:others_giving </td>
   <td style="text-align:right;"> -0.85 </td>
   <td style="text-align:right;"> 0.54 </td>
   <td style="text-align:right;"> -1.57 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> -1.91 </td>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 10509 </td>
   <td style="text-align:left;"> doses </td>
  </tr>
</tbody>
</table>


## Experiment


```r
outcomes <- c("solidarity_behaviour", "solidarity_attitude")
outcome_labels <- c("Solidarity Bahaviour", "Solidarity Attitude")
w2_treatments <- c("treatment_video")
w2_treatment_labels <- c("Treatment")
```



```r
#r, child = "2_main_results.Rmd", eval = TRUE}
# put in separate files later

df_2 <- dplyr::filter(main_df, wave == 2)


# df_2$solidarity_bahaviour %>% 
#  hist(main = "Distribution of allocations from behavioral measure")



df_2 %>%
  ggplot(aes(factor(party_id), solidarity_behaviour)) + geom_boxplot()
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
  ggtitle("Distribution of allocations from behavioral measure")
```

$title
[1] "Distribution of allocations from behavioral measure"

attr(,"class")
[1] "labels"

```r
stargazer::stargazer(
  lm(solidarity_behaviour ~ treatment_video, data = df_2), 
  lm(solidarity_attitude ~ treatment_video, data = df_2), 
  header = FALSE, type = "latex")
```


\begin{table}[!htbp] \centering 
  \caption{} 
  \label{} 
\begin{tabular}{@{\extracolsep{5pt}}lcc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{2}{c}{\textit{Dependent variable:}} \\ 
\cline{2-3} 
\\[-1.8ex] & solidarity\_behaviour & solidarity\_attitude \\ 
\\[-1.8ex] & (1) & (2)\\ 
\hline \\[-1.8ex] 
 treatment\_video & 0.039$^{***}$ & 0.069$^{***}$ \\ 
  & (0.007) & (0.009) \\ 
  & & \\ 
 Constant & 0.489$^{***}$ & 0.474$^{***}$ \\ 
  & (0.005) & (0.006) \\ 
  & & \\ 
\hline \\[-1.8ex] 
Observations & 12,456 & 13,780 \\ 
R$^{2}$ & 0.002 & 0.005 \\ 
Adjusted R$^{2}$ & 0.002 & 0.005 \\ 
Residual Std. Error & 0.406 (df = 12454) & 0.499 (df = 13778) \\ 
F Statistic & 29.243$^{***}$ (df = 1; 12454) & 65.398$^{***}$ (df = 1; 13778) \\ 
\hline 
\hline \\[-1.8ex] 
\textit{Note:}  & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
\end{tabular} 
\end{table} 

```r
models_basic <- lapply(c(outcomes), function(y)
lm_robust(as.formula(paste(y, "~",   paste(w2_treatments, collapse =  "+"))), data = df_2))
names(models_basic) <- outcomes


figure_video <- 
  lapply(models_basic, tidy) %>% bind_rows(.id = "outcome") %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(outcome = factor(outcome, outcomes, outcome_labels)) %>% 
  ggplot(aes(estimate, outcome)) + geom_point() +
  #facet_grid(~outcome) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.3), width = .1)+
  theme_bw()+
  #ylab("Change in support for regime (0-1)") +
    geom_vline(xintercept=0, linetype="longdash", lwd=0.35, size=0.75,alpha=0.3, colour = "#B55555")  +
    xlab("") + ylab("")  +
    theme(#panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          #strip.background = element_blank(),
          # strip.text.y = element_blank(),
          #axis.text.y=element_blank(),
          legend.position = "top",
          text = element_text(size=20),
          legend.title=element_blank(),
          strip.text.y = element_text(angle = 180),
          axis.ticks.y=element_blank()) +
    scale_color_grey()


figure_video
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-12-2.png" width="672" />

## Longer term follow up



```r
table(df$ID %in% df_2$ID)
```

```

FALSE  TRUE 
 4250 16800 
```

```r
df <- left_join(df, df_2 %>% dplyr::select(ID, treatment_video))

lm_robust(log(cash+1) ~ trading_factor*risk_factor+ treatment_video + round,  se_type = "stata", data = df) %>% tidy  %>% 
  kbl(digit =2) %>%
  kable_minimal()
```

<table class=" lightable-minimal" style='font-family: "Trebuchet MS", verdana, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> conf.low </th>
   <th style="text-align:right;"> conf.high </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:left;"> outcome </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 17.96 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 16794 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_factorHigh </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -0.26 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16794 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk_factorHigh </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.99 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16794 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treatment_video </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.33 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16794 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> round </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16794 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_factorHigh:risk_factorHigh </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.23 </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16794 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
</tbody>
</table>


## Individual solidarity

Should an older Indian woman be prioritized over a young German?


```r
hist(df$perspective_fed_german - df$perspective_fed_indian)
```

<img src="0_master_new_3_files/figure-html/unnamed-chunk-14-1.png" width="672" />

```r
lm_robust(log(cash+1) ~ trading_factor*risk_factor+ treatment_video + round + perspective_fed_german +perspective_fed_indian,  se_type = "stata", data = df) %>% tidy  %>% 
  kbl(digit =2) %>%
  kable_minimal()
```

<table class=" lightable-minimal" style='font-family: "Trebuchet MS", verdana, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> conf.low </th>
   <th style="text-align:right;"> conf.high </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:left;"> outcome </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5.63 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> 0.85 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> risk_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.94 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treatment_video </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> round </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> perspective_fed_german </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.45 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> perspective_fed_indian </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 9.28 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trading_factorHigh:risk_factorHigh </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.29 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 16792 </td>
   <td style="text-align:left;"> log(cash + 1) </td>
  </tr>
</tbody>
</table>


## Structural



```r
structural_results <- 
  lm_robust(cash ~ trading_importance + risk + others_average, fixed_effects = ~id,  se_type = "stata", data = df) %>% tidy 

gamma <- structural_results$estimate[3] / (1- structural_results$estimate[3])
beta  <- 2*(1+gamma)*structural_results$estimate[1]
delta  <- 2*(1+gamma)*structural_results$estimate[2]

gamma_lwr <- structural_results$conf.low[3] / (1- structural_results$conf.low[3])
gamma_upr <- structural_results$conf.high[3] / (1- structural_results$conf.high[3])

b <- seq(structural_results$conf.low[3], structural_results$conf.high[3], length = 10)
b/(1-b)
```

```
 [1] 0.0003649013 0.0004184862 0.0004720767 0.0005256730 0.0005792751
 [6] 0.0006328829 0.0006864964 0.0007401157 0.0007937408 0.0008473715
```

```r
pars <- c(gamma = gamma, beta = beta, delta = delta, gamma_lwr = gamma_lwr, gamma_upr = gamma_upr)
```


## Observational data


```r
owd <- read.csv("1_input/owid-covid-data.csv")

 small_owd <- owd %>% 
   dplyr::filter(date == "2021-10-20" & continent != "") %>% 
   select(iso_code, location, gdp_per_capita, population, people_fully_vaccinated_per_hundred, total_deaths_per_million, total_vaccinations) %>% 
   left_join(
     owd %>% dplyr::filter(date == "2021-04-05" & continent != "") %>%
       select(iso_code, location, total_deaths_per_million) %>%
       rename(historical_deaths_per_million = total_deaths_per_million))
 
 small_owd$total_vaccinations %>% sum(na.rm = TRUE)
```

```
[1] 5978275742
```

```r
 table(small_owd$people_fully_vaccinated_per_hundred %>% is.na)
```

```

FALSE  TRUE 
   91   122 
```

```r
r2_gdp <- lm_robust(
  people_fully_vaccinated_per_hundred ~  gdp_per_capita,
  data = small_owd)$r.squared

r2_deaths_1 <- lm_robust(
  people_fully_vaccinated_per_hundred ~  total_deaths_per_million,
  data = small_owd)$r.squared

r2_deaths_0 <- lm_robust(
  people_fully_vaccinated_per_hundred ~  historical_deaths_per_million,
  data = small_owd)$r.squared

c(r2_gdp, r2_deaths_1, r2_deaths_0)
```

```
[1] 0.55147307 0.03399409 0.05109060
```

```r
80000000000/ small_owd %>% dplyr::filter(gdp_per_capita > 27000) %>% pull(population) %>% sum
```

```
[1] 70.05532
```

```r
pc <- 80000000000/ small_owd %>% dplyr::filter(gdp_per_capita > 27000) %>% pull(population) %>% sum

small_owd <- small_owd %>% mutate(
  bill = ifelse(gdp_per_capita > 27000, pc*population, 0)
)

small_owd %>% dplyr::filter(location == "Germany" | location == "United States" )
```

```
  iso_code      location gdp_per_capita population
1      DEU       Germany       45229.25   83900471
2      USA United States       54225.45  332915074
  people_fully_vaccinated_per_hundred total_deaths_per_million
1                               65.48                 1130.935
2                               56.46                 2201.201
  total_vaccinations historical_deaths_per_million        bill
1          110512109                       919.375  5877674670
2          413330348                      1667.122 23322473337
```


## Wave 2 conjoint



```r
label <- "1_input/W2_exp3_vignettes_universe.dta" %>% 
  read_dta() %>%  
    mutate(vignr = as.numeric(vignr))


label$vig_doses %>% table()
```

.
 0  1  2  3 
48 48 48 48 

```r
label
```

# A tibble: 192 x 6
    vig_doses  vig_dose_share  vig_countries vig_benefit_econ~ vig_benefit_heal~
    <dbl+lbl>       <dbl+lbl>      <dbl+lbl>         <dbl+lbl>         <dbl+lbl>
 1 0 [1 Mill~ 0 [Deutschland~ 0 [20 Staaten] 0 [Bedürftige St~ 0 [Bedürftige St~
 2 0 [1 Mill~ 0 [Deutschland~ 0 [20 Staaten] 0 [Bedürftige St~ 1 [Bedürftige St~
 3 0 [1 Mill~ 0 [Deutschland~ 0 [20 Staaten] 1 [Bedürftige St~ 0 [Bedürftige St~
 4 0 [1 Mill~ 0 [Deutschland~ 0 [20 Staaten] 1 [Bedürftige St~ 1 [Bedürftige St~
 5 0 [1 Mill~ 0 [Deutschland~ 1 [80 Staaten] 0 [Bedürftige St~ 0 [Bedürftige St~
 6 0 [1 Mill~ 0 [Deutschland~ 1 [80 Staaten] 0 [Bedürftige St~ 1 [Bedürftige St~
 7 0 [1 Mill~ 0 [Deutschland~ 1 [80 Staaten] 1 [Bedürftige St~ 0 [Bedürftige St~
 8 0 [1 Mill~ 0 [Deutschland~ 1 [80 Staaten] 1 [Bedürftige St~ 1 [Bedürftige St~
 9 0 [1 Mill~ 0 [Deutschland~ 2 [160 Staate~ 0 [Bedürftige St~ 0 [Bedürftige St~
10 0 [1 Mill~ 0 [Deutschland~ 2 [160 Staate~ 0 [Bedürftige St~ 1 [Bedürftige St~
# ... with 182 more rows, and 1 more variable: vignr <dbl>

```r
conjoints <- 
  
  list(
    conjoint1_1 =
      df_2 %>%
      mutate(vignr = as.numeric(c_0031_w2),
             outcome = conjoint_choice1_exp3, rating = conjoint_rating1_exp3, contest = 1, candidate=1) %>%
      select(ID, vignr, outcome, rating, contest, candidate),
    
  conjoint1_2 =
      df_2 %>%
      mutate(vignr = as.numeric(c_0032_w2),
             outcome = conjoint_choice1_exp3, rating = conjoint_rating2_exp3, contest = 1, candidate=2) %>%
      select(ID, vignr, outcome, rating, contest, candidate), 

    
  conjoint2_1 =
      df_2 %>%
      mutate(vignr = as.numeric(vignette3),
             outcome = conjoint_choice2_exp3, rating = conjoint_rating3_exp3, contest = 2, candidate=1) %>%
      select(ID, vignr, outcome, rating, contest, candidate), 

        
    conjoint2_2 =
      df_2 %>%
      mutate(vignr = as.numeric(vignette4),
             outcome = conjoint_choice2_exp3, rating = conjoint_rating4_exp3, contest = 2, candidate=2) %>%
      select(ID, vignr, outcome, rating, contest, candidate), 

            
                conjoint3_1 =
      df_2 %>%
      mutate(vignr = as.numeric(vignette5),
             outcome = conjoint_choice3_exp3, rating = conjoint_rating5_exp3, contest = 3, candidate=1) %>%
      select(ID, vignr, outcome, rating, contest, candidate), 

                
                    conjoint3_2 =
      df_2 %>%
      mutate(vignr = as.numeric(vignette6),
             outcome = conjoint_choice3_exp3, 
             rating = conjoint_rating6_exp3, contest = 3, candidate=2) %>%
      select(ID, vignr, outcome, rating, contest, candidate) 
                    
) %>% bind_rows() %>%
  mutate(outcome = ifelse(candidate ==1, as.numeric(outcome=="1"), as.numeric(outcome=="2"))) %>%
  left_join(label) %>%
  mutate(doses=dplyr::recode(vig_doses, 
                         "0" = "1 Million doses",
                         "1" = "5 Million doses",
                         "2" = "10 Million doses",
                         "3" = "20 Million doses"))%>% 
                   mutate(share=dplyr::recode(vig_dose_share, 
                         "0" = "1 % of the vaccines",
                         "1" = "5 % of the vaccines",
                         "2" = "10 % of the vaccines",
                         "3" = "20 % of the vaccines"))%>% 
                   mutate(number=dplyr::recode(vig_countries, 
                         "0" = "20 Staaten",
                         "1" = "80 countries",
                         "2" = "160 countries"))%>% 
                   mutate(economic_benefits=dplyr::recode(vig_benefit_economic, 
                         "0" = "Without economic importance",
                         "1" = "With economic importance"))%>% 
                   mutate(health_benefits=dplyr::recode(vig_benefit_health, 
                         "0" = "No risk of infection",
                         "1" = "Risk of infection")) %>%
  left_join(df_2 %>% dplyr::select(ID, treatment_video)) 

# Normalizae data
conjoints_norm <- conjoints %>% 
  mutate(doses_norm = vig_doses - mean(vig_doses),
         number_norm = vig_countries - mean(vig_countries),
         share_norm = vig_dose_share - mean(vig_dose_share),
         economic_benefits = vig_benefit_economic - mean(vig_benefit_economic),
         health_benefits = vig_benefit_health - mean(vig_benefit_health)) %>%
  group_by(ID) %>% mutate(rating = rating - mean(rating)) %>% ungroup %>%
  left_join(df_2 %>% dplyr::select(ID, treatment_video))


models_interactions <-
  list(
    rating0 = lm_robust(rating ~  doses_norm*share_norm*number_norm*economic_benefits*health_benefits, data = conjoints_norm),
    rating1 = lm_robust(rating ~  doses_norm*share_norm*number_norm*economic_benefits*health_benefits, data = conjoints_norm),
    choice0 = lm_robust(outcome ~  doses_norm*share_norm*number_norm*economic_benefits*health_benefits, data = conjoints_norm),
    choice1 = lm_robust(outcome ~  doses_norm*share_norm*number_norm*economic_benefits*health_benefits, data = conjoints_norm)
  )


models_treatments <-
  list(
    rating = lm_robust(rating ~  treatment_video*vig_doses + treatment_video*vig_dose_share + treatment_video*vig_countries + treatment_video*vig_benefit_economic + treatment_video*vig_benefit_health, 
                        data = conjoints_norm),
    choice = lm_robust(outcome ~  treatment_video*vig_doses + treatment_video*vig_dose_share + treatment_video*vig_countries + treatment_video*vig_benefit_economic + treatment_video*vig_benefit_health, 
                        data = conjoints_norm)
    )

htmlreg(models_interactions)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">rating0</th>
<th style="padding-left: 5px;padding-right: 5px;">rating1</th>
<th style="padding-left: 5px;padding-right: 5px;">choice0</th>
<th style="padding-left: 5px;padding-right: 5px;">choice1</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.50<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.50<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.50;  0.50]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.50;  0.50]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.07<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.07<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.08; -0.06]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.08; -0.06]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02; -0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02; -0.02]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.09<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.09<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.10; -0.08]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.10; -0.08]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02; -0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02; -0.02]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">number_norm</td>
<td style="padding-left: 5px;padding-right: 5px;">0.21<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.21<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.07<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.07<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.20;  0.23]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.20;  0.23]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.06;  0.07]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.06;  0.07]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.21<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.21<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.06<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.06<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.24; -0.19]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.24; -0.19]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.06; -0.05]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.06; -0.05]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.11<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.11<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.03<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.03<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.08;  0.13]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.08;  0.13]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.02;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.02;  0.04]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:number_norm</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01; -0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01; -0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm:number_norm</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm:economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.05]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.05]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">number_norm:economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">number_norm:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.10<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.10<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.04;  0.15]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.04;  0.15]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.02]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm:number_norm</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm:economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:number_norm:economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.05;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.05;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm:number_norm:economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.00]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:number_norm:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.05]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.05]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm:number_norm:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.04;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.07;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.07;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm:economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.05;  0.05]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.05;  0.05]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">number_norm:economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.05</td>
<td style="padding-left: 5px;padding-right: 5px;">0.05</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.12]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.12]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.03]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm:number_norm:economic_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm:number_norm:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm:economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.05;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.05;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:number_norm:economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.05</td>
<td style="padding-left: 5px;padding-right: 5px;">0.05</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.11]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.11]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.03]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.03]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">share_norm:number_norm:economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.06;  0.07]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.06;  0.07]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.02]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.02]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">doses_norm:share_norm:number_norm:economic_benefits:health_benefits</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.04</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.04</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.10;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.10;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.00]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.00]</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">82692</td>
<td style="padding-left: 5px;padding-right: 5px;">82692</td>
<td style="padding-left: 5px;padding-right: 5px;">82692</td>
<td style="padding-left: 5px;padding-right: 5px;">82692</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">RMSE</td>
<td style="padding-left: 5px;padding-right: 5px;">2.03</td>
<td style="padding-left: 5px;padding-right: 5px;">2.03</td>
<td style="padding-left: 5px;padding-right: 5px;">0.49</td>
<td style="padding-left: 5px;padding-right: 5px;">0.49</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="5"><sup>*</sup> Null hypothesis value outside the confidence interval.</td>
</tr>
</tfoot>
</table>

```r
htmlreg(models_treatments)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">rating</th>
<th style="padding-left: 5px;padding-right: 5px;">choice</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
<td style="padding-left: 5px;padding-right: 5px;">0.17<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.53<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.12;  0.23]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.52;  0.54]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">treatment_video</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.19<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.04<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.27; -0.11]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.06; -0.02]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">vig_doses</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.10<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.03<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.12; -0.08]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03; -0.03]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">vig_dose_share</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.10<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.02<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.11; -0.08]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03; -0.02]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">vig_countries</td>
<td style="padding-left: 5px;padding-right: 5px;">0.17<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.06<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.14;  0.19]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.05;  0.06]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">vig_benefit_economic</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.21<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-0.05<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.25; -0.17]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.06; -0.04]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">vig_benefit_health</td>
<td style="padding-left: 5px;padding-right: 5px;">0.13<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.04<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.09;  0.17]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.03;  0.05]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">treatment_video:vig_doses</td>
<td style="padding-left: 5px;padding-right: 5px;">0.06<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.02<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.03;  0.08]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.01;  0.03]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">treatment_video:vig_dose_share</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.01;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.00;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">treatment_video:vig_countries</td>
<td style="padding-left: 5px;padding-right: 5px;">0.10<sup>*</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.02<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.06;  0.13]</td>
<td style="padding-left: 5px;padding-right: 5px;">[ 0.01;  0.03]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">treatment_video:vig_benefit_economic</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.07;  0.04]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.02;  0.01]</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">treatment_video:vig_benefit_health</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.05</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.01<sup>*</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.10;  0.01]</td>
<td style="padding-left: 5px;padding-right: 5px;">[-0.03; -0.00]</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">82692</td>
<td style="padding-left: 5px;padding-right: 5px;">82692</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">RMSE</td>
<td style="padding-left: 5px;padding-right: 5px;">2.03</td>
<td style="padding-left: 5px;padding-right: 5px;">0.49</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="3"><sup>*</sup> Null hypothesis value outside the confidence interval.</td>
</tr>
</tfoot>
</table>

These seem inconsistent but they really tap into different questions; they suggest preferences for small doses and suggest preferences that are not ver responsive to other conditions.  

Number of states blows everything else out of the water. This is really about what types of deals does Germany want: Germans want deals to be multilateral with sharing. 

Also the benefits conditions are conceptually muddled with numbers conditions.
Note:

* Regressions take numbers at face value despite uneven gaps
* The economic result is  weird, is it possible there's backwards coding.



## Save workspace


```r
save.image("Solidarity.Rdata")
```


