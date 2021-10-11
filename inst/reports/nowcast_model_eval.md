---
title: "REPEL nowcast eval"
output: 
  html_document:
    keep_md: TRUE
---



## Variable importance
<details>
<summary>disease status overall variable importance</summary>
![](nowcast_model_eval_files/figure-html/ds-var-importance-1.png)<!-- -->
</details>

<details>
<summary>cases overall variable importance</summary>
![](nowcast_model_eval_files/figure-html/cs-var-importance-1.png)<!-- -->
</details>

<details>
<summary>disease status partial dependency of lag vars by disease</summary>
![](nowcast_model_eval_files/figure-html/ds-lag-pd-by-disease-1.png)<!-- -->
</details>

<details>
<summary>cases partial dependency of variables of interest by disease</summary>
![](nowcast_model_eval_files/figure-html/cs-lag-pd-by-disease-1.png)<!-- -->
</details>


## Validation








<details>
<summary>disease status confusion matrix</summary>
<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> .metric </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> model </th>
   <th style="text-align:right;"> full_model </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> accuracy </td>
   <td style="text-align:left;"> proportion of the data that are predicted correctly </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> accuracy </td>
   <td style="text-align:left;"> proportion of the data that are predicted correctly </td>
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kap </td>
   <td style="text-align:left;"> similar measure to accuracy(), but is normalized by the accuracy that would be expected by chance alone and is very useful when one or more classes have large frequency distributions. </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kap </td>
   <td style="text-align:left;"> similar measure to accuracy(), but is normalized by the accuracy that would be expected by chance alone and is very useful when one or more classes have large frequency distributions. </td>
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.84 </td>
  </tr>
</tbody>
</table>
</details>

<details>
<summary>disease status direction change confusion matrix</summary>
<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> .metric </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> model </th>
   <th style="text-align:right;"> full_model </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> accuracy </td>
   <td style="text-align:left;"> proportion of the data that are predicted correctly </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> accuracy </td>
   <td style="text-align:left;"> proportion of the data that are predicted correctly </td>
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kap </td>
   <td style="text-align:left;"> similar measure to accuracy(), but is normalized by the accuracy that would be expected by chance alone and is very useful when one or more classes have large frequency distributions. </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kap </td>
   <td style="text-align:left;"> similar measure to accuracy(), but is normalized by the accuracy that would be expected by chance alone and is very useful when one or more classes have large frequency distributions. </td>
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.42 </td>
  </tr>
</tbody>
</table>
Note there are baseline cases where disease status is positive but cases are NA, which are imputed in the model as 0. 
</details>

<details>
<summary>disease status direction change confusion matrix by select diseases</summary>
<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Disease </th>
   <th style="text-align:left;"> Baseline Accuracy </th>
   <th style="text-align:left;"> REPEL (Overall/New Outbreaks) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> foot and mouth disease </td>
   <td style="text-align:left;"> 60% </td>
   <td style="text-align:left;"> 94% / 84% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vesicular stomatitis </td>
   <td style="text-align:left;"> 61% </td>
   <td style="text-align:left;"> 88% / 59% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ovine bluetongue disease </td>
   <td style="text-align:left;"> 43% </td>
   <td style="text-align:left;"> 78% / 48% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ovine pox disease </td>
   <td style="text-align:left;"> 95% </td>
   <td style="text-align:left;"> 97% / 41% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> african horse sickness </td>
   <td style="text-align:left;"> 99% </td>
   <td style="text-align:left;"> 100% / 33% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lumpy skin disease </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 95% / 30% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> peste des petits ruminants </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 96% / 28% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> newcastle disease </td>
   <td style="text-align:left;"> 82% </td>
   <td style="text-align:left;"> 91% / 20% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> african swine fever </td>
   <td style="text-align:left;"> 95% </td>
   <td style="text-align:left;"> 96% / 11% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> highly pathogenic avian influenza </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 93% / 8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> classical swine fever </td>
   <td style="text-align:left;"> 96% </td>
   <td style="text-align:left;"> 97% / 7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pleuropneumonia </td>
   <td style="text-align:left;"> 96% </td>
   <td style="text-align:left;"> 98% / 5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> swine vesicular disease </td>
   <td style="text-align:left;"> 100% </td>
   <td style="text-align:left;"> 100% / 0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rift valley fever </td>
   <td style="text-align:left;"> 44% </td>
   <td style="text-align:left;"> 50% / 0% </td>
  </tr>
</tbody>
</table>
</details>

<details>
<summary>disease status direction change confusion matrix by taxa</summary>
<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Taxa </th>
   <th style="text-align:left;"> Baseline Accuracy </th>
   <th style="text-align:left;"> REPEL (Overall/New Outbreaks) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> buffaloes </td>
   <td style="text-align:left;"> 54% </td>
   <td style="text-align:left;"> 98% / 96% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sheep/goats </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 97% / 47% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> equidae </td>
   <td style="text-align:left;"> 96% </td>
   <td style="text-align:left;"> 98% / 42% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> swine </td>
   <td style="text-align:left;"> 94% </td>
   <td style="text-align:left;"> 97% / 41% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cattle </td>
   <td style="text-align:left;"> 89% </td>
   <td style="text-align:left;"> 96% / 41% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> camelidae </td>
   <td style="text-align:left;"> 42% </td>
   <td style="text-align:left;"> 59% / 29% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> birds </td>
   <td style="text-align:left;"> 88% </td>
   <td style="text-align:left;"> 95% / 16% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hares/rabbits </td>
   <td style="text-align:left;"> 45% </td>
   <td style="text-align:left;"> 65% / 11% </td>
  </tr>
</tbody>
</table>
</details>

<details>
<summary>disease status direction change confusion matrix by continent</summary>
<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Continent </th>
   <th style="text-align:left;"> Baseline Accuracy </th>
   <th style="text-align:left;"> REPEL (Overall/New Outbreaks) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Americas </td>
   <td style="text-align:left;"> 88% </td>
   <td style="text-align:left;"> 97% / 45% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Asia </td>
   <td style="text-align:left;"> 92% </td>
   <td style="text-align:left;"> 96% / 43% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Africa </td>
   <td style="text-align:left;"> 92% </td>
   <td style="text-align:left;"> 96% / 38% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oceania </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 99% / 30% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Europe </td>
   <td style="text-align:left;"> 92% </td>
   <td style="text-align:left;"> 96% / 25% </td>
  </tr>
</tbody>
</table>
</details>

<details>
<summary>cases model stats</summary>

```
## # A tibble: 6 × 4
##   model    .metric .estimator     .estimate
##   <chr>    <chr>   <chr>              <dbl>
## 1 baseline rmse    standard   1260140.     
## 2 xgboost  rmse    standard   1254388.     
## 3 baseline rsq     standard         0.00620
## 4 xgboost  rsq     standard         0.225  
## 5 baseline mae     standard     34066.     
## 6 xgboost  mae     standard     34302.
```
</details>

<details>
<summary>cases residuals</summary>
![](nowcast_model_eval_files/figure-html/cd-resid-1.png)<!-- -->
</details>

<details>
<summary>cases residuals by disease</summary>
![](nowcast_model_eval_files/figure-html/cd-resid-disease-1.png)<!-- -->
</details>

<details>
<summary>cases residuals by taxa</summary>
![](nowcast_model_eval_files/figure-html/cd-resid-taxa-1.png)<!-- -->
</details>

<details>
<summary>cases residuals by continent</summary>
![](nowcast_model_eval_files/figure-html/cd-resid-continent-1.png)<!-- -->
</details>


### variable importance for specific cases of successfully predicting 0-1 switch
![](nowcast_model_eval_files/figure-html/unnamed-chunk-1-1.png)<!-- -->




