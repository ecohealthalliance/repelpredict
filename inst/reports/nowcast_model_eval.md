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
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> accuracy </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> proportion of the data that are predicted correctly </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.85 </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> kap </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> similar measure to accuracy(), but is normalized by the accuracy that would be expected by chance alone and is very useful when one or more classes have large frequency distributions. </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.45 </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.88 </td>
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
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> accuracy </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> proportion of the data that are predicted correctly </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.850 </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.960 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> kap </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="2"> similar measure to accuracy(), but is normalized by the accuracy that would be expected by chance alone and is very useful when one or more classes have large frequency distributions. </td>
   <td style="text-align:left;"> baseline </td>
   <td style="text-align:right;"> 0.051 </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:right;"> 0.500 </td>
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
   <td style="text-align:left;"> newcastle disease </td>
   <td style="text-align:left;"> 83% </td>
   <td style="text-align:left;"> 92% / 51% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vesicular stomatitis </td>
   <td style="text-align:left;"> 90% </td>
   <td style="text-align:left;"> 98% / 50% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> peste des petits ruminants </td>
   <td style="text-align:left;"> 91% </td>
   <td style="text-align:left;"> 98% / 48% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> foot and mouth disease </td>
   <td style="text-align:left;"> 77% </td>
   <td style="text-align:left;"> 92% / 46% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lumpy skin disease </td>
   <td style="text-align:left;"> 95% </td>
   <td style="text-align:left;"> 97% / 44% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ovine bluetongue disease </td>
   <td style="text-align:left;"> 80% </td>
   <td style="text-align:left;"> 92% / 40% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> african swine fever </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 97% / 38% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> african horse sickness </td>
   <td style="text-align:left;"> 99% </td>
   <td style="text-align:left;"> 100% / 33% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pleuropneumonia </td>
   <td style="text-align:left;"> 95% </td>
   <td style="text-align:left;"> 98% / 21% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ovine pox disease </td>
   <td style="text-align:left;"> 94% </td>
   <td style="text-align:left;"> 96% / 18% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> highly pathogenic avian influenza </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 93% / 13% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rift valley fever </td>
   <td style="text-align:left;"> 96% </td>
   <td style="text-align:left;"> 97% / 4% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> classical swine fever </td>
   <td style="text-align:left;"> 95% </td>
   <td style="text-align:left;"> 96% / 4% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> swine vesicular disease </td>
   <td style="text-align:left;"> 99% </td>
   <td style="text-align:left;"> 99% / 0% </td>
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
   <td style="text-align:left;"> cats </td>
   <td style="text-align:left;"> 75% </td>
   <td style="text-align:left;"> 95% / 74% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cervidae </td>
   <td style="text-align:left;"> 73% </td>
   <td style="text-align:left;"> 96% / 64% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> buffaloes </td>
   <td style="text-align:left;"> 77% </td>
   <td style="text-align:left;"> 96% / 59% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dogs </td>
   <td style="text-align:left;"> 77% </td>
   <td style="text-align:left;"> 94% / 57% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> swine </td>
   <td style="text-align:left;"> 87% </td>
   <td style="text-align:left;"> 96% / 53% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> camelidae </td>
   <td style="text-align:left;"> 78% </td>
   <td style="text-align:left;"> 95% / 53% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sheep/goats </td>
   <td style="text-align:left;"> 86% </td>
   <td style="text-align:left;"> 96% / 50% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> equidae </td>
   <td style="text-align:left;"> 91% </td>
   <td style="text-align:left;"> 97% / 42% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cattle </td>
   <td style="text-align:left;"> 86% </td>
   <td style="text-align:left;"> 95% / 42% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hares/rabbits </td>
   <td style="text-align:left;"> 86% </td>
   <td style="text-align:left;"> 95% / 38% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> birds </td>
   <td style="text-align:left;"> 85% </td>
   <td style="text-align:left;"> 94% / 28% </td>
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
   <td style="text-align:left;"> 83% </td>
   <td style="text-align:left;"> 96% / 57% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Africa </td>
   <td style="text-align:left;"> 84% </td>
   <td style="text-align:left;"> 95% / 48% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Asia </td>
   <td style="text-align:left;"> 86% </td>
   <td style="text-align:left;"> 96% / 46% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oceania </td>
   <td style="text-align:left;"> 93% </td>
   <td style="text-align:left;"> 99% / 42% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Europe </td>
   <td style="text-align:left;"> 87% </td>
   <td style="text-align:left;"> 95% / 38% </td>
  </tr>
</tbody>
</table>
</details>

<details>
<summary>cases model stats</summary>

```
## # A tibble: 6 x 4
##   model    .metric .estimator  .estimate
##   <chr>    <chr>   <chr>           <dbl>
## 1 baseline rmse    standard   233494.   
## 2 xgboost  rmse    standard   277316.   
## 3 baseline rsq     standard        0.289
## 4 xgboost  rsq     standard        0.183
## 5 baseline mae     standard     2856.   
## 6 xgboost  mae     standard     3712.
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



