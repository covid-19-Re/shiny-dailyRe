<h4>Assessment of current epidemiological situation</h4>

The effective reproductive number R<sub>e</sub>  in early March was 2-3, which is in the range of reproductive numbers reported for other places. The R<sub>e</sub>  fell below the critical value of 1 in mid March. R<sub>e</sub>  has increased continuously since the second week of May towards a peak in mid-June, with values around 1.4-1.9. Another smaller peak was observed in July. The peaks were driven by different geographic regions. Currently, R<sub>e</sub>  is lower. However, the median R<sub>e</sub>   was above 1 for most of the time in June and July and R<sub>e</sub>  was never significantly below 1 during that time. Thus, the situation remains concerning.


R<sub>e</sub>  values above 1 in June-July are not a consequence of an increased testing effort or of false positive test results. The number of weekly tests did indeed increase throughout June. Correcting for the increase in testing effort in the statistical analyses confirms that R<sub>e</sub>  was significantly above 1 for most of the time between mid-June and mid-July. This conclusion is in line with the observed increase in test positivity. The specificity of PCR tests is very high, leading to essentially no false positive results which could bias our estimates.


<h4>Methods</h4>

The reported R<sub>e</sub> for a given day is the mean R<sub>e</sub> over the last 3 days. We report the median with the 95% uncertainty interval. Details regarding the core method can be found [here](https://ibz-shiny.ethz.ch/covid-19-re/methods.pdf). We improved the method to infer infection dates from the observed data by using a [deconvolution](https://www.pnas.org/content/106/51/21825) step as discussed in two recent pre-prints [(1)](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2)[(2)](https://www.medrxiv.org/content/10.1101/2020.05.12.20099366v1). The deconvolution step allows to detect changes in R<sub>e</sub> more rapidly. Results based on data from June 26, 2020 using the original method can be accessed [here](https://raw.githubusercontent.com/covid-19-Re/covid19-additionalData/master/misc/2020-06-27_results_CH_convolution_method.png).

All estimates of R<sub>e</sub> are now based on data from the FOPH while we previously used data from openZH. The change was made because the openZH data have recently been updated with decreasing frequency.

Every day, the FOPH communicates the number of newly confirmed cases that they received over the last 24 hours. These cases typically stem from the last three days. To estimate R<sub>e</sub>  we therefore analyse the number of newly confirmed cases from each particular day ignoring the past two days. Thus the numbers in our dashboard are different from the numbers given in the daily FOPH situation reports.

Generally our assessment of the epidemiological situation is based on multiple methods, see [Appendix A in Policy Brief «Effect of Measures»](https://ncs-tf.ch/de/policy-briefs/effect-of-measures-21-april-20-en/download). The precise estimates are different, but the trends are consistent across methods.
