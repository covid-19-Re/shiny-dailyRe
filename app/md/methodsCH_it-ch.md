<h4>Assessment of current epidemiological situation</h4>

From mid-September onwards, the R<sub>e</sub> rose very sharply with a peak of over 1.5 at the end of the month. This corresponded to a doubling time of the case numbers of about one week. The R<sub>e</sub> based on hospitalizations followed the same trend and rose significantly above 1 for the first time since March. The rapid spread was observed in all Swiss cantons. Currently, the R<sub>e</sub> is decreasing and is around 1 based on the number of cases and hospitalizations.

*However, due to the high test positivity rate and delayed hospital reports, estimates based on the case numbers and hospitalizations may be too low.*

The R<sub>e</sub> based on positivity (confirmed cases/tests) is reliable if the persons tested are a random cross section of the population; since we test specifically symptomatic persons, this R<sub>e</sub> value may also be biased. Currently, the R<sub>e</sub> value based on deaths is the most reliable. However, this R<sub>e</sub> value reflects the epidemiological situation with a time lag of about 3 weeks. The reason is that it takes about 17 days between new infection and death.
A detailed weekly update of the epidemiological situation is published by the Science Task Force in the form of [Policy Briefs](https://ncs-tf.ch/de/policy-briefs).


<h4>Methods</h4>

The reported R<sub>e</sub> for a given day is the mean R<sub>e</sub> over the last 3 days. We report the estimated median with the 95% uncertainty interval. The method to estimate R<sub>e</sub> is described [here](https://ibz-shiny.ethz.ch/covid-19-re/methods.pdf). Since June, we first smooth the data with a LOESS step, and then estimate the infection dates using a [deconvolution](https://www.pnas.org/content/106/51/21825) step (discussed [here](https://www.medrxiv.org/content/10.1101/2020.05.12.20099366v1) and [here](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2)). Then, we estimate R<sub>e</sub>  using the [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html) software from the estimated infection dates. Results based on data from June 26, 2020 using the original method can be accessed [here](https://smw.ch/article/doi/smw.2020.20271).

Since June 2020, all estimates of R<sub>e</sub> are based on data from the FOPH. Every day, the FOPH communicates the number of newly confirmed cases that they received over the last 24 hours. These cases typically stem from the last three days. To estimate R<sub>e</sub> we therefore analyse the number of newly confirmed cases from each particular day ignoring the past two days. As a result, the numbers in our dashboard are different from the numbers given in the daily FOPH situation reports.

Generally our assessment of the epidemiological situation is based on multiple methods, see [Appendix A in Policy Brief «Effect of Measures»](https://ncs-tf.ch/de/policy-briefs/effect-of-measures-21-april-20-en/download). The precise estimates are different, but the trends are consistent across methods.


<h4>Caveats</h4>

In general, we note that the R<sub>e</sub> estimates for the last five days may still be consolidated during subsequent days. We thus base our interpretation on estimated trends over several days.

R<sub>e</sub> values above 1 are not a consequence of an increased testing effort or of false positive test results. The number of weekly tests has indeed increased substantially over the summer. Simultaneously however, the percentage of positive tests among all tests (i.e. the test positivity rate) has increased from 0.4% in June to around 15% at the moment. When correcting for the increase in testing effort in the statistical analyses, we still estimate R<sub>e</sub> significantly above 1 for most of the summer. Additionally, the specificity of PCR tests is very high, leading to essentially no false positive results which could bias our estimates.
