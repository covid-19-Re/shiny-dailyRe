<h4>Methods</h4>

The reported R<sub>e</sub> for a given day is the mean R<sub>e</sub> over the last 3 days. We report the estimated median with the 95% uncertainty interval. The method to estimate R<sub>e</sub> is described [here](https://ibz-shiny.ethz.ch/covid-19-re/methods.pdf). Since June, we first smooth the data with a LOESS step, and then estimate the infection dates using a [deconvolution](https://www.pnas.org/content/106/51/21825) step (discussed [here](https://www.medrxiv.org/content/10.1101/2020.05.12.20099366v1) and [here](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2)). Then, we estimate R<sub>e</sub>  using the [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html) software from the estimated infection dates.
Results based on data (for Switzerland) from June 26, 2020 using the original method can be accessed [here](https://smw.ch/article/doi/smw.2020.20271).


**Disclaimer**: The quality of the R<sub>e</sub> estimate for a particular country or region relies on the quality of raw data. We can only check the quality of data for some countries, in particular Switzerland and some surrounding countries.  Before drawing conclusions for a particular country, please check the data quality, even visually. In particular, sudden spikes in reporting on particular days or missing data on many days can bias Re estimates.

If you are working on the epidemic in a country for which we use data of bad quality, but have access to better data, please file an issue on https://github.com/covid-19-Re/shiny-dailyRe/issues, this would be extremely helpful!
