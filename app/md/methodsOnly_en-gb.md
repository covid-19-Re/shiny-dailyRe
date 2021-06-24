<h4>Methods</h4>

The reported R<sub>e</sub> for a given day is the mean R<sub>e</sub> over the last 3 days. We report the estimated median with the 95% uncertainty interval. The method to estimate R<sub>e</sub> is described [here](https://www.medrxiv.org/content/10.1101/2020.11.26.20239368v2.full-text). 
We further provide a 7-day estimate. This estimate is obtained by assuming a constant R<sub>e</sub> over the last seven days. The results of such a piecewise constant procedure can be viewed visually using the option "Step-wise constant".

Since June, we first smooth the data with a LOESS step, and then estimate the infection dates using a [deconvolution](https://www.pnas.org/content/106/51/21825) step (discussed [here](https://smw.ch/article/doi/smw.2020.20307) and [here](https://journals.plos.org/ploscompbiol/article/comments?id=10.1371/journal.pcbi.1008409)). Then, we estimate R<sub>e</sub>  using the [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html) software from the estimated infection dates. Results based on data from June 26, 2020 using the original method can be accessed [here](https://smw.ch/article/doi/smw.2020.20271).

**UPDATE (25.1.2021)**: We now construct 95% confidence intervals based on a block bootstrapping procedure. We found in simulations that this new procedure better reflects the uncertainty in the parameter estimates compared to our previous procedure. We are currently writing up an explanation of the new procedure and will post it as soon as it is available. In the meantime, the method can be accessed through our [code](https://github.com/covid-19-Re/shiny-dailyRe). Our general approach (up to the bootstrapping procedure) is explained [here](https://www.medrxiv.org/content/10.1101/2020.11.26.20239368v2.full-text).

**Update (15.2.2021)**: We now take the R<sub>e</sub> estimate on the original data as the R<sub>e</sub> point estimate, replacing the mean of the bootstrap replicates. The aim is to make the point estimate more stable. 
Further, we use the uncertainty intervals from EpiEstim in case it is larger than the bootstrapping interval in order to obtain conservative uncertainty intervals.
Updated code is available [here](https://github.com/covid-19-Re/shiny-dailyRe).

**Disclaimer**: The quality of the R<sub>e</sub> estimate for a particular country or region relies on the quality of raw data. We can only check the quality of data for some countries, in particular Switzerland and some surrounding countries.  Before drawing conclusions for a particular country, please check the data quality, even visually. In particular, sudden spikes in reporting on particular days or missing data on many days can bias R<sub>e</sub> estimates.

If you are working on the epidemic in a country for which we use data of bad quality, but have access to better data, please file an issue on https://github.com/covid-19-Re/shiny-dailyRe/issues, this would be extremely helpful!
