<h4>Einschätzung der momentanen epidemiologischen Lage</h4>

Ab Mitte September ist das R<sub>e</sub> sehr stark angestiegen mit einem Höchstand von über 1.5 zum Monatswechsel. Dies entsprach einer Verdopplungszeit der Fallzahlen von etwa einer Woche. Das R<sub>e</sub> basierend auf Hospitalisierungen folgte dem gleichen Trend und ist zum ersten Mal seit März signifikant über 1 gestiegen. Die schnelle Ausbreitung wurde in allen Kantonen der Schweiz beobachtet. Momentan geht das R<sub>e</sub>  zurück und ist basierend auf den Fallzahlen und Hospitalisierungen um 1.

*Wegen der hohen Positivitätsrate und verspäteten Spitalmeldungen sind die Schätzungen basierend auf Fallzahlen und Hospitalisierungen jedoch möglicherweise zu tief.*

Das R<sub>e</sub> basierend auf der Positivität (bestätigte Fälle/Tests) ist zuverlässig wenn die getesteten Personen ein zufälliger Querschnitt der Bevölkerung sind; nachdem wir insb. symptomatische Personen testen ist auch dieser R<sub>e</sub> Wert möglicherweise verzerrt. Momentan ist der R<sub>e</sub> Wert basierend auf den Todesfällen am zuverlässigsten. Dieser bildet jedoch die epidemiologische Lage sehr zeitverzögert, nämlich von vor ca. 3 Wochen, ab. Grund ist dass in etwa 17 Tage zwischen Neuansteckung und Todesfall vergehen.
Einen wöchentliches detailliertes Update der epidemiologischen Lage wird von der Science Task Force in Form von [Policy Briefs](https://ncs-tf.ch/de/policy-briefs) veröffentlicht.



<h4>Methoden</h4>

Das publizierte R<sub>e</sub> für einen bestimmten Tag ist ein Mittelwert über die letzten 3 Tage. Wir publizieren den geschätzten Median zusammen mit dem 95% Unsicherheitsintervall. Die Methode zur Schätzung von R<sub>e</sub> ist [hier](https://ibz-shiny.ethz.ch/covid-19-re/methods.pdf) erklärt. Seit Juni glätten wir zuerst die Daten mit dem sogenannten Loess-Verfahren und berechnen dann die Infektionszeitpunkte mithilfe einer sogenannten [Dekonvolution](https://www.pnas.org/content/106/51/21825) ([link](https://www.medrxiv.org/content/10.1101/2020.05.12.20099366v1) und [link](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2)). Dann wird basierend auf den Infektionszeitpunkten das R<sub>e</sub> mit dem [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html) Software Paket berechnet. Die Resultate basierend auf Daten vom 26.6.2020 mit der ursprünglichen Methode können [hier](https://smw.ch/article/doi/smw.2020.20271) angeschaut werden.

Die Schätzung von R<sub>e</sub> basiert seit Juni 2020 ausschliesslich auf Daten vom BAG. Das BAG kommuniziert täglich die Anzahl der eingegangenen Fälle der letzten 24 Stunden. Die Fälle stammen typischerweise von Fall-Bestätigungen der letzten drei Tage. Zur Schätzung von R<sub>e</sub> analysieren wir nun die Anzahl der Fall-Bestätigungen pro Tag und ignorieren die letzten zwei Tage. Daher weichen Zahlen unseres Dashboards und die Zahlen der BAG-Berichte leicht voneinander ab.

Generell basiert unsere Einschätzung der momentanen epidemiologischen Lage auf einer Reihe von Methoden, siehe [Appendix A in Policy Brief «Effect of Measures»](https://ncs-tf.ch/de/policy-briefs/effect-of-measures-21-april-20-en/download). Die verschiedenen Schätzungen können im Detail voneinander abweichen, stimmen aber im Trend überein.


<h4>Caveats</h4>

Wir weisen generell darauf hin, dass die Schätzungen der letzten ca. fünf Tage leichten Schwankungen unterliegen können. Wir basieren unsere Einschätzung daher auf geschätzten Trends über mehrere Tage.
R<sub>e</sub> Werte über 1 können nicht durch den Anstieg der Anzahl durchgeführter Tests oder durch falsch positive Test-Resultate erklärt werden: Die Anzahl der wöchentlichen Tests ist über den Sommer in der Tat stark angestiegen. Gleichzeitig ist jedoch der Prozentsatz der positiven Tests im Vergleich zu allen durchgeführten Tests (d.h. die Positivitätsrate) von ca. 0.4 % im Juni auf momentan rund 15% gestiegen. Dies bedeutet auch, dass wir das R<sub>e</sub> basierend auf der Positivitätsrate über den Sommer hinweg auf signifikant über 1 schätzen. Weiter ist die Spezifität der PCR-Tests sehr hoch, wodurch es zu praktisch keinen falsch positiven Resultaten kommt welche den Schätzwert beeinflussen könnten.
