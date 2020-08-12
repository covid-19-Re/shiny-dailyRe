<h4>Einschätzung der momentanen epidemiologischen Lage</h4>

Die effektive Reproduktionszahl R<sub>e</sub>  war Anfang März zwischen 2-3, was Schätzungen für andere Länder entspricht. R<sub>e</sub>  ist Mitte März unter den kritischen Schwellenwert von 1 gefallen. Das R<sub>e</sub>  ist ab der zweiten Mai-Woche wieder angestiegen. Mitte Juni wurde ein Höchststand des R<sub>e</sub>  mit einem Wert von 1.4-1.9 erreicht. Im Juli wurde ein weiterer, jedoch tieferer, Höchststand erreicht. Diese Höchststände sind auf das Infektionsgeschehen in verschiedenen schweizer Regionen zurückzuführen. Momentan ist das R<sub>e</sub>   tiefer. Der geschätzte Median des R<sub>e</sub>  war jedoch im Juni und Juli meist über 1 und R<sub>e</sub>  war in dieser Zeit nie signifikant unter 1. Daher bleibt die Situation besorgniserregend.


R<sub>e</sub>  Werte über 1 im Juni und Juli können nicht durch den Anstieg der Anzahl durchgeführter Tests oder durch falsch positive Test-Resultate erklärt werden. Im Juni ist die Anzahl der wöchentlichen Tests in der Tat stark angestiegen. Wenn man statistisch für den Anstieg in der Anzahl Tests korrigiert liegt der Schätzwert von R<sub>e</sub>   von Mitte Juni bis Mitte Juli meist signifikant über 1. Dieses Resultat spiegelt den beobachteten Anstieg der Test-Positivität wieder. Die Spezifität der PCR-Tests ist sehr hoch, wodurch es zu praktisch keinen falsch positiven Resultaten kommt welche den Schätzwert beeinflussen könnten.



<h4>Methoden</h4>

Das publizierte R<sub>e</sub> für einen bestimmten Tag ist ein Mittelwert über die letzten 3 Tage. Wir publizieren den geschätzten Median zusammen mit dem 95% Unsicherheitsintervall. Die Methode zur Schätzung von R<sub>e</sub> ist [hier](https://ibz-shiny.ethz.ch/covid-19-re/methods.pdf) erklärt. Eine Erweiterung der Methode erlaubt uns die Infektionszeitpunkte anhand den Daten mithilfe einer sogenannten [Dekonvolution](https://www.pnas.org/content/106/51/21825) genauer zu bestimmen. Diese Herangehensweise wird für COVID-19 in zwei Manuskripten diskutiert [(1)](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2)[(2)](https://www.medrxiv.org/content/10.1101/2020.05.12.20099366v1). Die Dekonvolution erlaubt Abnahmen und Steigungen des R<sub>e</sub> schneller zu detektieren. Die Resultate basierend auf Daten vom 26.6.2020 mit der ursprünglichen Methode können [hier](https://raw.githubusercontent.com/covid-19-Re/covid19-additionalData/master/misc/2020-06-27_results_CH_convolution_method.png) angeschaut werden.

Die Schätzung von R<sub>e</sub> basiert von nun an ausschliesslich auf Daten vom BAG. Diese Änderung war erforderlich, da die bisher analysierten Daten von openZH in letzter Zeit mit abnehmender Häufigkeit aktualisiert wurden.

Das BAG kommuniziert täglich die Anzahl der eingegangenen Fälle der letzten 24 Stunden. Die Fälle stamen typischerweise von Fall-Bestätigungen der letzten drei Tage. Zur Schätzung von R<sub>e</sub>  analysieren wir nun die Anzahl der Fall-Bestätigungen pro Tag und ignorieren die letzten zwei Tage. Daher weichen Zahlen unseres Dashboards und die Zahlen der BAG-Berichte leicht voneinander ab.

Generell basiert unsere Einschätzung der momentanen epidemiologischen Lage auf einer Reihe von Methoden, siehe [Appendix A in Policy Brief «Effect of Measures»](https://ncs-tf.ch/de/policy-briefs/effect-of-measures-21-april-20-en/download). Die verschiedenen Schätzungen können im Detail voneinander abweichen, stimmen aber im Trend überein.
