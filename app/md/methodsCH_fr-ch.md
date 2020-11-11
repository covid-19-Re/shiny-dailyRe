R<sub>e</sub><h4>Evaluation de la situation épidémique</h4>


Depuis la mi-septembre, R<sub>e</sub> a augmenté très rapidement jusqu’à un pic à 1.5 à la fin du mois. Cela correspondait à un doublement des cas chaque semaine environ. Le R<sub>e</sub> estimé à partir du nombre d’hospitalisations a suivé la même tendance et est passé au-dessus de 1 de manière significative pour la première fois depuis mars. Cette transmission rapide du virus a été observée dans tous les cantons suisses. Actuellement, le R<sub>e</sub> diminue et est autour de 1, en se basant sur le nombre de cas confirmés et d’admissions à l’hôpital.

*Cependant, en raison du fort taux de positivité des tests et du retard dans les remontées des hospitalisations, ces estimations de R<sub>e</sub> pourraient être trop faibles.*

Le R<sub>e</sub> calculé en prenant en compte le taux de positivité des tests (nombre de cas confirmés / nombre de tests) est une indicateur fiable si les personnes testées étaients prises au hasard dans la population. Puisque l’on teste spécifiquement les personnes symptomatiques, cette valeur de R<sub>e</sub> pourrait aussi être biaisée. Actuellement, l’estimation de R<sub>e</sub> fondée sur le nombre de morts est la plus fiable. Cependant, cette estimation de R<sub>e</sub> reflète la situation épidémique avec un retard d’environ 3 semaines. Car il s’écoule en moyenne environ 17 jours entre l’infection et le décès, lorsque celui-ci survient.
Un rapport hebdomadaire détaillé sur la situation épidémique est publié par la Science Task Force sous la forme de [Policy Briefs](https://ncs-tf.ch/de/policy-briefs).



<h4>Méthodes</h4>

La valeur de R<sub>e</sub> donnée pour un jour J est une moyenne des estimations réalisées sur les jours J-2, J-1 et J. Nous donnons la médiane des estimations ainsi que les intervalles d'incertitude à 95%. La méthode d'estimation de R<sub>e</sub> est expliquée en détails [ici](https://ibz-shiny.ethz.ch/covid-19-re/methods.pdf) (en anglais).). Depuis juin, nous lissons d’abord les données brutes par une étape reposant sur la méthode dite LOESS, nous inférons ensuite les dates d’infections des malades grâce à une étape de [déconvolution](https://www.pnas.org/content/106/51/21825) comme présentée dans deux pre-prints récents [(1)](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2)[(2)](https://www.medrxiv.org/content/10.1101/2020.05.12.20099366v1) (en anglais).  Nous estimons ensuite le nombre de reproduction effectif R<sub>e</sub> grâce au software [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html) auquel nous fournissons les dates estimées d’infection.

Les résultats utilisant les données disponibles du 26 juin 2020 et reposant sur la méthode précédente sont accessibles [ici](https://smw.ch/article/doi/smw.2020.20271).

Depuis juin 2020, toutes les estimations de R<sub>e</sub> reposent désormais seulement sur les données fournies par l’OFSP. Chaque jour, l’OFSP communique le nombre de nouveau cas signalés dans les dernières 24 heures. Ces cas ont généralement été testés positifs au cours des 3 derniers jours. Pour estimer R<sub>e</sub>, nous utilisons l’incidence journalière, le nombre de cas confirmés pour un jour donné et non le nombre de nouveaux cas signalés. Nous excluons les données des deux derniers jours car elle ne sont pas encore consolidées. Donc les données affichées sur nos graphes diffèrent des chiffres communiqués par l’OFSP dans sa communication quotidienne, elles ne représentent pas les mêmes quantités.


Notre evaluation de la situation épidémique se fondent sur plusieurs méthodes, voir [l’appendice A du Policy Brief sur l’effet des mesures prises (en anglais)](https://ncs-tf.ch/de/policy-briefs/effect-of-measures-21-april-20-en/download). Les valeurs estimées diffèrent selon la méthode employée, mais les tendances détectées sont identiques.


<h4>Mises en garde</h4>

Il faut noter que les estimations les plus récentes de la valeur de R<sub>e</sub> peuvent fluctuer légèrement avant d’être consolidées. Nous appuyons donc notre analyse sur les récentes tendances observées sur plusieurs jours.
Le fait que R<sub>e</sub> soit au-dessus de 1 n’est pas une conséquence de l’intensification des campagnes de tests ou de faux-positifs résultant de ces tests. En effet, bien que le nombre de tests hebdomadaires ait augmenté durant l’été, la fraction positive des tests (ou taux de tests positifs) a elle aussi augmenté de 0.4% en Juin à environ 15% en ce moment. Une correction prenant en compte l’augmentation du nombre de tests cette augmentation confirme que R<sub>e</sub> était significativement au-dessus de 1 pendant la majeure partie de l’été. Par ailleurs, la spécificité des tests PCR étant très élevée, le nombre de résultats faux positifs pouvant biaiser les estimations est très faible.
