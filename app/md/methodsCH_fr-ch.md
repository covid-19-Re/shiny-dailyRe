<h4>Evaluation de la situation épidémique</h4>


Le taux (ou nombre) de reproduction R<sub>e</sub> représente le nombre moyen de personnes qu’infecte une personne infectée. Sur la figure ci-dessous, nous donnons la dernière estimation du taux de reproduction R<sub>e</sub> en Suisse. Ce graphe est mis-à-jour quotidiennement avec les dernières données de l’OFSP. La valeur de R<sub>e</sub> la plus récente calculée aujourd’hui reflète la dynamique épidémique d’il y a environ 10 jours, en raison du décalage entre infections et résultats des tests. Cette valeur la plus récente est marquée d’un astérisque sur le graphe.

Une synthèse de la situation épidémique actuelle basée sur le taux de reproduction R<sub>e</sub> et d’autres indicateurs est disponible [ici](https://sciencetaskforce.ch/fr/news-francais/). Des rapports plus complets sur la situation épidémique sont publiés par la Science Task Force sous la forme de [Policy Briefs](https://ncs-tf.ch/de/policy-briefs).

Plus d’informations et des outils additionnels pour l’évaluation de la situation épidémique basée sur le R<sub>e</sub> sont disponibles ce dashboard. En particulier, des estimations sont disponibles par canton, par grandes régions suisses ainsi que pour d’autre pays.
Ces estimations et les taux d’incidence par pays sont aussi affichées sur une carte du monde.
De plus, on peut visualiser les changements de R<sub>e</sub> suivant l’introduction ou le relâchement de mesures de santé publique. Enfin, le nombre d’infections estimé en fonction du nombre de cas observés est lui aussi disponible. C’est ce nombre d’infections, au cours du temps. qui nous permet d’estimer R<sub>e</sub>. Toutes les estimations de R<sub>e</sub> sont disponibles librement au format CSV.


<h4>Résumé de la trajectoire de l’épidémie jusqu’à présent, basé sur R<sub>e</sub></h4>

Depuis le début de l’épidémie en Suisse, nous avons observé les tendances suivantes pour le taux de repoduction R<sub>e</sub>. Il était d’environ 2-3 à la début mars, ce qui correspond à la fourchette de valeurs estimée pour d’autres pays. Puis le R<sub>e</sub> est passé sous le seuil critique de 1 à la mi-mars. Il a ensuite augmenté continûment à partie de la deuxième semaine de mai, jusqu’à atteindre un pic à la mi-juin, avec des valeurs maximales entre 1,4 et 1,9. Un autre pic, moins haut, a été observé en juillet. Ces pics ont été causés par des résurgences épidémiques dans des régions différentes de Suisse. Pendant l’été, le R<sub>e</sub> s’est stabilisé autour de 1,1. Ce qui veut dire que le nombre de cas observés doublait toutes les 4 semaines environ.

Après la mi-septembre, le taux de repoduction a crû brutalement avec un pic au-delà de 1,5 à la fin du mois. Cela correspond a un doublement du nombre de cas chaque semaine. Cette propagation rapide du virus a été observée dans tous les cantons. Depuis octobre, le taux de reproduction a diminué, c’est ce qu’indiquent tous les indicateurs.


<h4>Méthodes</h4>

La valeur de R<sub>e</sub> donnée pour un jour J est une moyenne des estimations réalisées sur les jours J-2, J-1 et J. Nous donnons la médiane des estimations ainsi que les intervalles d'incertitude à 95%. La méthode d'estimation de R<sub>e</sub> est expliquée en détails [ici](https://ibz-shiny.ethz.ch/covid-19-re/methods.pdf) (en anglais).). Depuis juin, nous lissons d’abord les données brutes par une étape reposant sur la méthode dite LOESS, nous inférons ensuite les dates d’infections des malades grâce à une étape de [déconvolution](https://www.pnas.org/content/106/51/21825) comme présentée dans deux pre-prints récents [(1)](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2)[(2)](https://www.medrxiv.org/content/10.1101/2020.05.12.20099366v1) (en anglais).  Nous estimons ensuite le nombre de reproduction effectif R<sub>e</sub> grâce au software [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html) auquel nous fournissons les dates estimées d’infection.

Les résultats utilisant les données disponibles du 26 juin 2020 et reposant sur la méthode précédente sont accessibles [ici](https://smw.ch/article/doi/smw.2020.20271).

Depuis juin 2020, toutes les estimations de R<sub>e</sub> sont faites à partir de données de l’OFSP. Pendant les jours de semaine, l’OFSP communique le nombre de nouveaux cas confirmés reçus durants les dernières 24 heures. Ces cas proviennent en général des trois derniers jours. Pour estimer R<sub>e</sub>, nous analysons le nombre de cas confirmés pour chaque jour en ignorant les deux derniers jours (non consolidés). En conséquence, le nombre de cas affichés sur le dashboard peuvent être différents des chiffres communiqués dans les rapports journaliers de l’OFSP.


<h4>Mises en garde</h4>

En général, l’estimation de R<sub>e</sub> faite sur les 5 derniers jours est susceptible d’être consolidée les jours suivants. Nous fondons donc nos interprétations sur les tendances observées sur plusieurs jours.



Cependant, en raison du fort taux de positivité et du retard accumulé dans les remontées des cas admis à l’hôpital pendant la deuxième vague, les estimations basées sur ces données sont susceptibles d’être en dessous de la réalité durant cette période. Le R<sub>e</sub> basé sur la positivité des tests (cas confirmés / nombre de tests) est fiable si les personnes testées sont tirées aléatoirement dans la population; puisque les personnes présentant des symptômes sont testées en priorité, cette valeur de R<sub>e</sub> peut aussi être biasée. Durant la deuxième vague, la valeur de R<sub>e</sub> estimée à partir du nombre de morts est la plus fiable. Cependant, cette valeur de R<sub>e</sub> a un retard d’environ 3 semaines par rapport au présent. Il s’écoule en moyenne environ 17 jours entre l’infection et le décès lorsque celui-ci survient.


Les valeurs de R<sub>e</sub> au-dessus de 1 observées depuis l’été ne sont pas une conséquence de l’intensification de la campagne de tests, ou de tests faux-positifs.
Le nombre de tests hebdomadaires a en effet augmenté significativement depuis l’été. Simultanément, le percentage de tests positifs (le taux de positivité) a augmenté de 0,4% en juin à plus de 20% en octobre. Cela signifie aussi que nous estimons que le R<sub>e</sub> était significativement au-dessus de 1 pendant la majeure partie de l’été. Par ailleurs, la spécificité des tests PCR étant très haute, le taux de faux-positifs est très faible, ce qui enlève une source de biais potentiel.
