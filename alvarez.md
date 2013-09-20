Etude Alvarez
========================================================

La question de recherche est dans Read.me

Les données ne sont pas exploitables car pour un sujet donné ayant bénéficié d'une pédgogie active (ou passive) on ne sait pas quel est son comportement face à une question avant et après la formation. pour chaque personne il faut pouvoir répondre à la question: a la question Qx le sujet i a pépondu juste avant la formation (oui ou non) et après la formation (oui ou non). L'ensemble des réponses permet de compléter un tableau 2x2 qui sera soumis au test de Mac Nemar (variante du khi2 pour test dichotomique et séries appariées).

Pour exploiter à minima le questionnaire, on étudie le groupe uniquement sous l'angle du résultat final selon la méthode pédagogique. Y a t'il un groupe où le nombre de bonnes réponses est nettement plus élevé au questionnaire final. La réponse est positive uniquement pour la question 1. Pourla question 4, il existe une différence entre es groupes avant la formation (cf infra). Pour les autres questions, les deux groupes progressent sans que l'on puisse dire que l'un est supérieur à l'autre.

NB: au départ il ne semble pas y avoir de différences entre les 2 groupes pour ce qui est de l'age, du sexe, de l'expérience professionnelle ou des connaissance.
La réponse "ne se prononce pas" est embêtante:
- elle fausse le caractère dichotomique de la réponse
- elle  favorise les non réponses
- faut-il l'exclure de l'analyse ou la compter avec les réponses fausses ?

Conclusion:
cette étude ne permet pas de conclure à la supériorité d'une méthode sur une autre mais il y a des biais méthodologiques iimportants qui obèrent l'interprétation. On peut aussi s'interroger sur la pertinence des questions posées pour discriminer les deux approches pédagogiques.
- i initial (avant formation)
- f final (après la formation)



Analyse du fichier V3
=====================
1. le fichier est toujours sous forme de classeur excel => pas exploitable en l'état car les données sont dispersées sur 3 feuilles
2. la sauvegarde du fichier excel entraine un plantage => abandonner excel pour ce type de travail et utiliser libre office (gratuit!)
3. la deuxième ligne ne sert à rien => supprimer
4. nom des colonnes: ne pas utiliser de caractères accentués qui sont mal traduits par windows
5. PEC RCP: transformer toutes les données ayant plusieurs attributs dispersés sur plusieurs colonne avec des 0 et 1 en une variable sur 1 seule colonne avec le nom de l'attribut. Par exemple faire une seule colonne Pedagogie avec active et passive à la place des 0 et 1. Pareil pour les autres colonnes.
6. ce qu'il ne faut pas faire...
- les données sont dans un classeur de 3 feuilles ce qui revient à traiter 3 fichiers => impossible en l'état de comparer les données, il faut fusionner les fichiers
- le sexe est sur 2 colonnes => faire une seule colonne H et F


```r
library("epibasix")

file <- "QCM_2013_Qinitial.csv"
f1 <- read.csv(file, header = TRUE, sep = ",", skip = 2)
file <- "QCM_2013_Qfinal.csv"
f2 <- read.csv(file, header = TRUE, sep = ",", skip = 2)
names(f1)
```

```
##  [1] "N"          "Session"    "SEXE"       "Naissance"  "Experience"
##  [6] "Domaine"    "Pedagogie"  "Q1"         "Q2"         "Q3"        
## [11] "Q4"         "Q5"         "Q6"
```

```r
names(f2)
```

```
##  [1] "N"          "Session"    "SEXE"       "Naissance"  "Experience"
##  [6] "Domaine"    "Pedagogie"  "Q1"         "Q2"         "Q3"        
## [11] "Q4"         "Q5"         "Q6"
```



```r
summary(f1$SEXE)
```

```
## Femme Homme 
##    30    72
```

```r
summary(f1$Experience)
```

```
##  SXP XPM3 XPP3 
##   78   18    6
```

```r
summary(f1$Pedagogie)
```

```
##  Active Passive 
##      50      52
```

```r
t <- table(f1$SEXE, f1$Experience)
t
```

```
##        
##         SXP XPM3 XPP3
##   Femme  21    7    2
##   Homme  57   11    4
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  t 
## X-squared = 1.056, df = 2, p-value = 0.5898
```

```r
t <- table(f1$SEXE, f1$Pedagogie)
t
```

```
##        
##         Active Passive
##   Femme     17      13
##   Homme     33      39
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.6082, df = 1, p-value = 0.4355
```

```r
t <- table(f1$Experience, f1$Pedagogie)
t
```

```
##       
##        Active Passive
##   SXP      34      44
##   XPM3     12       6
##   XPP3      4       2
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  t 
## X-squared = 3.911, df = 2, p-value = 0.1415
```

Pas de différence entre les deux groupes en ce qui concerne la répartition des sexes et l'expérience.

Analyse des QCM
---------------------
On ne peut pas analyser la progression des individus selon la méthode peédagoqique utilisée, car il aurait fallu pouvoir suivre individuellement chaque sujet pour mesurer sa performance avant-après (le test approprié dans ce cas aurait été le test de Mac Nemar). En l'absence d'un tel suivi il n'est pas possible de dire si une différence observée est le fruit du hasard ou d'un réel effet de la méthode.

On peut simpement mesurer si pour une question, les réponses diffèrent entre les groupes

On observe une différence pour les questions 1 et 4:
- pour la q1 le groupe pédagogie active a de eilleurs résultats que le groupe passif
- pour q4 c'est plus délicat car il existe une différence entre les 2 groupes avant la formation. Le groupe actif est "meilleur" sur cette question et cette différence persiste après la fomation

#### Question1

```r
q1i <- table(f1$Q1, f1$Pedagogie)
q1f <- table(f2$Q1, f2$Pedagogie)
q1i
```

```
##        
##         Active Passive
##   FAUX      31      30
##   JUSTE     13      17
##   NSP        6       5
```

```r
q1f
```

```
##        
##         Active Passive
##   FAUX      10      22
##   juste     37      30
```

```r
chisq.test(q1i)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q1i 
## X-squared = 0.6017, df = 2, p-value = 0.7402
```

```r
chisq.test(q1f)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  q1f 
## X-squared = 4.076, df = 1, p-value = 0.04349
```

```r
prop.table(q1i, 2)
```

```
##        
##          Active Passive
##   FAUX  0.62000 0.57692
##   JUSTE 0.26000 0.32692
##   NSP   0.12000 0.09615
```

```r
prop.table(q1f, 2)
```

```
##        
##         Active Passive
##   FAUX  0.2128  0.4231
##   juste 0.7872  0.5769
```

#### Question2

```r
q2i <- table(f1$Q2, f1$Pedagogie)
q2f <- table(f2$Q2, f2$Pedagogie)
q2i
```

```
##        
##         Active Passive
##   FAUX      26      20
##   JUSTE     10       9
##   NSP       14      23
```

```r
q2f
```

```
##        
##         Active Passive
##   FAUX      20      32
##   juste     26      19
##   NSP        1       1
```

```r
chisq.test(q2i)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q2i 
## X-squared = 2.986, df = 2, p-value = 0.2247
```

```r
chisq.test(q2f)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q2f 
## X-squared = 3.615, df = 2, p-value = 0.1641
```

```r
prop.table(q2i, 2)
```

```
##        
##         Active Passive
##   FAUX  0.5200  0.3846
##   JUSTE 0.2000  0.1731
##   NSP   0.2800  0.4423
```

```r
prop.table(q2f, 2)
```

```
##        
##          Active Passive
##   FAUX  0.42553 0.61538
##   juste 0.55319 0.36538
##   NSP   0.02128 0.01923
```


#### Question3

```r
q3i <- table(f1$Q3, f1$Pedagogie)
q3f <- table(f2$Q3, f2$Pedagogie)
q3i
```

```
##        
##         Active Passive
##   FAUX      29      34
##   JUSTE     17       9
##   NSP        4       9
```

```r
q3f
```

```
##        
##         Active Passive
##   FAUX      13      21
##   juste     33      31
##   NSP        1       0
```

```r
chisq.test(q3i)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q3i 
## X-squared = 4.744, df = 2, p-value = 0.09329
```

```r
chisq.test(q3f)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q3f 
## X-squared = 2.699, df = 2, p-value = 0.2593
```

```r
prop.table(q3i, 2)
```

```
##        
##         Active Passive
##   FAUX  0.5800  0.6538
##   JUSTE 0.3400  0.1731
##   NSP   0.0800  0.1731
```

```r
prop.table(q3f, 2)
```

```
##        
##          Active Passive
##   FAUX  0.27660 0.40385
##   juste 0.70213 0.59615
##   NSP   0.02128 0.00000
```


#### Question4

```r
q4i <- table(f1$Q4, f1$Pedagogie)
q4f <- table(f2$Q4, f2$Pedagogie)
q4i
```

```
##        
##         Active Passive
##   FAUX      34      34
##   JUSTE     15      10
##   NSP        1       8
```

```r
q4f
```

```
##        
##         Active Passive
##   FAUX       0       1
##   FAUX      14      30
##   juste     32      21
##   NSP        1       0
```

```r
chisq.test(q4i)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q4i 
## X-squared = 6.408, df = 2, p-value = 0.04061
```

```r
chisq.test(q4f)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q4f 
## X-squared = 9.874, df = 3, p-value = 0.01967
```

```r
prop.table(q4i, 2)
```

```
##        
##         Active Passive
##   FAUX  0.6800  0.6538
##   JUSTE 0.3000  0.1923
##   NSP   0.0200  0.1538
```

```r
prop.table(q4f, 2)
```

```
##        
##          Active Passive
##   FAUX  0.00000 0.01923
##   FAUX  0.29787 0.57692
##   juste 0.68085 0.40385
##   NSP   0.02128 0.00000
```


#### Question5

```r
q5i <- table(f1$Q5, f1$Pedagogie)
q5f <- table(f2$Q5, f2$Pedagogie)
q5i
```

```
##        
##         Active Passive
##   FAUX      28      25
##   JUSTE     14      14
##   NSP        8      13
```

```r
q5f
```

```
##        
##         Active Passive
##   FAUX       0       1
##   FAUX      18      23
##   JUSTE     28      28
##   NSP        1       0
```

```r
chisq.test(q5i)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q5i 
## X-squared = 1.322, df = 2, p-value = 0.5164
```

```r
chisq.test(q5f)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q5f 
## X-squared = 2.363, df = 3, p-value = 0.5005
```

```r
prop.table(q5i, 2)
```

```
##        
##         Active Passive
##   FAUX  0.5600  0.4808
##   JUSTE 0.2800  0.2692
##   NSP   0.1600  0.2500
```

```r
prop.table(q5f, 2)
```

```
##        
##          Active Passive
##   FAUX  0.00000 0.01923
##   FAUX  0.38298 0.44231
##   JUSTE 0.59574 0.53846
##   NSP   0.02128 0.00000
```


#### Question6

```r
q6i <- table(f1$Q6, f1$Pedagogie)
q6f <- table(f2$Q6, f2$Pedagogie)
q6i
```

```
##        
##         Active Passive
##   FAUX      32      33
##   JUSTE      7       3
##   NSP       11      16
```

```r
q6f
```

```
##        
##         Active Passive
##   FAUX      29      36
##   JUSTE     18      16
```

```r
chisq.test(q6i)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  q6i 
## X-squared = 2.503, df = 2, p-value = 0.2861
```

```r
chisq.test(q6f)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  q6f 
## X-squared = 0.3316, df = 1, p-value = 0.5647
```

```r
prop.table(q6i, 2)
```

```
##        
##          Active Passive
##   FAUX  0.64000 0.63462
##   JUSTE 0.14000 0.05769
##   NSP   0.22000 0.30769
```

```r
prop.table(q6f, 2)
```

```
##        
##         Active Passive
##   FAUX  0.6170  0.6923
##   JUSTE 0.3830  0.3077
```



Analyse de la Prise en charge (PEC)
===================================

La PEC est évaluée par une vingtaine de critères de jugements. On compare les deux méthodes pédagogiques sur ces critères avec le test du CHI-2. Ce test donne la probabilité d'observer un tel résultats s'il y a une différence d'efficacité entre les 2 groupes. On considère (arbitraireent) qu'une différence est significative (réelle) si cette probabilité est inférieure à 5% (p-value < 0.05)

Sur cette règle de décision, seuls 2 critères de jugement sont significatifs:
- "pas d'interruptions"
- "stop message"
et c'est la métode passive qui semble donner les meilleurs résultats ?
Pour toutes les aures questions, il n'y a aps de différence entre les groupes.


```r
file <- "QCM_2013_PEC.csv"
f3 <- read.csv(file, header = TRUE, sep = ",", skip = 1)
names(f3)
```

```
##  [1] "X"                    "Session"              "Evaluateur"          
##  [4] "Pedagogie"            "Protection"           "neuro"               
##  [7] "LVA"                  "respi"                "Alerte"              
## [10] "STOP.MES.APRES"       "Position"             "Rythme"              
## [13] "Alternance"           "Insufflation"         "Pas.d.interruption"  
## [16] "STOP.MES"             "DAE.des.que.possible" "secutité"            
## [19] "RCP.apres.choc"       "Pouls.si.pas.choc"    "STOP.MES.1"          
## [22] "spontané"             "sur.demande"
```

```r

t <- table(f3$Pedagogie, f3$Protection)
t
```

```
##          
##            0  1
##   active   0 31
##   passive  2 28
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.5516, df = 1, p-value = 0.4577
```

```r

t <- table(f3$Pedagogie, f3$neuro)
t
```

```
##          
##            0  1
##   active   6 25
##   passive 12 18
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 2.21, df = 1, p-value = 0.1371
```

```r

t <- table(f3$Pedagogie, f3$LVA)
t
```

```
##          
##            0  1
##   active  10 21
##   passive 11 19
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.0086, df = 1, p-value = 0.9261
```

```r

t <- table(f3$Pedagogie, f3$respi)
t
```

```
##          
##            0  1
##   active  17 14
##   passive 16 14
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0, df = 1, p-value = 1
```

```r

t <- table(f3$Pedagogie, f3$Alerte)
t
```

```
##          
##            0  1
##   active  16 15
##   passive  8 22
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 2.999, df = 1, p-value = 0.08331
```

```r

t <- table(f3$Pedagogie, f3$STOP.MES.APRES)
t
```

```
##          
##           1
##   active  6
##   passive 4
```

```r
chisq.test(t)
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  t 
## X-squared = 0.4, df = 1, p-value = 0.5271
```

```r

t <- table(f3$Pedagogie, f3$Position)
t
```

```
##          
##            0  1
##   active   6 19
##   passive  5 21
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.0054, df = 1, p-value = 0.9415
```

```r

t <- table(f3$Pedagogie, f3$Rythme)
t
```

```
##          
##            0  1
##   active   9 16
##   passive 11 15
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.0304, df = 1, p-value = 0.8616
```

```r

t <- table(f3$Pedagogie, f3$Alternance)
t
```

```
##          
##            0  1
##   active   4 21
##   passive  5 21
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0, df = 1, p-value = 1
```

```r

t <- table(f3$Pedagogie, f3$Insufflation)
t
```

```
##          
##            0  1
##   active  13 12
##   passive 14 12
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0, df = 1, p-value = 1
```

```r

t <- table(f3$Pedagogie, f3$Pas.d.interruption)
t
```

```
##          
##            0  1
##   active  11 14
##   passive  3 23
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 5.212, df = 1, p-value = 0.02243
```

```r

t <- table(f3$Pedagogie, f3$STOP.MES)
t
```

```
##          
##           1
##   active  6
##   passive 0
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  t 
## X-squared = 6, df = 1, p-value = 0.01431
```

```r

t <- table(f3$Pedagogie, f3$DAE.des.que.possible)
t
```

```
##          
##            0  1
##   active   3 16
##   passive  5 21
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0, df = 1, p-value = 1
```

```r

t <- table(f3$Pedagogie, f3$secutité)
t
```

```
##          
##            0  1
##   active   9 10
##   passive 16 10
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.4111, df = 1, p-value = 0.5214
```

```r

t <- table(f3$Pedagogie, f3$RCP.apres.choc)
t
```

```
##          
##            0  1
##   active   4 15
##   passive  3 23
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.2056, df = 1, p-value = 0.6503
```

```r

t <- table(f3$Pedagogie, f3$Pouls.si.pas.choc)
t
```

```
##          
##            0  1
##   active   8 11
##   passive 11 15
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0, df = 1, p-value = 1
```

```r

t <- table(f3$Pedagogie, f3$STOP.MES.1)
t
```

```
##          
##           1
##   active  1
##   passive 2
```

```r
chisq.test(t)
```

```
## Warning: Chi-squared approximation may be incorrect
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  t 
## X-squared = 0.3333, df = 1, p-value = 0.5637
```

```r

t <- table(f3$Pedagogie, f3$spontané)
t
```

```
##          
##            0  1
##   active  11  7
##   passive 18  6
```

```r
chisq.test(t)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  t 
## X-squared = 0.3922, df = 1, p-value = 0.5311
```

