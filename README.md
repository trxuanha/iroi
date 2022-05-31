# IROI

A R implementation of Intervention Recommendation for Outcome Improvement (IROI) in paper "Intervention Recommendation for Improving Disability Employment".

# Public datasets

Adult census income (ACI). This dataset from Machine Learning Repository contains 48842
records. This dataset is frequently used to perform the analysis of income equality problems.
In our experiment, the goal is to recommend to individuals what they should improve to find
professional jobs. Main attributes used in our experiments are age, education, race, sex,
hours-per-week, workclass, marital-status, relationship, and occupation. The occupation
attribute is binarized to indicate whether an individual has a professional job or not. It is used
as the outcome.

Employee turnover (TO). This dataset has 1129 records about employee turnover. The dataset
has 16 variables describing the characteristics of Russian workers. Main attributes used in our
experiments are age, profession, coach, head gender, grey wage, way, extraversion, independ,
selfcontrol, anxiety, novator, and stag. The stag attribute is used as the outcome. It is the
employment time of workers.

# Computing infrastructure

Installation requirements for IROI:

* R software used to run PIR: pcalg, causalTree, dplyr, rpart.utils, rattle, stringr, survival, survminer, patchwork, ini, data.table, knitr.
* R software required for the baseline methods: recommenderlab.
* Python software required to process results: numpy, pandas, seaborn, matplotlib, scipy, statistics.

Infrastructure used to run experiments:
* OS: Red Hat Enterprise Linux, version 7.8.
* CPU: Intel(R) Xeon(R) Gold 6246 CPU @ 3.30GHz).
* RAM: 16 GB.
