Understanding heating operation in homes is crucial to transition to
alternative energy sources. This study proposes a methodology that uses
indoor temperatures to generate heating state labels, which are then used as
ground truth to train classification models identifying heating operation from
gas consumption and external temperature.
The heating state labels are generated from living room temperatures and
average indoor temperatures from a set of homes, which are then validated
against heating flow temperatures. The labels generated from average indoor
temperatures are found to be more robust and are used as ground truth labels
to test and compare the performance of four classification algorithms: Logistic
Regression, Decision Trees, Random Forest, and XGBoost.
The models using the four algorithms are trained on gas consumption, rolling
statistics and lagged variables extracted from gas consumption, date-time
features and external temperatures. Rolling statistics are found to be the most
significant in modelling, while the majority of date-time features were not
found important. The heating labels are found to be heavily imbalanced and
the performance of the models significantly improves after oversampling the
training data. Logistic Regression and Decision Trees models are found to be
the best performing, identifying heating operations with an F1-scores of 65%
and 66% respectively.
