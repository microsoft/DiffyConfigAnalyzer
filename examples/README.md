## 5GvRAN

The configs in the [5GvRAN](5GvRAN) directory are collected from a total of
seven RUs (Radio Units) in a 5G vRAN (virtualized Radio Access Network) testbed
at Microsoft.

42 RU configs were created by previous testbed users, and we generated an
additional 8 configs to retroactively introduce back several types of
bugs commonly encountered during the daily operation.

All RU configs have been converted from XML to JSON for evaluating Diffy.


## MySQL

The configs in the [MySQL](MySQL) directory are the same as the MySQL configs in the [ConfigV](https://github.com/ConfigV/ConfigV) paper dataset. 
ConfigV learned invariants (rules) from a [training/learning set](https://github.com/ConfigV/ConfigV/tree/master/Datasets/learningSet/MySQL) of 256 configurations and tested them on a 973 configurations, randomly selected from [GitHub](https://github.com/ConfigV/ConfigV/tree/master/Datasets/githubFiles).

We converted the MySQL configurations to the JSON format used by Diffy and discarded 30 configurations from the test set that could not be converted due to bad formatting and parsing errors.
The remaining 256 training and 943 test configurations are in the MySQL directory, with the training set files named with the prefix `LearningSet` and the test set files named with the prefix `GitHubFiles`, following the naming convention of the ConfigV dataset.
