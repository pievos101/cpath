<p align="center">
<img src="https://github.com/pievos101/cpath/blob/main/logo.png" width="400">
</p>

# Explainable AI with counterfactual paths

## Usage

Install the Python package cpath via pip

```python
pip install cpath
```

and import  

```python
import cpath
```

or from source 

```python
pip install ./cpath
import cpath
```

Other imports

```python
from imodels.util.data_util import get_clean_dataset
import numpy as np
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import balanced_accuracy_score
from sklearn.metrics import roc_auc_score

import sys
```

Example data set 

```python
clf_datasets = [
    ("breast-cancer", "breast_cancer", "imodels")
]

# Read in data set
X, y, feature_names = get_clean_dataset('breast_cancer', data_source='imodels')

# train-test split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20)

```

### Random Forest 

```python

# number of trees 
ntrees = 10

clf = RandomForestClassifier(n_estimators=ntrees) 
clf.fit(X_train, y_train)
pred = clf.predict(X_test)

```

### Explain using cpath

```python

P = cpath.cpaths(clf, X_test, y_test)

T = cpath.transition(P, X_test, y_test)

IMP = cpath.importance(T)

IMP["global"]

```

## Citation
If you find cpath please cite

```
@misc{pfeifer2023explainable,
      title={Explainable AI with counterfactual paths}, 
      author={Bastian Pfeifer and Mateusz Krzyzinski and Hubert Baniecki and Anna Saranti and Andreas Holzinger and Przemyslaw Biecek},
      year={2023},
      eprint={2307.07764},
      archivePrefix={arXiv},
      primaryClass={cs.AI}
}
```
