"""
    CPath starting script

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-13
"""

import os

import pandas as pd
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import train_test_split
from sklearn import tree

from cpath_packs.cpath import cpath
from cpath_packs.cpaths import cpaths
from cpath_packs.trans import transition

########################################################################################################################
# [0.] Import the iris dataset =========================================================================================
########################################################################################################################
print("Load the data")
data_path = os.path.join("data", "input_data", "titanic_dataset")

# Load and clean
data = pd.read_csv(os.path.join(data_path, "titanic_train.csv"))
data = data[data["Age"].notnull()]                              # Filter rows which are nan ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data["Sex"] = pd.get_dummies(data["Sex"])["female"]             # Dummy code sex (1==Female) ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create X and y
X = data[['Age', 'Pclass', 'Sex', 'PassengerId']].values
y = data["Survived"].values.astype("float")

########################################################################################################################
# [1.] Train after splitting of the dataset ============================================================================
########################################################################################################################
print("Train test split")
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2,
                                                    shuffle=True, stratify=y, random_state=42)

dt_classifier = tree.DecisionTreeClassifier()
dt_classifier.fit(X_train, y_train)

y_pred = dt_classifier.predict(X_test)                              # Predict class ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
roc_auc_performance = roc_auc_score(y_test, y_pred)                 # ROC AUC score on test set ~~~~~~~~~~~~~~~~~~~~~~~~
print(f"ROC-AUC performance regular: {roc_auc_performance}")

########################################################################################################################
# [2.] Apply Cpaths - Get counterfactual paths =========================================================================
########################################################################################################################
# cpath(dt_classifier, X_test, 4)
counterfactual_paths = cpaths(dt_classifier, X_test, y_test, k=4, n_iter=1000)
# print(counterfactual_paths)

########################################################################################################################
# [3.] Transition function =============================================================================================
########################################################################################################################
EDGES_all = transition(counterfactual_paths, X_test, y_test)
print(EDGES_all)
