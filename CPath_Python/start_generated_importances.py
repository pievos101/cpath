"""
    Generated importances starting script

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-30
"""

import os

import pandas as pd
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import train_test_split
from sklearn import tree

from generators.importance_generators import importance_generator

########################################################################################################################
# [0.] Import the iris dataset =========================================================================================
########################################################################################################################
print("Load the data")
data_path = os.path.join("data", "input_data", "titanic_dataset")

# Load and clean
data = pd.read_csv(os.path.join(data_path, "titanic_train.csv"))
data = data[data["Age"].notnull()]                              # Filter rows which are nan ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data["Sex"] = pd.get_dummies(data["Sex"])["female"]             # Dummy code sex (1==Female) ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create X and y -------------------------------------------------------------------------------------------------------
features_names_list = ['Age', 'Pclass', 'Sex', 'PassengerId']
X = data[features_names_list].values
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
# y_pred_proba = dt_classifier.predict_proba(X_test)

roc_auc_performance = roc_auc_score(y_test, y_pred)                 # ROC AUC score on test set ~~~~~~~~~~~~~~~~~~~~~~~~
print(f"ROC-AUC performance regular: {roc_auc_performance}")

########################################################################################################################
# [2.] Try some importance examples ====================================================================================
########################################################################################################################
importance_generator(dt_classifier, X_test, features_names_list)

