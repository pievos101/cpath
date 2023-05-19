"""
    CPath starting script

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-13
"""

import os

import pandas as pd
import shap
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import train_test_split
from sklearn import ensemble
from sklearn import tree

from cpath_packs.cpath import cpath
from cpath_packs.cpaths import cpaths
from cpath_packs.imp import importance
from cpath_packs.trans import transition
from utils.utilities import shap_values_aggr
from xai_quality_metrics.xai_infidelity import infidelity
from xai_quality_metrics.xai_sensitivity import sensitivity_n


########################################################################################################################
######################################### *** CPATH *** ################################################################
########################################################################################################################

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

# dt_classifier = tree.DecisionTreeClassifier()
model = ensemble.RandomForestClassifier(n_estimators=100)
model.fit(X_train, y_train)

y_pred = model.predict(X_test)                                      # Predict class ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# y_pred_proba = dt_classifier.predict_proba(X_test)

roc_auc_performance = roc_auc_score(y_test, y_pred)                 # ROC AUC score on test set ~~~~~~~~~~~~~~~~~~~~~~~~
print(f"ROC-AUC performance regular: {roc_auc_performance}")

########################################################################################################################
# [2.] Apply Cpaths - Get counterfactual paths =========================================================================
########################################################################################################################
# cpath(dt_classifier, X_test, 4)
counterfactual_paths = cpaths(model, X_test, y_test, k=4, n_iter=1000)
# print(counterfactual_paths)

########################################################################################################################
# [3.] Transition matrix ===============================================================================================
########################################################################################################################
transition_matrix = transition(counterfactual_paths, X_test, y_test)

########################################################################################################################
# [4.] Compute the importance ==========================================================================================
########################################################################################################################
IMP = importance(transition_matrix)
importance_normalized = IMP/sum(IMP)
importance_normalized = importance_normalized/sum(importance_normalized)

print("###############################################################################################################")
print("######################################### *** CPATH *** #######################################################")
print("###############################################################################################################")
print("---------------------------------------------------------------------------------------------------------------")
print("CPATH Importance per feature:")
print(features_names_list)
print(importance_normalized)
print("---------------------------------------------------------------------------------------------------------------")

########################################################################################################################
# [5.] XAI Infidelity metric ===========================================================================================
########################################################################################################################
print("Infidelity of Training Set:")
infidelity(model, X_train, features_names_list, importance_normalized)

print("Infidelity of Test Set:")
infidelity(model, X_test, features_names_list, importance_normalized)

########################################################################################################################
# [6.] XAI Sensitivity-N metric ========================================================================================
########################################################################################################################
print("---------------------------------------------------------------------------------------------------------------")
print("Sensitivity of Training set:")
sensitivity_n(model, X_train, features_names_list, importance_normalized)

print("Sensitivity of Test set:")
sensitivity_n(model, X_test, features_names_list, importance_normalized)

########################################################################################################################
######################################### *** SHAP *** #################################################################
########################################################################################################################
print("###############################################################################################################")
print("######################################### *** SHAP *** ########################################################")
print("###############################################################################################################")

explainer = shap.Explainer(model)
shap_values = explainer(X)

shap_feature_importance_list = shap_values_aggr(features_names_list, shap_values)
shap_feature_importance_list = shap_feature_importance_list/sum(shap_feature_importance_list)

print("---------------------------------------------------------------------------------------------------------------")
print("SHAP Importance per feature:")
print(features_names_list)
print(shap_feature_importance_list)
print("---------------------------------------------------------------------------------------------------------------")

# print("~~~~~~~~~~~~~~~~~~~~~~~~~~~ SHAP values: ~~~~~~~~~~~~~~~~~~~~~~~~~~~")
# shap.summary_plot(shap_values, X_train, feature_names=features_names_list)

print("Infidelity of Training Set:")
infidelity(model, X_train, features_names_list, shap_feature_importance_list)

print("Infidelity of Test Set:")
infidelity(model, X_test, features_names_list, shap_feature_importance_list)

print("---------------------------------------------------------------------------------------------------------------")

print("Sensitivity of Training set:")
sensitivity_n(model, X_train, features_names_list, shap_feature_importance_list)

print("Sensitivity of Test set:")
sensitivity_n(model, X_test, features_names_list, shap_feature_importance_list)

