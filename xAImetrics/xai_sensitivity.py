"""
    XAI sensitivity

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-28
"""

import copy
import itertools

import numpy as np
import shap
from scipy.stats import kendalltau, pearsonr, spearmanr
from sklearn import tree
from sklearn.model_selection import train_test_split

from xplanations.cpath_packs.cpaths import cpaths
from xplanations.cpath_packs.imp import importance
from xplanations.cpath_packs.trans import transition
from xplanations.explanation_lime import lime_explanation
from utils.utilities import shap_values_aggr


def sensitivity_n(X, y, dataset_name: str, features_names_list: list, lime_categorical_feature_indexes: list):
    """
    Compute the Sensitivity-N

    :param X: Input data
    :param y: Target variable
    :param dataset_name: Dataset name
    :param features_names_list: List of feature names - for presentation
    :param lime_categorical_feature_indexes: Indexes of the categorical features for LIME

    :return:
    """

    excercising_datasets_list = ["X_train", "X_test"]

    for excercising_dataset in excercising_datasets_list:

        print(f"Excercising dataset: {excercising_dataset}")

        X_train, X_test, y_train, y_test = train_test_split(X,
                                                            y,
                                                            test_size=0.2,
                                                            shuffle=True,
                                                            stratify=y,
                                                            random_state=42)

        dt_classifier = tree.DecisionTreeClassifier()
        dt_classifier.fit(X_train, y_train)

        if excercising_dataset == "X_train":
            X_excercise = np.array(X_train, copy=True)
            y_excercise = np.array(y_train, copy=True)
        elif excercising_dataset == "X_test":
            X_excercise = np.array(X_test, copy=True)
            y_excercise = np.array(y_test, copy=True)

        num_samples_n, feature_nr = X_excercise.shape

        ################################################################################################################
        # [1.] SHAP explanations ---------------------------------------------------------------------------------------
        ################################################################################################################
        explainer = shap.Explainer(dt_classifier)
        shap_values = explainer(X_excercise)
        shap_feature_importance, shap_importance_samples_times_features = shap_values_aggr(features_names_list,
                                                                                           shap_values)

        ################################################################################################################
        # [2.] CPATH ---------------------------------------------------------------------------------------------------
        ################################################################################################################
        counterfactual_paths = cpaths(dt_classifier, X_excercise, y_excercise, k=feature_nr, n_iter=1000)
        EDGES_l = transition(counterfactual_paths, X_excercise, y_excercise)

        IMP = importance(EDGES_l)
        imp_global = IMP["global"]
        imp_local = IMP["local"]

        ################################################################################################################
        # [3.] LIME ----------------------------------------------------------------------------------------------------
        ################################################################################################################
        lime_explanations_global_local_dict = lime_explanation(dt_classifier, X_excercise,
                                                               features_names_list, lime_categorical_feature_indexes)
        lime_global = lime_explanations_global_local_dict["global"]
        lime_local = lime_explanations_global_local_dict["local"]

        ################################################################################################################
        # [4.] Compute sensitivity-n -----------------------------------------------------------------------------------
        ################################################################################################################
        pearson_corr_shap, spearman_corr_shap, kendalls_tau_shap = \
            sensitivity_n_computation(dt_classifier, X_excercise, y_excercise, shap_feature_importance, feature_nr)
        pearson_corr_cpath, spearman_corr_cpath, kendalls_tau_cpath = \
            sensitivity_n_computation(dt_classifier, X_excercise, y_excercise, imp_global, feature_nr)
        pearson_corr_lime, spearman_corr_lime, kendalls_tau_lime = \
            sensitivity_n_computation(dt_classifier, X_excercise, y_excercise, lime_global, feature_nr)

        print(f"Sensitivity-n SHAP_{excercising_dataset}: Pearson: {round(pearson_corr_shap, 3)}, "
              f"Spearman: {round(spearman_corr_shap, 3)}, Kendall: {round(kendalls_tau_shap, 3)}")
        print(f"Sensitivity-n CPATH_{excercising_dataset}: Pearson: {round(pearson_corr_cpath, 3)}, "
              f"Spearman: {round(spearman_corr_cpath, 3)}, Kendall: {round(kendalls_tau_cpath, 3)}")
        print(f"Sensitivity-n LIME_{excercising_dataset}: Pearson: {pearson_corr_lime}, "
              f"Spearman: {round(spearman_corr_lime, 3)}, Kendall: {round(kendalls_tau_lime, 3)}")


def sensitivity_n_computation(model, x_data: np.ndarray, m_X_c: np.ndarray,
                              attribution_map_e: list, all_features_nr: int) -> tuple:
    """
    Computation of sensitivity

    :param model: Trained model
    :param x_data: Input data
    :param m_X_c: Target labels (practically y_data)
    :param attribution_map_e: List of global explanations
    :param all_features_nr: Number of all features

    :return: Tuple with different correlation metrics
    """

    all_feature_indexes = list(range(all_features_nr))

    all_predictions_differences_list = []
    all_attribution_sums_list = []

    # [1.] Iterate over feature numbers --------------------------------------------------------------------------------
    for features_nr in range(all_features_nr):

        features_combinations_list = list(itertools.combinations(all_feature_indexes, features_nr + 1))

        for features_set in features_combinations_list:

            features_set_list = list(features_set)
            # print(f"Features set: {features_set_list}")

            # Create the instance that has some features "inactive" ----------------------------------------------------
            x_data_S_i = copy.deepcopy(x_data)

            attribution_sum_e_s = 0

            # [2.] "Remove" all values of the columns specified through the features set -------------------------------
            for feature_remove_idx in features_set_list:
                # [2.1.] Set the values of those features to 0 ---------------------------------------------------------
                x_data_S_i[:, feature_remove_idx] = 0

                # [2.3.] Sum the attributions of the "inactive" features -----------------------------------------------
                attribution_sum_e_s += attribution_map_e[feature_remove_idx]

            # [3.] Gather the sum of the attibutions of all "inactive" features ----------------------------------------
            all_attribution_sums_list.append(attribution_sum_e_s)

            # [4.] Gather the model's prediction for the instance that has "inactive" features -------------------------
            m_X_S_i_c = model.predict(x_data_S_i)

            # [5.] Compute the difference between the original prediction and the one with "inactive" features ---------
            m_predictions_diff_all_data = m_X_c - m_X_S_i_c

            m_predictions_diff = sum(m_predictions_diff_all_data) / len(m_predictions_diff_all_data)
            # print(f"Prediction Diff: {m_predictions_diff}")
            all_predictions_differences_list.append(m_predictions_diff)
            # print("-------------------------------------------------------------------------------------------------")

    # [6.] Pearson correlation -----------------------------------------------------------------------------------------
    # print("\n")
    # print("---------------------------------------------------------------------------------------------------------")
    pearson_corr, _ = pearsonr(all_attribution_sums_list, all_predictions_differences_list)
    print('Sensitivity-n - Pearsons correlation: %.3f' % pearson_corr)

    # [7.] Spearman correlation ----------------------------------------------------------------------------------------
    print("-----------------------------------------------------------------------------------------------------------")
    spearman_corr, _ = spearmanr(all_attribution_sums_list, all_predictions_differences_list)
    print('Sensitivity-n - Spearman correlation: %.3f' % spearman_corr)

    # [8.] Kendall's Tau -----------------------------------------------------------------------------------------------
    print("-----------------------------------------------------------------------------------------------------------")
    kendalls_tau, kendalls_p_value = kendalltau(all_attribution_sums_list, all_predictions_differences_list)
    print(f'Kendalls Tau: {kendalls_tau}, Kendalls P-Value: {kendalls_p_value}')

    print("===========================================================================================================")

    return abs(pearson_corr), abs(spearman_corr), abs(kendalls_tau)
