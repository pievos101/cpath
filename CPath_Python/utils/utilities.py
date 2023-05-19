"""
    CPath utilities

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-13
"""

import numpy as np

from scipy.special import softmax


def shuffle_column_values(data: np.ndarray, column_index: int) -> np.ndarray:
    """
    Permute the values of a column

    :param data: Numpy array of input data - the column's values will be shuffled
    :param column_index: Index of column that will be shuffled
    :return:
    """

    original_column_values = data[:, column_index]
    np.random.shuffle(original_column_values)
    data[:, column_index] = original_column_values

    return data


def shap_values_aggr(features_names_list: list, shap_values: np.ndarray) -> list:
    """
    Aggregate, mean and softmax application to the array of shap values
    per feature - as taken from the work of Vinícius Trevisan
    https://towardsdatascience.com/using-shap-values-to-explain-how-your-machine-learning-model-works-732b3f40e137

    :param features_names_list: List with names of features
    :param shap_values: Array with shap values
    :return: The list of feature importance list
    """

    importances = []
    for i in range(shap_values.values.shape[1]):
        importances.append(np.mean(np.abs(shap_values.values[:, i])))

    # [1.] Calculate the normalized version ----------------------------------------------------------------------------
    importances_norm = softmax(importances)

    # [2.] Organize the importances and columns in a dictionary --------------------------------------------------------
    feature_importances = {fea: imp for imp, fea in zip(importances, features_names_list)}
    feature_importances_norm = {fea: imp for imp, fea in zip(importances_norm, features_names_list)}

    # [3.] Sorts the dictionary ----------------------------------------------------------------------------------------
    feature_importances = {k: v for k, v in sorted(feature_importances.items(), key=lambda item: item[1], reverse=True)}
    feature_importances_norm = {k: v for k, v in
                                sorted(feature_importances_norm.items(), key=lambda item: item[1], reverse=True)}

    # [4.] Print the feature importance's dictionary -------------------------------------------------------------------
    for k, v in feature_importances.items():
        print(f"{k} -> {v:.4f} (softmax = {feature_importances_norm[k]:.4f})")

    # [5.] Resort them so that the sequence matches the one of the columns of the data ---------------------------------
    feature_importances_resorted = []
    for feature_name in features_names_list:
        feature_importances_resorted.append(feature_importances[feature_name])

    return feature_importances_resorted
