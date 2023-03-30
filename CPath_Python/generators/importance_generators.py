"""
    Importance generators to compute infidelity and sensitivity-n

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-30
"""

import sys

import numpy as np
from xai_quality_metrics.xai_infidelity import infidelity
from xai_quality_metrics.xai_sensitivity import sensitivity_n


def importance_generator(model, x_data: np.ndarray, features_names_list: list):
    """
    Importance generator

    :param model: Already trained model
    :param x_data: Input data
    :param features_names_list: Names of features

    :return:
    """

    features_nr = len(features_names_list)

    tryouts_nr = 10000

    min_sensitivity = sys.float_info.max
    min_infidelity = sys.float_info.max

    best_sensitivity_features = []
    best_infidelity_features = []

    for tryouts_idx in range(tryouts_nr):

        generated_importances = list(np.random.uniform(low=0.01, high=1.0, size=(features_nr,)))
        print(f"Generated importances: {generated_importances}")

        sensitivity_idx = abs(sensitivity_n(model, x_data, features_names_list, generated_importances))
        infidelity_idx = infidelity(model, x_data, features_names_list, generated_importances)

        if sensitivity_idx < min_sensitivity:
            min_sensitivity = sensitivity_idx
            best_sensitivity_features = generated_importances
        if infidelity_idx < min_infidelity:
            min_infidelity = infidelity_idx
            best_infidelity_features = generated_importances

        print("-------------------------------------------------------------------------------------------------------")

    print("===========================================================================================================")
    print(f"Best sensitivity: {min_sensitivity} ::: {best_sensitivity_features}")
    print(f"Best infidelity: {min_infidelity} ::: {best_infidelity_features}")
    print("===========================================================================================================")
