"""
    XAI sensitivity

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-28
"""

import copy
import itertools

from captum.attr import Saliency
from captum.metrics import sensitivity_max

from scipy.stats import pearsonr, spearmanr
import torch
import torchvision


def sensitivity_n(model, x_data, features_names_list: list, attribution_map_e: list) -> float:
    """
    Compute the Sensitivity-N

    :param model: Model under quality inspection
    :param x_data: Input data
    :param features_names_list: List of feature names - for presentation
    :param attribution_map_e: Attribution map

    :return:
    """

    all_features_nr = len(features_names_list)
    all_feature_indexes = list(range(all_features_nr))

    m_X_c = model.predict(x_data)

    all_predictions_differences_list = []
    all_attribution_sums_list = []

    # [1.] Iterate over feature numbers --------------------------------------------------------------------------------
    for features_nr in range(all_features_nr):

        features_combinations_list = list(itertools.combinations(all_feature_indexes, features_nr + 1))

        for features_set in features_combinations_list:

            features_set_list = list(features_set)
            print(f"Features set: {features_set_list}")

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

            m_predictions_diff = sum(m_predictions_diff_all_data)/len(m_predictions_diff_all_data)
            print(f"Prediction Diff: {m_predictions_diff}")
            all_predictions_differences_list.append(m_predictions_diff)
            print("---------------------------------------------------------------------------------------------------")

    # [6.] Pearson correlation -----------------------------------------------------------------------------------------
    print("\n")
    print("-----------------------------------------------------------------------------------------------------------")
    pearson_corr, _ = pearsonr(all_attribution_sums_list, all_predictions_differences_list)
    print('Pearsons correlation: %.3f' % pearson_corr)

    # [7.] Spearman correlation ----------------------------------------------------------------------------------------
    print("-----------------------------------------------------------------------------------------------------------")
    spearman_corr, _ = spearmanr(all_attribution_sums_list, all_predictions_differences_list)
    print('Spearman correlation: %.3f' % spearman_corr)

    print("===========================================================================================================")


def sensitivity_max_captum(model, x_data, features_names_list: list, attribution_map_e: list) -> float:
    """
    Use the Sensitivity-Max from Captum

    :param model:
    :param x_data:
    :param features_names_list:
    :param attribution_map_e:
    :return:
    """

    # ImageClassifier takes a single input tensor of images Nx3x224x224, -----------------------------------------------
    # and returns an Nx10 tensor of class probabilities. ---------------------------------------------------------------
    net = torchvision.models.vgg19(pretrained=True)
    saliency = Saliency(net)
    print("Saliency and saliency.attribute")
    print(saliency)
    print(saliency.attribute)

    images_nr = 5
    print(f"Nr. of images: {images_nr}")
    input = torch.randn(images_nr, 3, 224, 224, requires_grad=True)

    # Computes sensitivity score for saliency maps of class 3 ----------------------------------------------------------
    sens = sensitivity_max(saliency.attribute, input, target=3)
    print(f">>>>>>>>>>>>>>> Sensitivity-Max of Captum: {sens}")

