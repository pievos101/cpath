"""
    XAI infidelity

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-28
"""

import numpy as np


def infidelity(model, x_data, features_names_list: list, attribution_map_e: list) -> float:
    """
    Compute the Sensitivity-N

    :param model: Model under quality inspection
    :param x_data: Input data
    :param features_names_list: List of feature names - for presentation
    :param attribution_map_e: Attribution map

    :return:
    """

    infidelity_all = []

    mu = 0
    sigmas_list = [0.2, 0.5, 1.0]

    prediction_x_orig = model.predict(x_data)  # [samples_nr,] ::: 1 prediction per sample -----------------------------

    # [1.] Define the matrix that specifies the perturbation of the original matrix ------------------------------------
    for sigma in sigmas_list:

        # Same size as input [samples_nr * features_nr]
        added_noise_matrix_I = np.random.normal(mu, sigma, x_data.shape)

        # [2.] Create the perturbed data (x - I) -----------------------------------------------------------------------
        # Same size as input [samples_nr * features_nr]
        x_minus_I = x_data - added_noise_matrix_I

        # [3.] Calculate the prediction for this sample using this model -----------------------------------------------
        #      and then diff with original prediction ------------------------------------------------------------------
        prediction_x_minus_I = model.predict(x_minus_I)                     # [samples_nr,] ::: 1 prediction per sample
        predictions_diff = prediction_x_orig - prediction_x_minus_I         # [samples_nr,] ::: 1 prediction per sample

        # [4.] Noise/Perturbation I \times Phi(f, x) -------------------------------------------------------------------
        #      [samples_nr * features_nr] \times [features_nr, 1] --> # [samples_nr,]
        attribution_map_e_array = np.array(attribution_map_e)
        noised_attribution = np.matmul(added_noise_matrix_I, attribution_map_e_array)
        # print(added_noise_matrix_I.shape, attribution_map_e_array.shape, noised_attribution.shape)

        # [5.] Subtract and ^2 the noised attribution with the predictions difference ----------------------------------
        noised_attr_minus_pred_diff = noised_attribution - predictions_diff
        noised_attr_minus_pred_diff_pow_2 = np.power(noised_attr_minus_pred_diff, 2)

        # [6.] Infidelity ----------------------------------------------------------------------------------------------
        infidelity_all.extend(noised_attr_minus_pred_diff_pow_2)

    # [7.] Mean value of gathered infidelities -------------------------------------------------------------------------
    infidelity_mean = np.average(infidelity_all)

    print(f"Infidelity: {infidelity_mean}")

