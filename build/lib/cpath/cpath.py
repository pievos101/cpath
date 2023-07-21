"""
    CPath functionality

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-13
"""

import copy
import random
import numpy as np
from .utilities import shuffle_column_values


def cpath(model, test_set, k):
    """
    Cpath - Bastian's explanation method

    :param model: Model
    :param test_set: Test set
    :param k: Number of samples
    :return:
    """

    # [1.] Get the predictions of the model ----------------------------------------------------------------------------
    labels = model.predict(test_set)

    # [2.] Init the variables ------------------------------------------------------------------------------------------
    test_setX = copy.deepcopy(test_set)
    cf_path = np.repeat(np.nan, k)
    label_switch = False

    # [3.] Randomly select a feature -----------------------------------------------------------------------------------
    test_set_shape = test_set.shape
    test_set_row_nr = test_set_shape[0]
    test_set_col_nr = test_set_shape[1]
    feature_cols_range = list(range(test_set_col_nr))

    f_start = random.choices(feature_cols_range, k=k)
    cf_path = copy.deepcopy(f_start)

    label_switch_all = np.repeat(False, test_set_row_nr)

    # [4.] For all sampled features ------------------------------------------------------------------------------------
    for xx in range(0, k):

        test_setX = shuffle_column_values(test_setX, f_start[xx])
        labels_perm = model.predict(test_setX)

        # [5.] Predict and check whether label changes -----------------------------------------------------------------
        if not all(v == 0 for v in list(labels_perm)):

            # [6.] Sampling according to Bernoulli distribution --------------------------------------------------------
            p = np.mean(labels != labels_perm)
            stop = np.random.binomial(1, p, 1)[0]

            # [7.] Label permutations ----------------------------------------------------------------------------------
            if stop:
                label_switch_all = labels != labels_perm
                label_switch = True
                cf_path = cf_path[0:(xx+1)]

                # print(f"Cf_Path assign A ::: xx: {xx}, {cf_path}")

                break
        else:
            cf_path = np.repeat(np.nan, k)

            # print(f"Cf_Path assign B ::: xx: {xx}, {cf_path}")

            break

    # [8.] Return value ------------------------------------------------------------------------------------------------
    cpath_dict = {
        "orig_path": f_start,
        "cf_path": cf_path,
        "label_switch": label_switch,
        "label_switch_all": label_switch_all
    }

    # print("--------- cpath_dict ---------")   # ----------------------------------------------------------------------
    # print(cpath_dict["orig_path"])
    # print(cpath_dict["cf_path"])
    # print(cpath_dict["label_switch"])
    # print(cpath_dict["label_switch_all"])
    # print("------------------------------")   # ----------------------------------------------------------------------

    return cpath_dict
