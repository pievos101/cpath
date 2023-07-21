"""
    CPATH 
    :author: Anna Saranti and Bastian Pfeifer
"""

import numpy as np
import copy
import random
import math
import sys
from .utilities import shuffle_column_values

def cpath(model, test_set, k):
    """
    cpath 

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


def cpaths(model, data, target, k= 4, n_iter= 1000):
    """
    Cpaths - Many Cpaths

    :param model: Any model that contains the methods fit() and predict(), such as in scikit-learn
    :param data: input data for prediction
    :param target: Target values
    :param k: Number of samples
    :param n_iter: Number of iterations
    :return:
    """

    # [1.] Calculate CF paths for each sample --------------------------------------------------------------------------
    PATHS = np.empty((n_iter, k + 2,))
    PATHS[:] = np.nan

    PATHS_l = np.empty([n_iter, k + 2, len(target)])
    PATHS_l[:] = np.nan

    # [2.] First iteration ---------------------------------------------------------------------------------------------
    for xx in range(0, len(target)):
        PATHS_l[:, :, xx] = PATHS

    # [3.] Second iteration --------------------------------------------------------------------------------------------
    for xx in range(0, n_iter):

        res = cpath(model, data, k=k)
        ids = np.where(res["label_switch_all"] == 1)[0]

        for yy in range(0, len(target)):

            # PATHS_l[[yy]][xx, 0:len(res["cf_path"])] = res["cf_path"]
            PATHS_l[xx, 0:len(res["cf_path"]), yy] = res["cf_path"]
            # print(res["cf_path"])
            # print(f"PATHS_l[xx, :, yy]: {PATHS_l[xx, :, yy]}")

            if yy in ids:
                PATHS_l[xx, k, yy] = True
                PATHS_l[xx, k + 1, yy] = sum(res["label_switch_all"])
            else:
                PATHS_l[xx, k, yy] = False
                PATHS_l[xx, k + 1, yy] = sum(res["label_switch_all"])

            # print(f"PATHS_l[xx, :, yy]: {PATHS_l[xx, :, yy]}")
            # print("-------------------------------------------------")

    return PATHS_l


def importance(EDGES_l) -> dict:
    """
    Importance function

    :param EDGES_l: Transition matrices 3d returned by trans.py
    :return:
    """

    EDGES_all = np.sum(EDGES_l, axis=2)
    ncols = EDGES_all.shape[1]
    sumALL  = EDGES_all.sum()
    IMP_global = EDGES_all.sum(axis=0)/sumALL
    
    IMP_local = []
    for xx in range(EDGES_l.shape[2]):
        ncols   = EDGES_l[:,:,xx].shape[1]
        sumALL  = EDGES_l[:,:,xx].sum()
        IMP_xx  = EDGES_l[:,:,xx].sum(axis=0)/sumALL
        IMP_local.append(IMP_xx)
    
    importance_dict={}
    importance_dict["global"] = IMP_global
    importance_dict["local"] = IMP_local
    return importance_dict



def transition(PATHS_l, data, target, add1 = False) -> dict:
    """
    Transition function

    :param PATHS_l: Model
    :param data: Test set
    :param target: Target of the test set data
    :param add1:
    :return:
    """

    # [1.] Init --------------------------------------------------------------------------------------------------------
    k = PATHS_l.shape[1] - 2
    data_shape = data.shape
    data_row_nr = data_shape[0]
    data_col_nr = data_shape[1]

    EDGES = np.zeros((data_col_nr, data_col_nr))
    EDGES_l = np.empty([data_col_nr, data_col_nr, len(target)])

    for xx in range(0, len(target)):
        EDGES_l[:, :, xx] = EDGES

    for aa in range(0, len(target)):
        for xx in range(0, PATHS_l[:, :, aa].shape[0]):     # for xx in range(0, dim(PATHS_l[[aa]])[1]):  --------------

            check = True

            for yy in range(0, k-1):
                if not check:
                    break                                   # path of length 1 or end of path !!! ----------------------

                zz = yy + 1

                if math.isnan(PATHS_l[xx, 1, aa]):              # path of length 1 -------------------------------------
                    if PATHS_l[xx, k, aa] == 1:

                        # print(f"xx, yy, aa, PATHS_l[xx, yy, aa], np.nansum(PATHS_l[xx, :, aa]): "
                        #      f"{xx, yy, aa, PATHS_l[xx, yy, aa], np.nansum(PATHS_l[xx, :, aa])}")

                        EDGES_l[int(PATHS_l[xx, yy, aa]), int(PATHS_l[xx, yy, aa]), aa] = \
                            EDGES_l[int(PATHS_l[xx, yy, aa]), int(PATHS_l[xx, yy, aa]), aa] + \
                            np.nansum(PATHS_l[xx, :, aa]) + 1                           # penalty on path length -------

                    check = False
                    break  # path of length 1

                if math.isnan(PATHS_l[xx, zz, aa]):
                    check = False
                    break                                                               # end of path ------------------

                if PATHS_l[xx, k, aa] == 1:

                    # print(f"xx, yy, aa, PATHS_l[xx, yy, aa], np.nansum(PATHS_l[xx, :, aa]): "
                    #      f"{xx, yy, aa, PATHS_l[xx, yy, aa], np.nansum(PATHS_l[xx, :, aa])}")

                    EDGES_l[int(PATHS_l[xx, yy, aa]), int(PATHS_l[xx, zz, aa]), aa] = \
                        EDGES_l[int(PATHS_l[xx, yy, aa]), int(PATHS_l[xx, zz, aa]), aa] + \
                        np.nansum(PATHS_l[xx, :, aa]) + 1                               # penalty on path length -------

    return EDGES_l

