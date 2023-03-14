"""
    CPaths functionality

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-14
"""

import numpy as np

from cpath_packs.cpath import cpath


def cpaths(model, data, target, k: int = 4, n_iter: int = 1000):
    """
    Cpaths - Many Cpaths

    :param model: TODO
    :param data:
    :param target:
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
        PATHS_l[[xx]] = PATHS

    # [3.] Second init -------------------------------------------------------------------------------------------------
    for xx in range(0, n_iter):

        res = cpath(model, data, k=k)
        ids = np.where(res["label_switch_all"] == 1)[0]

        for yy in range(0, len(target)):

            PATHS_l[[yy]][xx, 0:len(res["cf_path"])] = res["cf_path"]
            if yy in ids:
                PATHS_l[[yy]][xx, k + 1] = True
                PATHS_l[[yy]][xx, k + 2] = sum(res["label_switch_all"])
            else:
                PATHS_l[[yy]][xx, k + 1] = False
                PATHS_l[[yy]][xx, k + 2] = sum(res["label_switch_all"])

    return PATHS_l

