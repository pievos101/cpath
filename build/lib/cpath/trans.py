"""
    Transition function

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-14
"""

import math
import sys

import numpy as np
# np.set_printoptions(threshold=sys.maxsize)


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

