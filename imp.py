"""
    Importance function

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-14
"""

import math
import sys

import numpy as np
# np.set_printoptions(threshold=sys.maxsize)


def importance(EDGES_all) -> dict:
    """
    Importance function

    :param EDGES_all: Transition matrix
    :return:
    """

    ncols = EDGES_all.shape[1]
    IMP_all = np.empty(ncols)
    sumALL  = EDGES_all.sum()

    IMP_all = EDGES_all.sum(axis=0)/sumALL

    return(IMP_all)