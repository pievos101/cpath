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
        ncols   = EDGES_all[:,:,xx].shape[1]
        sumALL  = EDGES_all[:,:,xx].sum()
        IMP_xx  = EDGES_all[:,:,xx].sum(axis=0)/sumALL
        IMP_local = IMP_local.append(IMP_xx)
    
    importance_dict={}
    importance_dict["global"] = IMP_global
    importance_dict["local"] = IMP_local
    return importance_dict