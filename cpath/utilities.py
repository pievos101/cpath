"""
    CPath utilities

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-13
"""

import numpy as np


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
