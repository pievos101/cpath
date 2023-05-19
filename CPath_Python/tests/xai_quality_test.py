"""
    XAI quality evaluation test-cases

    :author: Anna Saranti
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-30
"""

from hypothesis import given, settings
from hypothesis import strategies


@given(feature_importances=strategies.lists(
    strategies.floats(min_value=0.1, max_value=0.5, allow_infinity=False, allow_nan=False), min_size=4, max_size=4))
@settings(max_examples=10)
def test_fractional_em_start_uniform(feature_importances: float):
    """

    :param feature_importances:
    :return:
    """

    print(f"feature_importances: {feature_importances}")    # .map(normalize)





