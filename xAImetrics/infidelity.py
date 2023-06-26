"""
    XAI infidelity

    :author: Anna Saranti and refactored by Bastian Pfeifer
    :copyright: © 2023 HCI-KDD (ex-AI) group
    :date: 2023-03-28
"""

def infidelity(X, y, dataset_name: str, features_names_list: list, lime_categorical_feature_indexes: list) -> float:
    """
    Compute Infidelity

    :param X: Input data
    :param y: Target variable
    :param dataset_name: Dataset name
    :param features_names_list: List of feature names - for presentation
    :param lime_categorical_feature_indexes: Indexes of the categorical features for LIME

    :return:
    """

    excercising_datasets_list = ["X_train", "X_test"]

    for excercising_dataset in excercising_datasets_list:

        print(f"Excercising dataset: {excercising_dataset}")

        X_train, X_test, y_train, y_test = train_test_split(X,
                                                            y,
                                                            test_size=0.2,
                                                            shuffle=True,
                                                            stratify=y,
                                                            random_state=42)

        dt_classifier = tree.DecisionTreeClassifier()
        dt_classifier.fit(X_train, y_train)

        if excercising_dataset == "X_train":
            X_excercise = np.array(X_train, copy=True)
            y_excercise = np.array(y_train, copy=True)
        elif excercising_dataset == "X_test":
            X_excercise = np.array(X_test, copy=True)
            y_excercise = np.array(y_test, copy=True)

        num_samples_n, feature_nr = X_excercise.shape

        ################################################################################################################
        # [1.] SHAP explanations ---------------------------------------------------------------------------------------
        ################################################################################################################
        explainer = shap.Explainer(dt_classifier)
        shap_values = explainer(X_excercise)
        shap_feature_importance, shap_importance_samples_times_features = shap_values_aggr(features_names_list,
                                                                                           shap_values)

        ################################################################################################################
        # [2.] CPATH ---------------------------------------------------------------------------------------------------
        ################################################################################################################
        counterfactual_paths = cpaths(dt_classifier, X_excercise, y_excercise, k=feature_nr, n_iter=1000)
        EDGES_l = transition(counterfactual_paths, X_excercise, y_excercise)

        IMP = importance(EDGES_l)
        imp_global = IMP["global"]
        imp_local = IMP["local"]

        ################################################################################################################
        # [3.] LIME ----------------------------------------------------------------------------------------------------
        ################################################################################################################
        lime_explanations_global_local_dict = lime_explanation(dt_classifier, X_excercise,
                                                               features_names_list, lime_categorical_feature_indexes)
        lime_global = lime_explanations_global_local_dict["global"]
        lime_local = lime_explanations_global_local_dict["local"]

        ################################################################################################################
        # [4.] Infidelity ----------------------------------------------------------------------------------------------
        ################################################################################################################
        infidelity_shap = infidelity_computation(dt_classifier, X_excercise, shap_feature_importance)
        infidelity_cpath = infidelity_computation(dt_classifier, X_excercise, imp_global)
        infidelity_lime = infidelity_computation(dt_classifier, X_excercise, lime_global)

        print(f"SHAP Infidelity_{excercising_dataset}: {round(infidelity_shap, 3)}")
        print(f"CPATH Infidelity_{excercising_dataset}: {round(infidelity_cpath, 3)}")
        print(f"LIME Infidelity_{excercising_dataset}: {round(infidelity_lime, 3)}")


infidelity_computation <- function(model, x_data: np.ndarray, attribution_map_e: list):
    """
    Compute the value of infidelity

    :param model: Trained model
    :param x_data: Input data
    :param attribution_map_e: List of global explanations

    :return:
    """

    infidelity_all = c()

    mu = 0
    sigmas_list = c(0.2, 0.5, 1.0)

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
        prediction_x_minus_I = model.predict(x_minus_I)  # [samples_nr,] ::: 1 prediction per sample
        predictions_diff = prediction_x_orig - prediction_x_minus_I  # [samples_nr,] ::: 1 prediction per sample

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

    return infidelity_mean
