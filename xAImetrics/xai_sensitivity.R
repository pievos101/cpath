sensitivity_n_computation <- function(model, x_data, m_X_c, attribution_map_e, all_features_nr) {

  all_feature_indexes <- 1:all_features_nr
  all_predictions_differences_list <- vector()
  all_attribution_sums_list <- vector()
  
  for (features_nr in 1:(all_features_nr-1)) {
    features_combinations_list <- combn(all_feature_indexes, features_nr + 1)
    #print(features_combinations_list)

    for (i in 1:ncol(features_combinations_list)) {
      features_set <- features_combinations_list[, i]
      
      features_set_list <- as.list(features_set)
      # cat(paste0("Features set: ", features_set_list, "\n"))
      
      x_data_S_i <- x_data
      attribution_sum_e_s <- 0
      
      for (feature_remove_idx in features_set_list) {
        x_data_S_i[, feature_remove_idx] <- 0
        attribution_sum_e_s <- attribution_sum_e_s + attribution_map_e[feature_remove_idx]
      }
      
      all_attribution_sums_list <- c(all_attribution_sums_list, attribution_sum_e_s)
      m_X_S_i_c <- predict(model, x_data_S_i)$predictions[,1]
      m_predictions_diff_all_data <- m_X_c - m_X_S_i_c
      m_predictions_diff <- sum(m_predictions_diff_all_data) / length(m_predictions_diff_all_data)
      all_predictions_differences_list <- c(all_predictions_differences_list, m_predictions_diff)
      # cat("-------------------------------------------------------------------------------------------------\n")
    }
  }
  
  # [6.] Pearson correlation
  # cat("\n")
  # cat("---------------------------------------------------------------------------------------------------------\n")
  pearson_corr <- cor(all_attribution_sums_list, all_predictions_differences_list, method = "pearson")
  #cat(paste0("Sensitivity-n - Pearsons correlation: ", round(pearson_corr, 3), "\n"))
  
  # [7.] Spearman correlation
  #cat("-----------------------------------------------------------------------------------------------------------\n")
  spearman_corr <- cor(all_attribution_sums_list, all_predictions_differences_list, method = "spearman")
  #cat(paste0("Sensitivity-n - Spearman correlation: ", round(spearman_corr, 3), "\n"))
  
  # [8.] Kendall's Tau
  #cat("-----------------------------------------------------------------------------------------------------------\n")
  kendalls_tau <- cor(all_attribution_sums_list, all_predictions_differences_list, method = "kendall")
  kendalls_p_value <- cor.test(all_attribution_sums_list, all_predictions_differences_list, method = "kendall")$p.value
  #cat(paste0("Kendalls Tau: ", kendalls_tau, ", Kendalls P-Value: ", kendalls_p_value, "\n"))
  
  #cat("===========================================================================================================\n")
  
  return(abs(spearman_corr))
  #return(list(abs(pearson_corr), abs(spearman_corr), abs(kendalls_tau)))

}