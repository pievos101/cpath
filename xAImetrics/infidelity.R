
infidelity_computation <- function(model, x_data, attribution_map_e, sigma) {
  
  infidelity_all <- c()
  
  mu <- 0
  sigmas_list <- sigma #c(0.1, 0.2, 0.5, 1.0)
  
  prediction_x_orig <- predict(model, x_data)$predictions[,1]
  #prediction_x_orig <- apply(prediction_x_orig,1,function(x){which.max(x)-1})  # [samples_nr,] ::: 1 prediction per sample
  

  for (sigma in sigmas_list) {
    # Same size as input [samples_nr * features_nr]
    added_noise_matrix_I <- matrix(rnorm(length(x_data), mu, sigma), nrow = nrow(x_data), ncol = ncol(x_data))
    
    # Same size as input [samples_nr * features_nr]
    x_minus_I <- x_data - added_noise_matrix_I
    
    # [3.] Calculate the prediction for this sample using this model
    #      and then diff with original prediction
    prediction_x_minus_I <- predict(model, x_minus_I)$predictions[,1]  # [samples_nr,] ::: 1 prediction per sample
    #prediction_x_minus_I <- apply(prediction_x_minus_I,1,function(x){which.max(x)-1})  # [samples_nr,] ::: 1 prediction per sample
  

    predictions_diff <- prediction_x_orig - prediction_x_minus_I  # [samples_nr,] ::: 1 prediction per sample
    
    # [4.] Noise/Perturbation I \times Phi(f, x)
    #      [samples_nr * features_nr] \times [features_nr, 1] --> # [samples_nr,]
    attribution_map_e_array <- as.matrix(attribution_map_e)
    noised_attribution <- added_noise_matrix_I %*% attribution_map_e_array
    
    # [5.] Subtract and ^2 the noised attribution with the predictions difference
    noised_attr_minus_pred_diff <- noised_attribution - predictions_diff
    noised_attr_minus_pred_diff_pow_2 <- noised_attr_minus_pred_diff^2
    
    # [6.] Infidelity
    infidelity_all <- c(infidelity_all, noised_attr_minus_pred_diff_pow_2)
  }
  
  # [7.] Mean value of gathered infidelities
  infidelity_mean <- mean(infidelity_all)
  
  return(infidelity_mean)
}
