#'@import dplyr
#'@import tidyr
#'@import ggplot2

name_path <- function(path_row, k){
  paste(path_row[1:k][!is.na(path_row[1:k])], collapse = "-")
}

#'@export
get_cpath_summary <- function(cpaths, only_counterfactual=TRUE){
  if (only_counterfactual){
    paths <- cpaths$paths[cpaths$counterfactuality,]
    swapped_fraction <- cpaths$swapped_fractions[cpaths$counterfactuality,]
  } else{
    paths <- cpaths$paths
    swapped_fraction <- cpaths$swapped_fractions
  }
  k <- ncol(paths)
  
  paths_df <- data.frame(cbind(paths, swapped_fraction))
  colnames(paths_df)[1:k] <- paste0("v", 1:k, "_var")
  colnames(paths_df)[(k+1):(2*k)] <- paste0("v", 1:k, "_sf")
  
  summaries <- vector("list", k)
  cols <- colnames(paths_df) 
  
  for (i in 1:k){
    summaries[[i]] <- paths_df %>% 
      group_by_at(cols[1:i]) %>% 
      summarise(sf = mean(get(cols[i+k])), count = n())
    colnames(summaries[[i]]) <- c(paste0("v", 1:i, "_var"), paste0("v", i, c("_sf", "_count")))
  }
  
  
  cpath_summary <- summaries %>% 
    Reduce(function(dtf1,dtf2) right_join(dtf1,dtf2, multiple="all"), .)
  
  cpath_summary <- data.frame(cpath_summary)
  
  cpath_summary$count <- cpath_summary[,paste0("v", k, "_count")] 
  
  cpath_summary <- cpath_summary[,c(paste0("v", 1:k, "_var"), 
                                    paste0("v", 1:k, "_sf"), 
                                    paste0("v", 1:k, "_count"), 
                                    "count")]
  
  
  
  cpath_summary$name <- apply(cpath_summary, 1, name_path, k=k)
  cpath_summary$max_sf <- apply(cpath_summary[,(k+1):(2*k)], 1, max, na.rm=T)
  cpath_summary <- data.frame(cpath_summary)
  cpath_summary
}

#'@export
plot_paths <- function(cpath_summary, n_paths = 50, min_length = 2, 
                       count_threshold = 0, column_names=NULL){
  cpath_plot <- cpath_summary[complete.cases(cpath_summary[,1:min_length]),]
  cpath_plot <- cpath_plot %>%
    filter(count >= count_threshold) %>% 
    select(-count) %>% 
    top_n(n_paths, max_sf) %>%
    pivot_longer(cols = starts_with("v"),
                 names_to = c("v", ".value"),
                 names_sep="_",
    ) %>%
    na.omit()
  cpath_plot$start_name <- substr(cpath_plot$name, 1, 1)
  p <- ggplot(cpath_plot, aes(x = v, y = sf)) +
    geom_line(aes(group=name, size=count), color="darkgrey") +
    geom_point(aes(color=factor(var))) +
    facet_wrap(~start_name) +
    scale_size_continuous(name = "Path count", range = c(0.25, 1.5)) +
    labs(x = "Node", y="Average fraction of swapped labels") +
    theme_minimal() 
  
  if (is.null(column_names)){
    p <- p + scale_color_brewer(name = "Variable", type="qual", palette="Set1")
  } else {
    p <- p + scale_color_brewer(name = "Variable", type="qual", palette="Set1",
                                labels = column_names) 
  }
  p
}





