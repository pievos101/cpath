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
  
  paths_agg_tmp <- data.frame(cbind(paths, swapped_fraction))
  colnames(paths_agg_tmp)[1:k] <- paste0("v", 1:k, "_var")
  colnames(paths_agg_tmp)[(k+1):(2*k)] <- paste0("v", 1:k, "_sf")
  
  diffs <- cbind(swapped_fraction[,1], t(apply(as.matrix(swapped_fraction), 1, diff)))
  paths_diff_tmp <- data.frame(cbind(paths, diffs))
  cpath_delta <- data.frame()
  for (i in 1:k){
    tmp_res <- aggregate(paths_diff_tmp[,i+k] ~ paths_diff_tmp[,i], paths_diff_tmp, mean)
    colnames(tmp_res) <- c("var", "sf_delta")
    tmp_res$node <- paste0("v", i)
    cpath_delta <- rbind(cpath_delta, tmp_res)
  }
  
  summaries <- vector("list", k)
  cols <- colnames(paths_agg_tmp) 
  for (i in 1:k){
    summaries[[i]] <- paths_agg_tmp %>% 
      group_by_at(cols[1:i]) %>% 
      summarise(sf = mean(get(cols[i+k])), count = n())
    colnames(summaries[[i]]) <- c(paste0("v", 1:i, "_var"), paste0("v", i, c("_sf", "_count")))
  }
  
  cpath_agg <- summaries %>% 
    Reduce(function(dtf1,dtf2) right_join(dtf1,dtf2, multiple="all"), .)
  
  cpath_agg <- data.frame(cpath_agg)
  
  cpath_agg$count <- cpath_agg[,paste0("v", k, "_count")] 
  cpath_agg <- cpath_agg[,c(paste0("v", 1:k, "_var"), 
                                    paste0("v", 1:k, "_sf"), 
                                    paste0("v", 1:k, "_count"), 
                                    "count")]
  cpath_agg$max_sf <- apply(cpath_agg[,(k+1):(2*k)], 1, max, na.rm=T)
  cpath_agg <- data.frame(cpath_agg)
  list("cpath_agg" = cpath_agg, "cpath_delta" = cpath_delta)
}

#'@export
plot_paths <- function(cpath_summary, n_paths = 50, min_length = 2, 
                       count_threshold = 0, column_names=NULL){
  cpath_agg <- cpath_summary$cpath_agg
  k <- (ncol(cpath_agg) - 2)/3
  cpath_agg$name <- apply(cpath_agg, 1, name_path, k=k)
  cpath_plot <- cpath_agg[complete.cases(cpath_agg[,1:min_length]),]
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
    labs(x = "Node", y="Average swapped labels fraction") +
    theme_minimal() 
  
  if (is.null(column_names)){
    p <- p + scale_color_brewer(name = "Variable", type="qual", palette="Set1")
  } else {
    p <- p + scale_color_brewer(name = "Variable", type="qual", palette="Set1",
                                labels = column_names) 
  }
  p
}

#'@export
plot_deltas <- function(cpath_summary, column_names=NULL){
  cpath_delta <- cpath_summary$cpath_delta
  cpath_delta$v <- as.factor(cpath_delta$v)
  cpath_delta$var <-  as.factor(cpath_delta$var)
  p <- ggplot(cpath_delta, aes(x = var, y = sf_delta, fill = node)) +
    geom_bar(stat="identity", width = 0.8, position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(name = "Node", palette = "Set1") + 
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() + 
    labs(x = "Variable", y = "Average change of swapped labels fraction")
  if (!is.null(column_names)){
    p <- p + scale_x_discrete(labels = column_names)
  }
  p
} 

