combine_chains <- function(parameter, list_obj){
        parameter_df = as.data.frame(cbind(
        list_obj[[1]][parameter],
        list_obj[[2]][parameter],
        list_obj[[3]][parameter],
        list_obj[[4]][parameter]
            )
        )

        names(parameter_df) <- c("chain_1", "chain_2", "chain_3", "chain_4")

        return(parameter_df)
    }

 diagnostics <- function(dataframe){
        rhat <- posterior::rhat(as.matrix(dataframe))
        rhat_basic <- posterior::rhat_basic(as.matrix(dataframe))
        ess_bulk <- posterior::ess_bulk(as.matrix(dataframe))
        ess_tail <- posterior::ess_tail(as.matrix(dataframe))
        ess_basic <- posterior::ess_basic(as.matrix(dataframe))

        results = as.data.frame(list(
                           rhat=rhat, 
                           rhat_basic=rhat_basic, 
                           ess_bulk=ess_bulk,
                           ess_tail=ess_tail,
                           ess_basic=ess_basic))

        return(results)
    }

    one_chain <- function(df){
        return(df$chain_1)
    }

# Rank statistics
rank_stats <- function(parameter, draws, prior_parameters) {
        sum(draws[[parameter]] < prior_parameters[[parameter]])
    }
    
weighted_ranks <- function(parameter, draws, prior_parameters, posterior_weights, one_chain){
    unweighted_bool <- as.data.frame(draws[[parameter]] < prior_parameters[[parameter]] )
    unweighted <- as.data.frame(sapply(unweighted_bool, as.numeric)) %>% 
    tibble::rowid_to_column("index") %>% 
    pivot_longer(cols = -c(index), names_to = "chain", values_to = "ranks") 
    
    weighted = posterior_weights %>% 
        inner_join(unweighted, by = c("chain", "index")) %>% 
        mutate(weighted_rank = weights * ranks)
    if(one_chain){
        result <- sum(weighted[weighted["chain"] == "chain_1", ]$weighted_rank)
    } else{
        result <- sum(weighted$weighted_rank)
    }
    return(result)
}