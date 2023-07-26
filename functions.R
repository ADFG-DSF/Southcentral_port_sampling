## Make Jags dataset for composition estimates
# arguments are:
#  a formatted version of the int_boat.rds
#  the port of interest
#  the statistic of interest.
# the output is:
#  an array formatted: [years, angler type, area] for stat given
#  a centered year variable
#  the total number of areas
#  the total number of years
#  the total count for each year and angler type across areas 
make_jagsdat <- function(dat, port, stat){
  data <- dat[dat$port == port, c("year", "fleet", "area", stat)] %>%
    dplyr::mutate(yearc = year - median(unique(year)))
  data_full <- expand.grid(list(yearc = unique(data$yearc), 
                                fleet = unique(data$fleet), 
                                area = unique(data$area))) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::arrange(area, fleet, yearc) %>%
    dplyr::mutate(value = ifelse(is.na(!!dplyr::sym(stat)), 0, !!dplyr::sym(stat)))
  
  count = array(data_full$value,
                dim = c(length(unique(data_full$yearc)),
                        2,
                        length(unique(data_full$area))))
  
  list(
    count = count,
    yearc = unique(data_full$yearc),
    A = length(unique(data_full$area)),
    Y = length(unique(data_full$yearc)),
    M = apply(count, c(1,2), sum))
}
##

## Make Jags dataset for composition estimates model selection
# similar to above except argument yr is the year to be in the testing dataset.
make_jagsdatpred <- function(dat, port, stat, yr){
  loo_yearc <- yr - median(unique(dat$year[dat$port == port]))
  temp <- dat[dat$port == port, c("year", "fleet", "area", stat)] %>%
    dplyr::mutate(yearc = year - median(unique(year)))
  temp_full <- expand.grid(list(yearc = unique(temp$yearc),
                                 fleet = unique(temp$fleet),
                                 area = unique(temp$area))) %>%
    dplyr::left_join(temp, by = c("yearc", "fleet", "area")) %>%
    dplyr::arrange(area, fleet, yearc) %>%
    dplyr::mutate(value = ifelse(is.na(!!dplyr::sym(stat)), 0, !!dplyr::sym(stat)))
  
  train <- temp_full[!(temp_full$yearc %in% loo_yearc),]
  count = array(train$value,
                dim = c(length(unique(train$yearc)),
                        2,
                        length(unique(train$area))))
  
  test <- temp_full[temp_full$yearc %in% loo_yearc,]
  pred_yr <- test$yearc
  pred_fleet <- ifelse(test$fleet == "Charter", 0, 1)
  lo_count <- matrix(test$value,
                     nrow = 2, 
                     ncol = length(unique(test$area)))
  pred_n <- dim(test)[1]
  
  list(
    count = count,
    yearc = unique(train$yearc),
    A = length(unique(train$area)),
    Y = length(unique(train$yearc)),
    M = apply(count, c(1,2), sum),
    pred_M = apply(lo_count, 1, sum),
    pred_fleet = c(0, 1),
    pred_year = rep(loo_yearc, 2),
    lo_count = lo_count)
}


## Plot composition data and estimates
##
plot_post <- function(post, dat, stat, plotport, inc_pred = "mean", bystock = TRUE, charteronly = FALSE, title = NULL){
  stopifnot(inc_pred %in% c("none", "mean", "re"))
  data <- dat[dat$port == plotport, c("year", "fleet", "area", stat)] %>%
    dplyr::arrange(area, fleet, year) %>%
    dplyr::mutate(yearc = year - median(year))
  data_full <- expand.grid(list(yearc = unique(data$yearc), 
                                fleet = unique(data$fleet), 
                                area = unique(data$area),
                                value = 0)) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(value = ifelse(is.na(!!dplyr::sym(stat)), value, !!dplyr::sym(stat)),
                  area = factor(area, levels = unique(data$area)))
  
  x <- 
    data.frame(
      area = 1,
      charter = rep(1:0, each = length(min(data_full$yearc):max(data_full$yearc))),
      private = rep(0:1, each = length(min(data_full$yearc):max(data_full$yearc))),
      yearc = rep(min(data_full$yearc):max(data_full$yearc), times = 2),
      yearccharter = c(min(data_full$yearc):max(data_full$yearc), rep(0, length(min(data_full$yearc):max(data_full$yearc)))),
      yearcprivate = c(rep(0, length(min(data_full$yearc):max(data_full$yearc))), min(data_full$yearc):max(data_full$yearc))) %>%
      dplyr::arrange(area, private, yearc) %>%
      as.matrix()
  
  out <- data_full %>%
    ggplot(aes(x = yearc, weight = value, fill = area, group = area)) +
    geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
    scale_y_continuous(name = "Percent") +
    scale_x_continuous(name = "Year", breaks = seq(min(data$yearc), max(data$yearc), 4), labels = seq(min(data$year), max(data$year), 4)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = NULL, ncol = 3)) +
    ggtitle(title)
  if(bystock == TRUE){out <- out + facet_grid(. ~ fleet)}
  
  if(inc_pred %in% c("mean", "re")){
    b <- 
      data.frame(
        intercept = post$mean$alpha,
        fleet = if(!is.null(post$mean$beta)){t(post$mean$beta)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
        year = if(!is.null(post$mean$epsilon)){post$mean$epsilon} else rep(0, length(post$mean$alpha)),
        year = if(!is.null(post$mean$gamma)){t(post$mean$gamma)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2)) %>%
      as.matrix() %>%
      t()
    
    p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(unique(data$area));
    p$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
    p$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
    
    p <- 
      tidyr::pivot_longer(p, -c(fleet, yearc), names_to = "area") %>%
      dplyr::mutate(area = factor(area, levels = rev(unique(data$area)))) %>%
      dplyr::arrange(fleet, yearc, area) %>% 
      dplyr::group_by(fleet, yearc) %>% 
      dplyr::mutate(pct = cumsum(value))
    p$area <- factor(p$area, levels = unique(data$area))
    if(charteronly == TRUE){p <- p[p$fleet == "Charter", ]}
  }
  if(inc_pred == "re"){    
    re <- function(post_re){
      re_mat <- matrix(NA, nrow = prod(dim(post_re[,,1])), ncol = dim(post_re)[3])
      for(i in 1:dim(post_re)[3]){re_mat[,i] <- c(post_re[,1,i], post_re[,2,i])}
      re_mat
    }
    
    p2 <- as.data.frame(exp(x%*%b + re(post$q50$re))/apply(exp(x%*%b + re(post$q50$re)), 1, sum)) %>% setNames(unique(data$area));
    p2$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
    p2$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
    
    p2 <-
      tidyr::pivot_longer(p2, -c(fleet, yearc), names_to = "area") %>%
      dplyr::mutate(area = factor(area, levels = rev(unique(data$area)))) %>%
      dplyr::arrange(fleet, yearc, area) %>%
      dplyr::group_by(fleet, yearc) %>%
      dplyr::mutate(pct = cumsum(value))
    p2$area <- factor(p2$area, levels = unique(data$area))
    if(charteronly == TRUE){p2 <- p2[p2$fleet == "Charter", ]}
  }
  if(inc_pred == "none"){return(out)}
  else{if(inc_pred == "mean"){return(out + 
                                    geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
                                    guides(color = guide_legend(title = NULL, ncol = 3)))}
    else{return(out + 
                  geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) + 
                  geom_line(data = p2, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, linetype = 1) +
                  guides(color = guide_legend(title = NULL, ncol = 3)))}}
}
#I think this is obsolete but i'm not sure so I commented out for the time being.
# ## Multinomial probabilities from posterior
# p_post <- function(post, dat, stat, plotport){
#   data <- dat[dat$port == plotport, c("year", "fleet", "area", stat)] %>%
#     dplyr::arrange(area, fleet, year) %>%
#     dplyr::mutate(yearc = year - median(year))
#   data_full <- expand.grid(list(yearc = unique(data$yearc), 
#                                 fleet = unique(data$fleet), 
#                                 area = unique(data$area),
#                                 value = 0)) %>%
#     dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
#     dplyr::mutate(value = ifelse(is.na(!!dplyr::sym(stat)), value, !!dplyr::sym(stat)),
#                   area = factor(area, levels = unique(data$area)))
#   
#   x <- 
#     data.frame(
#       area = 1,
#       charter = rep(1:0, each = length(unique(data_full$yearc))),
#       private = rep(0:1, each = length(unique(data_full$yearc))),
#       yearc = rep(min(data_full$yearc):max(data_full$yearc), times = 2),
#       yearccharter = c(min(data_full$yearc):max(data_full$yearc), rep(0, length(unique(data_full$yearc)))),
#       yearcprivate = c(rep(0, length(unique(data_full$yearc))), min(data_full$yearc):max(data_full$yearc))) %>%
#     dplyr::arrange(area, private, yearc) %>%
#     as.matrix()
#   
#   b <- 
#     data.frame(
#       intercept = post$mean$alpha,
#       fleet = if(!is.null(post$mean$beta)){t(post$mean$beta)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
#       year = if(!is.null(post$mean$epsilon)){post$mean$epsilon} else rep(0, length(post$mean$alpha)),
#       year = if(!is.null(post$mean$gamma)){t(post$mean$gamma)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2)) %>%
#     as.matrix() %>%
#     t()
#   
#   p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(unique(data$area));
#   p$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
#   p$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2) + median(dat[dat$port == plotport, c("year", "fleet", "area", stat)]$year)
#   
#   p
# }

## Multinomial probabilities from posterior
# arguments are:
#  the posterior output for the port and statistic (effort, pelagic harvest, or non-pelagic harvest) of interest
#  user groups to induce (default is both but can specify only "Charter" for the few ports w insufficient private sample size)
#  years to include (0 or NULL). Use 0 to specify a constant mean model and NULL to give all years
#  Areas included for the port modeled in this posterior
# the output is a dataframe with the estimated mean proportion of harvest or effort for each area fished out of the port. 
p_post <- function(post, fleets = c("charter", "private"), yrs, areas){
  stopifnot(yrs %in% c(NULL, 0))
  if(is.null(yrs)) yrsc = post$data$yearc else yrsc = yrs
  #lookup to convert centered years back to calander years.
  yr_median <- list("1" = NA, "20" = 2009.5, "22" = 2008.5, "24" = 2007.5)
  x0 <- 
    expand.grid(list(yrsc = yrsc, fleets = fleets)) %>%
    dplyr:: mutate(area = 1,
                   charter = ifelse(fleets == 'charter', 1, 0),
                   private = ifelse(fleets == 'private', 1, 0),
                   yearc = yrsc,
                   yearccharter = ifelse(fleets == 'charter', yrsc, 0),
                   yearcprivate = ifelse(fleets == 'private', yrsc, 0))
  x <- x0 %>%
    dplyr::select(-yrsc, -fleets) %>%
    as.matrix()
  
  b <- array(NA, dim= c(dim(post$sims.list$alpha)[1], 6, dim(post$mean$alpha)))
  p <- array(NA, dim = c(dim(post$sims.list$alpha)[1], dim(x)[1], dim(post$mean$alpha)))
  for(i in 1:dim(post$sims.list$alpha)[1]){
    b[i, ,] <- 
      data.frame(
        intercept = post$sims.list$alpha[i,],
        fleet = if(!is.null(post$sims.list$beta[i,,])){t(post$sims.list$beta[i,,])} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
        year = if(!is.null(post$sims.list$epsilon[i,])){post$sims.list$epsilon[i,]} else rep(0, length(post$mean$alpha)),
        year = if(!is.null(post$sims.list$gamma[i,,])){t(post$sims.list$gamma[i,,])} else matrix(0, nrow = length(post$mean$alpha), ncol = 2)) %>%
      as.matrix() %>% 
      t()
    p[i, ,] <- exp(x%*%b[i, ,])/apply(exp(x%*%b[i, ,]), 1, sum)
  }
  out <- 
    paste0(format(round(apply(p, c(2,3), mean), 2)),
           " (",
           format(round(apply(p, c(2,3), quantile, probs = 0.05), 2)),
           "-",
           format(round(apply(p, c(2,3), quantile, probs = 0.95), 2)),
           ")" ) %>%
    matrix(nrow = dim(x)[1], ncol = dim(post$mean$alpha), byrow = FALSE) %>%
    as.data.frame() %>%
    dplyr::mutate(fleet = ifelse(x0$charter == 1, "Charter", "Private"),
                  year =  x0$yearc + yr_median[[as.character(length(yrsc))]])
  names(out) <- c(areas, "fleet", "year")
  out[, c("fleet", "year", areas)]
}

#Summarize Interview data
# arguments are:
#  int_boat.rds
#  the statistic of interest from int_boat.rds.
# the output is a hinge type plot that shows the distribution of reported harvest, effort and cpue by species targeted for each user group
plot_int <- function(dat, Hvar){
  plottext <- 
    expand.grid(target = levels(int_boat$target),
                fleet = c("Charter", "Private"),
                name = c("H", "E", "cpue"))
  plottext$name <- 
    factor(plottext$name, 
           levels = c("H", "E", "cpue"),
           labels = c("Harvest", "Effort", "CPUE"))
  plottext$n <- 
    c(format(as.vector(table(dat$target[!is.na(dat[Hvar])], dat$fleet[!is.na(dat[Hvar])])), trim = TRUE, big.mark = ","),
      rep(format(as.vector(table(dat$target[!is.na(dat$E)], dat$fleet[!is.na(dat$E)])), trim = TRUE, big.mark = ","), 2))
  out <- 
    dat %>%
    dplyr::mutate(cpue = !!dplyr::sym(Hvar)/E) %>%
    dplyr::select(fleet, target, !!dplyr::sym(Hvar), E, cpue) %>%
    dplyr::rename(H = !!dplyr::sym(Hvar)) %>%
    tidyr::pivot_longer(c(H, E, cpue)) %>%
    dplyr::mutate(name = factor(name, 
                                levels = c("H", "E", "cpue"),
                                labels = c("Harvest", "Effort", "CPUE"))) %>%
    ggplot(aes(x = target, y = value)) +
    geom_boxplot() + #varwidth = TRUE
    geom_text(data = plottext, aes(target, Inf, label = n), vjust = 1) +
    #  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
    facet_grid(name ~ fleet, scales = "free_y") +
    scale_x_discrete(name = "Target Species") +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom", axis.text.x=element_text(angle=30,hjust=1))
  out
}

tab_data <- function(yr_range, jag_dat, areas){
  temp <- data.frame(fleet = rep(c("Charter", "Private"), each = length(yr_range)),
                     year = as.character(rep(yr_range, times = 2)),
                     count = rbind(jag_dat$count[,1,], jag_dat$count[,2,]),
                     total = c(jag_dat$M[,1], jag_dat$M[,2]))
  names(temp) <- c("Fleet", "Year", areas, "Total")
  temp
}

#Evaluate cross-validated predictions using the log-likelihood for the left our observations given the modeled probabilities
#sloppy because these models predict poorly
#Use median prob to correct for highly skewed posterior predictions
#Take log first to allow for very low probability events
#since using median is same as taking log after (for odd numbers anyway)
get_llpred <- function(pred, obs){
  ll <- matrix(NA,
               nrow = dim(pred$sims.list$q_pred)[1],
               ncol = dim(pred$sims.list$q_pred)[2])
  #  sum_ll <- vector(mode = "numeric", length = dim(pred$sims.list$q_pred)[1])
  for(i in 1:dim(pred$sims.list$q_pred)[1]){
    ll[i, 1] <- dmultinom(obs$lo_count[1, ], prob = pred$sims.list$q_pred[i, 1, ], log = TRUE)
    ll[i, 2] <- dmultinom(obs$lo_count[2, ], prob = pred$sims.list$q_pred[i, 2, ], log = TRUE)
  }
  apply(ll, 2, median)
}

get_llpred_Charter <- function(pred, obs){
  ll <- vector("numeric", length = dim(pred$sims.list$q_pred)[1])
  for(i in 1:dim(pred$sims.list$q_pred)[1]){
    ll[i] <- dmultinom(obs$lo_count[1, ], prob = pred$sims.list$q_pred[i, ], log = TRUE)
  }
  median(ll)
}

#make a table of model selection criteria
tab_ll <- 
  function(ll){
    lg = length(ll)
    sums <- -sapply(ll, sum)
    mu_diffs <- sums - min(sums)
    diffs <- vector(mode = "list", length = lg)
    for(i in 1:lg){diffs[[i]] <- -ll[[i]] - -ll[[which(mu_diffs == 0)]]}
    se_diffs <- sapply(diffs, function(y) sqrt(prod(dim(y))*var(as.vector(y))))
    boot_diff <- lapply(diffs, function(x){replicate(2000, sum(sample(as.vector(x), length(as.vector(x)), TRUE)))})
    prob_diff <- sapply(boot_diff, function(x) mean(x > 0))
    data.frame(ll = format(round(sums), trim = TRUE, big.mark = ","),
               diff = paste0(format(round(mu_diffs), trim = TRUE, big.mark = ","),
                             " (",
                             format(round(se_diffs), trim = TRUE, big.mark = ","),
                             ")"),
               p_diff = format(round(prob_diff, 3), nsmall = 3, trim = TRUE)) %>%
      dplyr::mutate(model = if(lg == 2){factor(c("constant mean", "trending mean"),
                                               levels = c("constant mean", "trending mean", "shared mean", "unique mean", "unique mean, shared trend", "unique mean and trend"),
                                               ordered = TRUE)}
                    else{factor(c("shared mean", "unique mean", "unique mean, shared trend", "unique mean and trend"),
                                levels = c("constant mean", "trending mean", "shared mean", "unique mean", "unique mean, shared trend", "unique mean and trend"),
                                ordered = TRUE)}) %>%
      dplyr::select(model, ll, diff, p_diff)
  }

# get random effect standard deviation from model posteriors
# This function works but is poorly written and executed. I hope the next person writes it better if they use it at all.
# It is included because overdispersion is ridiculously large and anyone who uses the spatial composition results needs to know that and make limited inference as a result.
get_sd <- function(index, posts, obj_names){
  sapply(posts[[index]], function(x) x$mean$sd) %>%
    as.data.frame() %>%
    setNames("sd") %>%
    dplyr::mutate(model = if(length(posts[[index]]) == 2){factor(c("constant mean", "trending mean"),
                                                                 levels = c("constant mean", "trending mean", "shared mean", "unique mean", "unique mean, shared trend", "unique mean and trend"),
                                                                 ordered = TRUE)} 
                  else{factor(c("shared mean", "unique mean", "unique mean, shared trend", "unique mean and trend"),
                              levels = c("constant mean", "trending mean", "shared mean", "unique mean", "unique mean, shared trend", "unique mean and trend"),
                              ordered = TRUE)},
                  port = gsub(".*_(\\w+)", "\\1", obj_names[[index]]),
                  composition = if(grepl("^postHp", obj_names[[index]])){"Harvest: Pelagic"}
                  else{if(grepl("^postHnp", obj_names[[index]])){"Harvest: Non-Pelagic"}else{"Effort"}},
                  sd = format(round(sd, 2), trim = TRUE))
}
