## Make Jags dataset for composition estimates
##
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
##
make_jagsdatpred <- function(dat, port, stat, yr){
  dat$yearc <- dat$year - median(unique(dat$year))
  temp <- dat[dat$port == port & dat$year != yr, c("yearc", "fleet", "area", stat)] %>%
    dplyr::arrange(yearc, fleet, area)
  train <- temp %>%
    dplyr::group_by(yearc, fleet, area) %>%
    dplyr::summarise(lH = sum(!!dplyr::sym(stat)))
  train_full <- expand.grid(list(yearc = unique(train$yearc), 
                                 fleet = unique(train$fleet), 
                                 area = unique(train$area))) %>%
    dplyr::left_join(train, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(value = ifelse(is.na(!!dplyr::sym(stat)), 0, !!dplyr::sym(stat)))
  
  count = array(train_full$value,
                dim = c(length(unique(train_full$yearc)),
                        2,
                        length(unique(train_full$area))))
  
  test <- 
    dat[dat$port == port & dat$year == yr, c("yearc", "fleet", "area", stat)] %>%
    dplyr::arrange(yearc, fleet, area)
  pred_yr <- test$yearc
  pred_fleet <- ifelse(test$fleet == "Charter", 0, 1)
  pred_area <- test$area
  pred_n <- dim(test)[1]
  
  list(
    count = count,
    yearc = unique(train$yearc),
    A = length(unique(train$area)),
    Y = length(unique(train$yearc)),
    M = apply(count, c(1,2), sum),
    pred_n = pred_n,
    pred_fleet = pred_fleet,
    pred_year = pred_yr,
    pred_area = pred_area)
}

## Plot composition data and estimates
##
plot_post <- function(post, dat, stat, plotport, inc_pred = "mean"){
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
    ggplot(aes(x = yearc, weight = value, fill = area)) +
    geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
    facet_grid(. ~ fleet) +
    scale_y_continuous(name = "Percent") +
    scale_x_continuous(name = "Year", breaks = seq(min(data$yearc), max(data$yearc), 3), labels = seq(min(data$year), max(data$year), 3)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
  
  if(inc_pred == "none"){return(out)}
  else{if(inc_pred == "mean"){
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
    
    return(out + geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1))}
    else{
      b <- 
        data.frame(
          intercept = post$mean$alpha,
          fleet = if(!is.null(post$mean$beta)){t(post$mean$beta)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
          year = if(!is.null(post$mean$epsilon)){post$mean$epsilon} else rep(0, length(post$mean$alpha)),
          year = if(!is.null(post$mean$gamma)){t(post$mean$gamma)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2)) %>%
        as.matrix() %>%
        t()
      
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
      return(out + geom_line(data = p2, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, linetype = 1))}}
}

## Multinomial probabilities from posterior
p_post <- function(post, dat, plotport){
  data <- dat[dat$port == plotport, c("year", "fleet", "area", "E")] %>%
    dplyr::arrange(area, fleet, year) %>%
    dplyr::mutate(yearc = year - median(year))
  data_full <- expand.grid(list(yearc = unique(data$yearc), 
                                fleet = unique(data$fleet), 
                                area = unique(data$area),
                                value = 0)) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(E = ifelse(is.na(E), value, E),
                  area = factor(area, levels = unique(data$area)))
  
  x <- 
    data.frame(
      area = 1,
      charter = rep(1:0, each = length(unique(data_full$yearc))),
      private = rep(0:1, each = length(unique(data_full$yearc))),
      yearc = rep(min(data_full$yearc):max(data_full$yearc), times = 2),
      yearccharter = c(min(data_full$yearc):max(data_full$yearc), rep(0, length(unique(data_full$yearc)))),
      yearcprivate = c(rep(0, length(unique(data_full$yearc))), min(data_full$yearc):max(data_full$yearc))) %>%
    dplyr::arrange(area, private, yearc) %>%
    as.matrix()
  
  b <- 
    data.frame(
      intercept = post$mean$alpha,
      fleet = if(!is.null(post$mean$beta)){t(post$mean$beta)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
      year = if(!is.null(post$mean$epsilon)){post$mean$epsilon} else rep(0, length(post$mean$alpha)),
      year = if(!is.null(post$mean$gamma)){t(post$mean$gamma)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2)) %>%
    as.matrix() %>%
    t()
  
  re <- function(post_re){
    re_mat <- matrix(NA, nrow = prod(dim(post_re)[1:2]), ncol = dim(post_re)[3])
    for(i in 1:dim(post_re)[3]){re_mat[,i] <- c(post_re[,1,i], post_re[,2,i])}
    re_mat
  }
  
  p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(unique(data$area));
  # p$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  # p$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
  
  p2 <- as.data.frame(exp(x%*%b + re(post$mean$re))/apply(exp(x%*%b + re(post$mean$re)), 1, sum)) %>% setNames(unique(data$area));
  # p2$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  # p2$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
  
  list(p = p, p_re = p2)
}

## Manaul calculation of deviance wo re
get_deviance <- function(dat, preds){
  ll <- rep(NA, dim(preds[[1]])[1])
  for(i in 1:dim(preds[[1]])[1]) {ll[i] <- dmultinom(dat[i, ], 
                                                     prob = as.matrix(preds[[1]][i,]), 
                                                     log = TRUE)}
  -2*sum(ll)
}

#Summarize Interview data
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
