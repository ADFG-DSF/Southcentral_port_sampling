## Make Jags dataset to effort composition estimates
##
make_jagsdat <- function(dat, port, stat){
  data <- dat[dat$port == port, c("year", "fleet", "area", stat)] %>%
    dplyr::mutate(yearc = year - median(unique(year)))
  data_full <- expand.grid(list(yearc = unique(data$yearc), 
                                fleet = unique(data$fleet), 
                                area = unique(data$area))) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::arrange(area, fleet, yearc) %>%
    dplyr::mutate(value = ifelse(is.na(E), 0, E))
  
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

## Plot composition data and estimates
##
plot_post <- function(post, dat, plotport, inc_re = FALSE){
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
    re_mat <- matrix(NA, nrow = prod(dim(post_re[,,1])), ncol = dim(post_re)[3])
    for(i in 1:dim(post_re)[3]){re_mat[,i] <- c(post_re[,1,i], post_re[,2,i])}
    re_mat
  }
  
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
  
  out <- data_full %>%
    ggplot(aes(x = yearc, weight = E, fill = area)) +
    geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
    geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
    facet_grid(. ~ fleet) +
    scale_y_continuous(name = "Percent") +
    scale_x_continuous(name = "Year", breaks = seq(min(data$yearc), max(data$yearc), 3), labels = seq(min(data$year), max(data$year), 3)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
  
  if(inc_re == FALSE){return(out)}
    else{return(out + geom_line(data = p2, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, linetype = 1))}
}
##

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
