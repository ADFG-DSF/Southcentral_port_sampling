#Sloppy code required objects from multinomial demo (homer).R

mod_fyl
logLik(mod_fyl)

#format data a matrix
dat_ll <-
  dat %>% 
  dplyr::select(year, fleet, area, count) %>%
  tidyr::spread(area, count) %>%
  cbind(predict(mod_fyl, expand.grid(fleet = c("Charter", "Private"), year = 2003:2017), type = "probs"))
ll <- rep(NA, dim(dat_ll)[1])
for(i in 1:30) {ll[i] <- dmultinom(as.matrix(dat_ll[i, 3:6]), prob = as.matrix(dat_ll[i, 7:10]), log = TRUE)}
sum(ll)

#but a not weighted model call gives the same result
mod_test <- nnet::multinom(as.matrix(dat_ll[3:6]) ~ dat_ll$year + dat_ll$fleet)
mod_test
logLik(mod_test)

#for at individual observations
y <- mapply(function(x, y) rep(x + 1, each = y), dat$y, dat$count) %>% do.call(c, .)
y2 <- matrix(c(0,0,0,0), nrow = length(y), ncol = 4)
for(i in 1:4853) (y2[i, y[i]] <- 1)
year <- mapply(function(x, y) rep(x, each = y), dat$year, dat$count) %>% do.call(c, .)
fleet <- mapply(function(x, y) rep(x, each = y), dat$fleet, dat$count) %>% do.call(c, .)
#this model call also matches
mod_test2 <- nnet::multinom(y ~ year + fleet)
mod_test2
logLik(mod_test2)
#and get the correct log liklihood
ll2 <- rep(NA, length(y))
for(i in 1:length(y)) {ll2[i] <- dmultinom(y2[i, ], prob = mod_test2$fitted.values[i, ], log = TRUE)}
sum(ll2)



b = data.frame(
intercept = post$mean$alpha,
fleet = if(!is.null(post$mean$beta)){t(post$mean$beta)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
year = if(!is.null(post$mean$epsilon)){post$mean$epsilon} else rep(0, length(post$mean$alpha))) %>%
as.matrix() %>%
t()
b
dat <- rbind(jags_dat[[4]]$count[,1,], jags_dat[[4]]$count[,2,])
x = data.frame(
area = 1,
charter = rep(1:0, each = length(unique(data_full$yearc))),
private = rep(0:1, each = length(unique(data_full$yearc))),
yearc = rep(min(data_full$yearc):max(data_full$yearc), times = 2)) %>%
dplyr::arrange(area, private, yearc) %>%
as.matrix()
p <- exp(x%*%b)/apply(exp(x%*%b), 1, sum)

ll <- rep(NA, dim(x)[1])
for(i in 1:dim(x)[1]) {ll[i] <- dmultinom(dat[i, ], prob = p[i,], log = TRUE)}
sum(ll)




















