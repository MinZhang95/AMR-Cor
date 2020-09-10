library(dplyr)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

sero = "Typhimurium" 
anti = "AZI"

pop1 = "human"
pop2 = "animal"

# Data requested
pop1Sub <- read.csv(paste0("./realdata/MIC/", sero, "_", anti, "_", pop1, ".csv"))
pop2Sub <- read.csv(paste0("./realdata/MIC/", sero, "_", anti, "_", pop2, ".csv"))

commonTime <- intersect(unique(pop1Sub$Time), unique(pop2Sub$Time))

dat <- rbind(pop1Sub %>% mutate(Pop=1), pop2Sub %>% mutate(Pop=2)) %>% 
  filter(Time %in% commonTime) %>% 
  mutate(Concl = ifelse(Concl == "S", 1, 2), 
         LogRslt = round(log(Rslt, base = 2), 0), 
         TimePoint = sapply(Time, function(t) which(t==commonTime))) %>%
  dplyr::select(TimePoint, Sign, LogRslt, Concl, Pop)

timeLen <- max(unique(dat$TimePoint))
template <- data.frame(TimePoint = 1:timeLen)

# Initial values 
## alpha
alpha <- dat %>% group_by(Pop) %>% count(TimePoint) %>% 
  left_join(., dat %>% filter(Concl == 2) %>% group_by(Pop) %>% count(TimePoint), 
            by = c("Pop"="Pop", "TimePoint"="TimePoint")) %>% 
  mutate(p = n.y/n.x) %>% 
  dplyr::select(Pop, TimePoint, p) %>% 
  tidyr::spread(., Pop, p) %>% 
  mutate(`1` = replace(`1`, is.na(`1`), 0.00001), 
         `2` = replace(`2`, is.na(`2`), 0.00001)) %>% 
  .[, 2:3] %>% apply(., 2, function(t) log(t/(1-t))) %>% as.matrix()
alpha[which(alpha==Inf)] <- 11.5129155

## beta0
beta0 <- dat %>% filter(Concl == 1) %>% group_by(TimePoint, Pop) %>% 
  summarise(naivem = mean(LogRslt)) %>% tidyr::spread(., Pop, naivem) %>% 
  left_join(template, ., c("TimePoint" = "TimePoint")) %>% 
  .[, 2:3] %>% zoo::na.aggregate(.) %>% as.matrix()

## beta1
if (dat %>% filter(Concl == 2) %>% nrow() > 0) {
  beta1 <- dat %>% filter(Concl == 2) %>% group_by(TimePoint, Pop) %>% 
    summarise(naivem = mean(LogRslt)) %>% tidyr::spread(., Pop, naivem) %>% 
    left_join(template, ., c("TimePoint" = "TimePoint")) %>% 
    .[, 2:3] %>% zoo::na.aggregate(.) %>% as.matrix()
} else {
  beta1 <- matrix(max(dat$LogRslt)+1, nrow = nrow(beta0), ncol = ncol(beta0))
}

## sigma0
sigma0 <- dat %>% group_by(Concl, Pop) %>% mutate(sd = sd(LogRslt)) %>% 
  distinct(sd) %>% mutate(sd = ifelse(sd==0, 1, sd)) %>% filter(Concl==1) %>% 
  pull(sd) %>% as.vector()

## sigma1
if (dat %>% filter(Concl == 2) %>% nrow() > 0) {
  sigma1 <- dat %>% group_by(Concl, Pop) %>% mutate(sd = sd(LogRslt)) %>% 
    distinct(sd) %>% mutate(sd = ifelse(sd==0, 1, sd)) %>% filter(Concl==2) %>% 
    pull(sd) %>% as.vector()
} else {
  sigma1 <- c(1, 1)
}

## theta
theta <- alpha %>% apply(., 2, mean) %>% as.vector()

## tau
tau <- alpha %>% apply(., 2, sd) %>% ifelse(.==0, 1, .) %>% as.vector()

## eta
eta <- matrix(c(1, rep(cor(alpha[, 1], alpha[, 2]) %>% ifelse(is.na(.), 0.1, .), 2), 1), nrow = 2)

## mu0
mu0 <- beta0 %>% apply(., 2, mean) %>% as.vector()

## gamma0
gamma0 <- beta0 %>% apply(., 2, sd) %>% as.vector()

## rho0
rho0 <- matrix(c(1, rep(cor(beta0[, 1], beta0[, 2]), 2), 1), nrow = 2)

## mu1
mu1 <- beta1 %>% apply(., 2, mean) %>% as.vector()

## gamma1
gamma1 <- beta1 %>% apply(., 2, sd) %>% ifelse(.==0, 1, .) %>% as.vector()

## rho1
rho1 <- matrix(c(1, rep(cor(beta1[, 1], beta1[, 2]) %>% ifelse(is.na(.), 0.1, .), 2), 1), nrow = 2)

## mua
mua <- c(mu0[1], mu1[1])
mub <- c(mu0[2], mu1[2])

# Data manipulation
intervalBreaks_from <- ifelse("<=" %in% dat$Sign, min(dat$LogRslt), min(dat$LogRslt)-1)
intervalBreaks_to <- max(dat$LogRslt)
intervalBreaks <- c(intervalBreaks_from:intervalBreaks_to)


feedin <- list(T = dat %>% distinct(TimePoint) %>% nrow(),
               n = nrow(dat),
               y = dat %>% mutate(yInterval = LogRslt - intervalBreaks[1]) %>% 
                 mutate(yInterval = ifelse(Sign == "<=", 0, yInterval), 
                        yInterval = ifelse(Sign == ">" , length(intervalBreaks), yInterval)) %>% pull(yInterval),
               k = length(intervalBreaks),
               intervalBreaks = intervalBreaks,
               time = dat$TimePoint,
               pop = dat$Pop)

initf <- function() {
  list(alpha = alpha, beta0 = beta0, beta1 = beta1,
       sigma0 = sigma0, sigma1 = sigma1, 
       theta = theta, tau = tau, eta = eta, 
       mua = mua, gamma0 = gamma0, rho0 = rho0, 
       mub = mub, gamma1 = gamma1, rho1 = rho1 )
}

res <- stan(
  file = "./mod.stan", 
  data = feedin, 
  init = initf, 
  chains = 3, iter = 20000
)
saveRDS(res, paste0("./realdata/RDS/", sero, "_", anti, "_", pop1, "_", pop2, ".rds"))

res %>% summary() %>% .[["summary"]] %>% round(., 5) %>% 
  write.csv(., paste0("./realdata/EST/", sero, "_", anti, "_", pop1, "_", pop2, ".csv"), 
            row.names = T)
