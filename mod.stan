functions {
  // Define log probability density function for an interval censored normal
  real intervalCensoredNormal_lpmf(int y, real mu, real sigma, vector breaks) {
    real p;
    if (y == 0) { // from -inf to the first break
      p = normal_lcdf(breaks[1] | mu, sigma);
    } else if (y == rows(breaks)) { // from the last break to +inf
      p = normal_lccdf(breaks[rows(breaks)] | mu, sigma);
    } else { // between two breaks
      p = log_diff_exp(normal_lcdf(breaks[y+1] | mu, sigma), normal_lcdf(breaks[y] | mu, sigma));
    }
    return p;
  }
}

data {
  int<lower = 0> T; // number of month
  int<lower = 0> n; // number of obs
  int time[n]; // observed month col
  int pop[n]; // observed population col
  int y[n]; // observed location wrt the breaks
  int<lower = 0> k; 
  vector[k] intervalBreaks; // a vector with k elements
}

parameters {
  matrix[T, 2] alpha; 
  matrix[T, 2] beta0; 
  matrix[T, 2] beta1;
  vector<lower = 0>[2] sigma0;
  vector<lower = 0>[2] sigma1;
  
  vector[2] theta;
  vector<lower = 0>[2] tau;
  corr_matrix[2] eta;
  
  ordered[2] mua;
  vector<lower = 0>[2] gamma0;
  corr_matrix[2] rho0;
  
  ordered[2] mub;
  vector<lower = 0>[2] gamma1;
  corr_matrix[2] rho1;
}

transformed parameters {
  vector[2] mu0 = [mua[1], mub[1]]';
  vector[2] mu1 = [mua[2], mub[2]]';
  
  matrix[T, 2] prop;
  for (i in 1:T) {
    for (j in 1:2) {
      prop[i, j] = inv_logit(alpha[i, j]);
    }
  }
}

model {
  mua ~ normal(0, 10); 
  mub ~ normal(0, 10); 
  theta ~ normal(0, 10);
  
  gamma0 ~ cauchy(0, 2); 
  gamma1 ~ cauchy(0, 2); 
  tau ~ cauchy(0, 2);
  
  rho0 ~ lkj_corr(0.001); 
  rho1 ~ lkj_corr(0.001); 
  eta ~ lkj_corr(0.001);
  
  for (r in 1:T) {
    beta0[r] ~ multi_normal(mu0, quad_form_diag(rho0, gamma0));
    beta1[r] ~ multi_normal(mu1, quad_form_diag(rho1, gamma1));
    alpha[r] ~ multi_normal(theta, quad_form_diag(eta, tau));
  }
  
  for (i in 1:n) {
    int t; 
    int s; 
    real p;
    
    t = time[i]; 
    s = pop[i]; 
    p = prop[t, s];
    
    target += log_mix(1-p,
                     intervalCensoredNormal_lpmf(y[i] | beta0[t, s], sigma0[s], intervalBreaks),
                     intervalCensoredNormal_lpmf(y[i] | beta1[t, s], sigma1[s], intervalBreaks));
  }
}


