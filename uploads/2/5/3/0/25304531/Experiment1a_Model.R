require(rstan);
require(coda);
require(loo);
n_old_cont=1475;
n_old_disc=1464;
n_sim=2947;
n_new=2945;

n_fold=10;
n_old_cont_onefold=round(n_old_cont/n_fold);
n_old_disc_onefold=round(n_old_disc/n_fold);
n_sim_onefold=round(n_sim/n_fold);
n_new_onefold=round(n_new/n_fold);

n_old_cont_old=687;
n_old_cont_sim=403;
n_old_disc_old=582;
n_old_disc_sim=394;
n_sim_old=957;
n_sim_sim=1057;
n_new_old=159;
n_new_sim=693;

n_old_cont_responses_raw = c(rep(1, n_old_cont_old), rep(2, n_old_cont_sim), rep(3, n_old_cont - n_old_cont_old - n_old_cont_sim));
n_old_cont_responses_shuffle = n_old_cont_responses_raw[sample(1:n_old_cont,n_old_cont, replace=F)];
n_old_disc_responses_raw = c(rep(1, n_old_disc_old), rep(2, n_old_disc_sim), rep(3, n_old_disc - n_old_disc_old - n_old_disc_sim));
n_old_disc_responses_shuffle = n_old_disc_responses_raw[sample(1:n_old_disc,n_old_disc, replace=F)];
n_sim_responses_raw = c(rep(1, n_sim_old), rep(2, n_sim_sim), rep(3, n_sim - n_sim_old - n_sim_sim));
n_sim_responses_shuffle = n_sim_responses_raw[sample(1:n_sim,n_sim, replace=F)];
n_new_responses_raw = c(rep(1, n_new_old), rep(2, n_new_sim), rep(3, n_new - n_new_old - n_new_sim));
n_new_responses_shuffle = n_new_responses_raw[sample(1:n_new,n_new, replace=F)];

n_old_cont_responses_test=matrix(1, nrow=n_fold, ncol=3);
n_old_disc_responses_test=matrix(1, nrow=n_fold, ncol=3);
n_sim_responses_test=matrix(1, nrow=n_fold, ncol=3);
n_new_responses_test=matrix(1, nrow=n_fold, ncol=3);
for (i in 1:n_fold){
	start=(i-1)*n_old_cont_onefold+1;
	end=i*n_old_cont_onefold;
	if (i==n_fold){
		end=n_old_cont;
	}
	n_old_cont_responses_test[i,1]=sum(n_old_cont_responses_shuffle[start:end] == 1);
	n_old_cont_responses_test[i,2]=sum(n_old_cont_responses_shuffle[start:end] == 2);
	n_old_cont_responses_test[i,3]=sum(n_old_cont_responses_shuffle[start:end] == 3);
	
	start=(i-1)*n_old_disc_onefold+1;
	end=i*n_old_disc_onefold;
	if (i==n_fold){
		end=n_old_disc;
	}
	n_old_disc_responses_test[i,1]=sum(n_old_disc_responses_shuffle[start:end] == 1);
	n_old_disc_responses_test[i,2]=sum(n_old_disc_responses_shuffle[start:end] == 2);
	n_old_disc_responses_test[i,3]=sum(n_old_disc_responses_shuffle[start:end] == 3);
	
	start=(i-1)*n_sim_onefold+1;
	end=i*n_sim_onefold;
	if (i==n_fold){
		end=n_sim;
	}
	n_sim_responses_test[i,1]=sum(n_sim_responses_shuffle[start:end] == 1);
	n_sim_responses_test[i,2]=sum(n_sim_responses_shuffle[start:end] == 2);
	n_sim_responses_test[i,3]=sum(n_sim_responses_shuffle[start:end] == 3);

	start=(i-1)*n_new_onefold+1;
	end=i*n_new_onefold;
	if (i==n_fold){
		end=n_new;
	}
	n_new_responses_test[i,1]=sum(n_new_responses_shuffle[start:end] == 1);
	n_new_responses_test[i,2]=sum(n_new_responses_shuffle[start:end] == 2);
	n_new_responses_test[i,3]=sum(n_new_responses_shuffle[start:end] == 3);

}


sdt_data <- list(
	#first, total number of trials in each trial type
     n_old_cont=n_old_cont,
     n_old_disc=n_old_disc,
     n_sim=n_sim,
     n_new=n_new,
     #then, number of different responses for each trial type
     n_old_cont_old=n_old_cont_old,
     n_old_cont_sim=n_old_cont_sim,
     n_old_disc_old=n_old_disc_old,
     n_old_disc_sim=n_old_disc_sim,
     n_sim_old=n_sim_old,
     n_sim_sim=n_sim_sim,
     n_new_old=n_new_old,
     n_new_sim=n_new_sim,
     
     n_old_cont_responses=c(n_old_cont_old, n_old_cont_sim, n_old_cont - n_old_cont_old - n_old_cont_sim),
     n_old_disc_responses=c(n_old_disc_old, n_old_disc_sim, n_old_disc - n_old_disc_old - n_old_disc_sim),
     n_sim_responses=c(n_sim_old, n_sim_sim, n_sim - n_sim_old - n_sim_sim),
     n_new_responses=c(n_new_old, n_new_sim, n_new - n_new_old - n_new_sim),
	
	 #priors
	 lambda1_mu0 = -1,#lambda1 is for old/similar, lambda2 is for similar/new
	 lambda1_sigma0=5, 
	 lambda2_mu0=-0.5, 
	 lambda2_sigma0 = 5,
	 mu_old_cont_mu0= -1,
	 mu_old_disc_mu0= -1,
	 mu_old_sigma0= 5,
	 sigma_old_alpha0=1,
	 sigma_old_beta0=1,
	 mu_sim_mu0= -0.5, 
	 mu_sim_sigma0=5, 
	 sigma_sim_alpha0=1,
	 sigma_sim_beta0=1

)


sdt_model <- "
	data {

		int n_fold;
        int n_old_cont;
     	int n_old_disc;
     	int n_sim;
     	int n_new;
    
     	int n_old_cont_responses[3];
     	int n_old_disc_responses[3];
     	int n_sim_responses[3];
     	int n_new_responses[3];
     	
     	int n_old_cont_responses_test[10,3];
     	int n_old_disc_responses_test[10,3];
     	int n_sim_responses_test[10,3];
     	int n_new_responses_test[10,3];
     		 
	 	real lambda1_mu0;
	 	real<lower=0>  lambda1_sigma0;
	 	real lambda2_mu0;
	 	real<lower=0>  lambda2_sigma0;
	 	real  mu_old_cont_mu0;
	 	real  mu_old_disc_mu0;
	 	real<lower=0>  mu_old_sigma0;
	 	real<lower=0>  sigma_old_alpha0;
	 	real<lower=0>  sigma_old_beta0;
	 	real  mu_sim_mu0;
	 	real<lower=0>  mu_sim_sigma0;
	 	real<lower=0>  sigma_sim_alpha0;
	 	real<lower=0>  sigma_sim_beta0;
	 }
	
	parameters {
        ordered[2] lambda;
        real mu_old_cont;
        real mu_old_disc;
        real<lower=0> sigma_old;
        real mu_sim;
        real<lower=0> sigma_sim;       
	}
	
	transformed parameters {
        real<lower=0> theta_old_cont_old;
        real<lower=0> theta_old_cont_sim;
        real<lower=0> theta_old_disc_old;
        real<lower=0> theta_old_disc_sim;
        real<lower=0> theta_sim_old;
        real<lower=0> theta_sim_sim;
        real<lower=0> theta_new_old;
        real<lower=0> theta_new_sim;
        simplex[3] thetas_old_cont;
        simplex[3] thetas_old_disc;
        simplex[3] thetas_sim;
        simplex[3] thetas_new;
        
        theta_old_cont_old <- Phi((lambda[1]-mu_old_cont)/sigma_old);
        theta_old_cont_sim <- Phi((lambda[2]-mu_old_cont)/sigma_old) - Phi((lambda[1]-mu_old_cont)/sigma_old);
        theta_old_disc_old <- Phi((lambda[1]-mu_old_disc)/sigma_old);
        theta_old_disc_sim <- Phi((lambda[2]-mu_old_disc)/sigma_old) - Phi((lambda[1]-mu_old_disc)/sigma_old);
        
        theta_sim_old <- Phi((lambda[1]-mu_sim)/sigma_sim);
        theta_sim_sim <- Phi((lambda[2]-mu_sim)/sigma_sim) - Phi((lambda[1]-mu_sim)/sigma_sim);
        

        theta_new_old <- Phi((lambda[1]));
        theta_new_sim <- Phi((lambda[2])) - Phi((lambda[1]));
        
        thetas_old_cont[1] <- theta_old_cont_old;
        thetas_old_cont[2] <- theta_old_cont_sim;
        thetas_old_cont[3] <- 1-theta_old_cont_old-theta_old_cont_sim;
		thetas_old_disc[1] <- theta_old_disc_old;
		thetas_old_disc[2] <- theta_old_disc_sim;
		thetas_old_disc[3] <- 1-theta_old_disc_old-theta_old_disc_sim;
        thetas_sim[1] <- theta_sim_old;
        thetas_sim[2] <- theta_sim_sim;
        thetas_sim[3] <- 1-theta_sim_old-theta_sim_sim;
        thetas_new[1] <- theta_new_old;
        thetas_new[2] <- theta_new_sim;
        thetas_new[3] <- 1-theta_new_old-theta_new_sim;       
        }
	
	model {
	    lambda[1] ~ normal(lambda1_mu0, lambda1_sigma0);
	    lambda[2] ~ normal(lambda2_mu0, lambda2_sigma0);
	    sigma_old ~ inv_gamma(sigma_old_alpha0, sigma_old_beta0);
	    sigma_sim ~ inv_gamma(sigma_sim_alpha0, sigma_sim_beta0);
		mu_sim ~ normal(mu_sim_mu0, mu_sim_sigma0);
		mu_old_cont ~ normal(mu_old_cont_mu0, mu_old_sigma0);
		mu_old_disc ~ normal(mu_old_disc_mu0, mu_old_sigma0);
	    
	    n_old_cont_responses ~ multinomial(thetas_old_cont);
        n_old_disc_responses ~ multinomial(thetas_old_disc);
        n_sim_responses ~ multinomial(thetas_sim);
        n_new_responses ~ multinomial(thetas_new);

	}
	
	generated quantities {
    	vector[n_fold] log_lik;
    	for (i in 1:n_fold){
    		log_lik[i] <- multinomial_log(n_old_cont_responses_test[i,], thetas_old_cont);
    		log_lik[i] <- log_lik[i] + multinomial_log(n_old_disc_responses_test[i,], thetas_old_disc);
    		log_lik[i] <- log_lik[i] + multinomial_log(n_sim_responses_test[i,], thetas_sim);
    		log_lik[i] <- log_lik[i] + multinomial_log(n_new_responses_test[i,], thetas_new);	    	
    	}	
    }


";

#run to see model outputs
fit_MU <- stan(model_code = sdt_model, data = sdt_data, iter=10000, chains=2, verbose=TRUE); #Run to fit model

#run to compare conditions
samps_MU <- extract(fit, c('mu_old_cont', 'mu_old_disc')) #collect samples for comparison
summary(samps_MU$mu_old_cont - samps_MU$mu_old_disc) #gives you comparison difference up to max

#run to evaluate fit
log_likMU <- extract_log_lik(fit_MU) 
loo_MU <- loo(log_likMU) 
waic_MU <- waic(log_likMU)


