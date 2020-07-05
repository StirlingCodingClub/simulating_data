



library("MASS");

var_names <- c("Var_1", "Var_2");
N         <- 10;
mean_vals <- c(0, 0);
cor_mat   <- matrix(data = c(1, 0, 0, 1), nrow = 2, byrow = FALSE);

rownames(cor_mat) <- var_names;
colnames(cor_mat) <- var_names;


sim_dat   <- mvrnorm(n = N, mu = mean_vals, Sigma = cor_mat, empirical = TRUE);
