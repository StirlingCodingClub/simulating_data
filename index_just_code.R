# Random uniform numbers
rand_unifs_10    <- runif(n = 10, min = 0, max = 1);
rand_unifs_10000 <- runif(n = 10000, min = 0, max = 1);
hist(rand_unifs_10000, xlab = "Random value (X)", col = "grey",
     main = "", cex.lab = 1.5, cex.axis = 1.5);

# Random normal numbers
rand_norms_10    <- rnorm(n = 10, mean = 0, sd = 1);
rand_norms_10000 <- rnorm(n = 10000, mean = 0, sd = 1);
hist(rand_norms_10000, xlab = "Random value (X)", col = "grey",
     main = "", cex.lab = 1.5, cex.axis = 1.5);

# Random poisson numbers
rand_poissons       <- rpois(n = 10, lambda = 4);
rand_poissons_10000 <- rpois(n = 10000, lambda = 4);
hist(rand_poissons_10000, xlab = "Random value (X)", col = "grey",
     main = "", cex.lab = 1.5, cex.axis = 1.5);

# Random binomials
rand_binoms1 <- rbinom(n = 1, size = 1000, prob = 0.5);
print(rand_binoms1);

rand_binoms2 <- rbinom(n = 2, size = 1000, prob = 0.5);
print(rand_binoms2);

rand_binomis10000 <- rbinom(n = 10000, size = 1000, prob = 0.5);
hist(rand_binomis10000, xlab = "Random value (X)", col = "grey",
     main = "", cex.lab = 1.5, cex.axis = 1.5);


# Random sampling using sample
rand_number_1 <- sample(x = 1:10, size = 1);
rand_number_10 <- sample(x = 1:10, size = 10);
rand_number_10_r <- sample(x = 1:10, size = 10, replace = TRUE);

# Random sampling of strings
species   <- c("species_A", "species_B", "species_C");
sp_sample <- sample(x = species, size = 12, replace = TRUE, 
                    prob = c(0.5, 0.25, 0.25) 
                    );

# Simulating data with known correlations

N   <- 10000;
rho <- 0.3;
x1  <- rnorm(n = N, mean = 0, sd = 1);
x2  <- (rho * x1) + sqrt(1 - rho*rho) * rnorm(n = N, mean = 0, sd = 1);

cor(x1, x2);

# install.packages("MASS");
library("MASS");

matrix_data <- c(12.68, 13.95, 3.07, 13.95, 30.39, 4.70, 3.07, 4.70, 2.18);
cv_mat      <- matrix(data = matrix_data, nrow = 3, ncol = 3, byrow = TRUE);
rownames(cv_mat) <- c("M1", "M2", "M3");
colnames(cv_mat) <- c("M1", "M2", "M3");

mns      <- c(159.54, 245.26, 25.52);
sim_data <- mvrnorm(n = 40, mu = mns, Sigma = cv_mat);

apply(X = sim_data, MARGIN = 2, FUN = mean);

cov(sim_data);

par(mar = c(5, 5, 1, 1));
plot(x = sim_data[,1], y = sim_data[,2], pch = 20, cex = 1.25, cex.lab = 1.25,
     cex.axis = 1.25, xlab = expression(paste("Value of ", M[1])),
     ylab = expression(paste("Value of ", M[2])));


N           <- 20;
matrix_data <- c(12.68, 13.95, 3.07, 13.95, 30.39, 4.70, 3.07, 4.70, 2.18);
cv_mat      <- matrix(data = matrix_data, nrow = 3, ncol = 3, byrow = TRUE);
mns_1       <- c(159.54, 245.26, 25.52);
sim_data_1  <- mvrnorm(n = N, mu = mns, Sigma = cv_mat);
colnames(sim_data_1) <- c("Length", "Width", "Mass");
# Below, I bind a column for indicating 'species_1' identity
species     <- rep(x = "species_1", times = 20); # Repeats 20 times
sp_1        <- data.frame(species, sim_data_1);

offspring   <- rpois(n = N, lambda = sp_1$Mass * 0.1);
sp_1        <- cbind(sp_1, offspring);


# First making species 2
mns_2       <- c(159.54, 245.26, 25.52 + 3); # Add a bit
sim_data_2  <- mvrnorm(n = N, mu = mns, Sigma = cv_mat);
colnames(sim_data_2) <- c("Length", "Width", "Mass");
species     <- rep(x = "species_2", times = 20); # Repeats 20 times
offspring   <- rpois(n = N, lambda = sim_data_2[,3] * 0.1);
sp_2        <- data.frame(species, sim_data_2, offspring);
# Now make species 3
mns_3       <- c(159.54, 245.26, 25.52 + 4.5); # Add a bit more
sim_data_3  <- mvrnorm(n = N, mu = mns, Sigma = cv_mat);
colnames(sim_data_3) <- c("Length", "Width", "Mass");
species     <- rep(x = "species_3", times = 20); # Repeats 20 times
offspring   <- rpois(n = N, lambda = sim_data_3[,3] * 0.1);
sp_3        <- data.frame(species, sim_data_3, offspring);
# Bring it all together in one data set
dat <- rbind(sp_1, sp_2, sp_3);


aov_result <- aov(Mass ~ species, data = dat);
summary(aov_result);

# install.packages("lme4")
library(lme4);

mod <- glmer(offspring ~ Mass + (1 | species), data = dat, family = "poisson");
summary(mod);



N         <- 1200;
species   <- c("species_A", "species_B");
sp_sample <- sample(x = species, size = N, replace = TRUE);
sp_mass   <- rnorm(n = N, mean = 100, sd = 4);
for(i in 1:N){
        if(sp_sample[i] == "species_A"){
                sp_mass[i] <- sp_mass[i] + runif(n = 1, min = 0, max = 4);
        }
}
sim_data  <- data.frame(species, sp_mass);





