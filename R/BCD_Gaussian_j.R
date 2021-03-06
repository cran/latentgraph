BCD_Gaussiaon_j <- function(j,dat,Xj_star,C_plus,p.fac, accuracy,lambda1,lambda2,lambda3,theta_hat_j,alpha_hat_j,delta_hat_j,R,n,p){
  ## Inpute
  theta_test_j <- theta_hat_j #previous step's result
  alpha_test_j <- alpha_hat_j
  delta_test_j <- delta_hat_j

  ## estimate parameter for jth variable
  error <- 100
  x_j <- dat[,j]
  Xminusj_star <- dat[,-j]
  while(error > accuracy){
    theta_hat_j <- update_theta(lambda1, x_j, Xj_star, alpha_hat_j, delta_hat_j,Xminusj_star)
    alpha_hat_j <- update_alpha(lambda2, x_j, Xminusj_star, theta_hat_j, delta_hat_j,Xj_star)
    delta_hat_j <- update_deltaglm(n,R,lambda3,p.fac, C_plus, x_j, Xminusj_star, theta_hat_j, Xj_star, alpha_hat_j)

    error <- calculate_error(theta_hat_j, theta_test_j, alpha_hat_j, alpha_test_j, delta_hat_j, delta_test_j) # minimize error, make 'test'(k-1) close to 'hat'(k)
    #print(error)

    theta_test_j <- theta_hat_j
    alpha_test_j <- alpha_hat_j
    delta_test_j <- delta_hat_j
  }
  return(list(theta_hat_j = theta_hat_j, alpha_hat_j = alpha_hat_j, delta_hat_j = delta_hat_j))
}
