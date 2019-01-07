library(DCGen)


ExpA = new_expert("A", "Triangle", c(54.2, 10, 150), "Triangle", c(2.3, 0, 5), "Triangle", c(5.1, 3, 8))
ExpB = new_expert("B", "Triangle", c(158.8, 30, 230), "Triangle", c(5.7, 2, 15), "Triangle", c(9.9, 7, 13))
ExpC = new_expert("C", "Triangle", c(204.4, 30, 410), "Triangle", c(7.1, 2, 10), "Triangle", c(3.5, 2, 6))

experts <- aggregate_experts(list(ExpA, ExpB, ExpC))


pars <- rand_parameters(experts, 300, method='mixture', type='continuous')

cvs <- generate_diffusion_curves(pars, t_max=20)


# visualise_inputs(pars)
# visualise_fitted(pars)
# visualise_comparing(pars)

# visualise_curves(cvs, dN=T)
# visualise_curvefit(pars, cvs)

