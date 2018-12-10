# DCGen
DCGen is a package for generating Bass diffusion curves has been developed to generate diffusion curves relating to technology adoption or product salesbas on elicitation data. The work underpinning the DCGen was undertaken at the University of Sheffield, in the United Kingdom, and is a unique combination of two separate bodies of work; the Bass model of product diffusion and Bayesian expert elicitation (Grimm et al 2018).  This combination allows probabilistic estimates of diffusion that are based on established theory and modelling methods to be generated.
The diffusion estimates produced by the DCGen can be used for a variety of purposes, including budget impact modelling, infectious disease modelling, dynamic cost-effectiveness modelling (Grimm et al 2016 and Grimm et al 2017) and sales forecasting.

### Input elicitation data



### Aggregate elicitation data


#### Linear pooling

#### Mixture model

### Translate elicitation data to Bass Diffusion Curves


#### Visualisation
```r
# Generate a diffusion curve with a (M, p, q) set 
dc <- generate_diffusion_continuous(140, 0.03, 0.41)
dc <- ts(dc[2:3], 0)
ts.plot(dc, col=c("green", "blue"))
legend("right", lty=1, legend=c("N", "dN"), col=c("green", "blue"))
```

#### Validation


### Webapp

An online webapp developed with RShiny is available on 

[Sheffield]()

or [Source codes](https://github.com/Sheffield-Diffusion-Curve/DCGApp)


### References:
* Grimm SE, Stevens JW, Dixon S.  Estimating Future Health Technology Diffusion Using Expert Beliefs Calibrated to an Established Diffusion Model.  Value Health. 2018 Aug;21(8):944-950. doi: 10.1016/j.jval.2018.01.010.
* Grimm SE, Dixon S, Stevens JW. When Future Change Matters: Modeling Future Price and Diffusion in Health Technology Assessments of Medical Devices. Value Health. 2016 Sep - Oct;19(6):720-726. doi: 10.1016/j.jval.2016.06.002.
* Grimm SE, Dixon S, Stevens JW. Assessing the Expected Value of Research Studies in Reducing Uncertainty and Improving Implementation Dynamics. Med Decis Making. 2017 Jul;37(5):523-533. doi: 10.1177/0272989X16686766. 
