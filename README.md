# itchmodel - Intertemporal choice model

## Overview
`itchmodel` is a computational model that can be used to understand the cognitive mechanisms underlying intertemporal choice performance: the __defer/speedup effect__ (also known as the delay/speedup effect) and the __date/delay effect__. 

The model builds on two tradeoff models of intertemporal choice: one that explains choice probabilities and full response time distributions in intertemporal choice [Dai & Busemeyer, 2014](https://doi.org/10.1037/a0035976) and another that can account for contextual effects, such as framing of time  [Scholten & Read, 2013](https://doi.org/10.1037/a0031171).

## Description

Conceptually, the model explains intertemporal choice as a __three-step process__:

![Intertemporal choice model](./itchmodel_schematic.png)

1. A decision maker transforms money into utility (value function), and calendar time into perceived/weighted time (time function), separately for the small-but-sooner and large-but-later options. These value and time functions are governed by power transformations.

2. Differences in utility are compared against differences in perceived/weighted time with different weights (_w_ scales the difference between weighted delays and the difference between valued outcomes in a common currency), resulting in _d_, the overall advantage of the large-but-later option over the small-but-sooner option.

3. The mean rate of preference, _d_, is accumulated over time to a threshold that triggers an explicit preferential choice (i.e. diffusion or sequential-sampling process), providing predicted choice and response time.

The current version of the model can be parameterized in multiple ways to explain context effects, such as time framing:

1. Parameter _mu_ varies between frames, corresponding to the hypothesis that time framing influences valuation.

2. Parameter _kappa_ varies between frames, corresponding to the hypothesis that time framing influences time weighting/perception.

3. Parameters _mu_ and _kappa_ vary between frames, corresponding to the hypothesis that time framing influences both valuation and time weighting/perception.

4. Parameters _mu_ and _t0_ vary between frames; as 1., but also explaining RT differences due to differences in stimuli.

5. Parameters _kappa_ and _t0_ vary between frames; as 2., but also explaining RT differences due to differences in stimuli.

6. Parameters _mu_, _kappa_, and _t0_ vary between frames; as 3., but also explaining RT differences due to differences in stimuli.

## Installation

The model can be installed from R as follows:
```
devtools::install_github("bramzandbelt/itchmodel")
library(itchmodel)
```

## Usage

Usage will be explained in more detail soon. In the meantime, see the R Markdown notebooks inside the `analysis` directory for usage examples.


## Colophon

### Version

0.0.1 - June 2018

### Contact

E-mail: bramzandbelt@gmail.com

### References

- Scholten, M. & Read, D. (2013) Time and outcome framing in intertemporal tradeoffs. Journal of Experimental Psychology: Learning, Memory, and Cognition, 39, 1192–1212.

- Dai, J. & Busemeyer, J.R. (2014) A probabilistic, dynamic, and attribute-wise model of intertemporal choice. Journal of Experimental Psychology: General, 143, 1489–1514.
