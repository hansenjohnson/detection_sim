# detection_sim
Simulate and compare whale detection methods

## Project outline
- the right whale movement model code is in `r/rw_sim.R`  
- examples for how to run the model are in `tests/movement_model_example.R`  
- eventually the analysis steps will be documented and executable in `master.R`

## Analysis steps
1. find literature estimates for calling rate and surfacing rate for right whales  
2. add these behaviors to the model  
3. simulate acoustic and/or visual detection for a stationary platform  
4. adapt for a moving platform  
5. simulate glider- and plane-based detection
6. compare results, update model parameters as necessary, and repeat  

## Running remotely

Currently set up to run remotely on "kaos" machine at Dal

### kaos setup

Install required packages:
```
R -e "install.packages(c('tidyverse','parallel','zoo'), repos='https://cran.rstudio.com/')"
```

Run over ssh:
```
Rscript r/run_box_surveys.R 
```

Run headless:
```
# run job (output will print to nohup.out)
nohup Rscript r/run_box_surveys.R &

# record PID
57073

# kill process (only if necessary)
kill -9 <PID>
```
