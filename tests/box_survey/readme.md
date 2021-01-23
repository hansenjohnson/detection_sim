# README
Overview of the box survey concept, as it was first being conceptualized. The final version ff the box surveys that were run 1000 times using the kaos computer are not under the "tests" folder, but the "r" folder.

## Goal

The goal of this test is to adapt the `detection_sim` code we've developed to address the overarching question(s):

What is the probability of detecting right whale presence in a (fisheries) management area? How does this vary by whale number, whale behavior, platform, and effort?  

## Approach

The modeling approach is to:
1. Define a management area, or 'box', to survey. Fisheries grid cells are approximately 18km long by 12km wide.
2. Randomly generate a survey track from right to left across the box at a given speed and time resolution.
3. Simulate whale movement, surfacing, and calling behaviour within the box during the survey period. This requires an additional step to 'reflect' whales off of the boundaries and keep them inside the box
4. Simulate the detection process
5. Repeat steps 2-4 many, many times with various combinations of whale numbers, platforms, etc. to generate a distribution of possible outcomes
6. Compare the results

## Structure

`old_box_survey_functions.R` - all of the code and functions to implement this analysis
`box_survey_example.R` - simple worked examples / tests of most of the single steps and analysis components outlined above
`old_run_box_surveys.R` - script to run larger analysis described in step 5 above. This will take a long time to run, and will save the summary table as output. Note that this script contains separate output for DFO boxes, TC boxes, and the kaos run, as all three were being tested separately while troubleshooting.

## Issues

- All components need more testing
- Not sure if 'reflecting' whales is the best way to keep them in the box, and/or if it introduces any issues/bias
- I (HDJ) found an issue in the `rw_sim()` function where all whales were starting in a dive. I *think* I fixed this, but it's something to watch out for
- The whales were started within the specified survey boxes, instead of a radius, but I (VC) forgot to switch to m, not km, so there was an error in the code that made for strange plots - HDJ fixed this

