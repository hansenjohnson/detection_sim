# README
Overview of the box survey concept

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

`box_survey_functions.R` - all of the code and functions to implement this analysis
`box_survey_example.R` - simple worked examples / tests of most of the single steps and analysis components outlined above
`run_box_surveys.R` - script to run larger analysis described in step 5 above. This will take a long time to run, and will save the summary table as output

## Issues

- All components need more testing
- Whale position is initialized in a circle of a given radius within the box. This should eventually be updated so that whales are randomly placed within box
- Not sure if 'reflecting' whales is the best way to keep them in the box, and/or if it introduces any issues/bias
- I (HDJ) found an issue in the `rw_sim()` function where all whales were starting in a dive. I *think* I fixed this, but it's something to watch out for

