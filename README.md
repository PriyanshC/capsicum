# Capsicum

This repository contains the codebase for my final-year project on a capture checking native algebraic effects and handlers library.

This is a research experiment is not published as a publicly available library at this current point in time.

The choice of name was influenced by my supervisor, Jamie's naming convention of phonetically similar herbs and spices, associating the prefix **cap-** with **cap**ture checking, or equality **cap**ability.

## Pre-requisites
- sbt
- JDK >= 21

## Main
Starting sbt loads the **capsicum** project.
```bash
# runs the entry-point in `src/main/scala/capsicum/examples/Main.scala`
run

# runs the test-suite in `src/test/scala/capsicum/TODO/
test
```


## Benchmarks subproject
The backtracking benchmark uses JMH and is located in a subproject.
```bash
# Load the subproject
project benchmarks

# Running the benchmark with default parameters
Jmh/run # run

# Running the benchmark with custom parameters, as expected by Jmh
Jmh/run -i 3 -wi 3 -rf json -rff results.json
```


## Experiments subproject
This subproject contains a range of code snippets, either relevant to capture checking or other effect systems. 
```bash
# Load the subproject
project experiments

# Execute any source code where objects extend `App` or methods are annotated with @main 
run

# Directory structure:
cd experiments/src/gh-issue-snippets # Contains examples of the code used when publishing issues to the scala3 repository
cd experiments/src/libs-with-cc # Basic programs of other effect systems with CC enabled
cd experiments/src/motivating-examples # contains two directories: cc-off/ and cc-on/ which show flaws in programs and the CC error messages (if any)
```
