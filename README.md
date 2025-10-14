<div align="center" style="margin-bottom: 2em">
    <img src="./assets/logo.svg" width="60%"/>
</div>

# HADES - Automated Hardware Design Exploration for Cryptographic Primitives

Source code for the paper [HADES - Automated Hardware Design Exploration for Cryptographic Primitives](https://eprint.iacr.org/2024/130.pdf).

## 1. Features
HADES is a framework written in Scala to automatically perform a design space exploration on cryptographic designs and secure them against physical attacks.

## 2. Contact and Support
Please contact Fabian Buschkowski ([fabian.buschkowski@rub.de](fabian.buschkowski@rub.de)) or Niklas Höher ([niklas.hoeher@rub.de](niklas.hoeher@rub.de)) if you have any questions, comments, if you found a bug that should be corrected, or if you want to reuse the framework or parts of it for your own research projects.

## 3. Setup
Clone this repository into a destination of your choice.

The repository contains a nix-shell that should automatically load all necessary dependencies for you. Simply install [nix](https://nix.dev/install-nix) and you should have everything you need.

Alternatively, you can install all necessary dependencies yourself:
In order to run the tool, a working setup of [SpinalHDL](https://spinalhdl.github.io/SpinalDoc-RTD/master/index.html) is needed. Follow the [installation guide](https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Getting%20Started/Install%20and%20setup.html) to set it up. In order to simulate designs during testing, we additionally recommend to install [Verilator](https://www.veripool.org/verilator/) for simulation and [GTKWave](https://gtkwave.sourceforge.net/) to view the simulated waveforms. After SpinalHDL is set up, move our code to a directory of your choice and you should be ready to start.

## 4. Quick Start
If you are using nix, simply start the nix shell by `nix-shell` (you might need sudo privileges to do so). After the shell is started, navigate to the root directory of the project and run the Scala Build Tool by executing `sbt` (you might also need sudo privileges).

If you are not using nix, open a shell in the root directory of this project and start the Scala Build Tool by running `sbt`. 

Once sbt is started, you can perform the Design Space Exploration by entering `runMain HADES` in the shell. When executed the first time, the initial compilation can take a few minutes, but subsequent executions will be significantly faster.

### 4.1 Configuring the Tool
Our tool can be configured via a configuration file written in JSON. All configuration files are located in the directory `src/main/scala/Configuration`. We refer to the following overview for a description of the existing settings and their functionality:

Parent       | Parameter              | Allowed Options         | Description
-------------|------------------------|-------------------------|----------------------------------------
`General`    | `Cell-library`         | String  | Name of the file containing the sizes of gates. Use `45nm` to use the provided file. Additional files should be placed in `src/main/scala/Gadgets`
| | `Reset-Type` | `synchronous`, `asynchronous` | Synchronous or asynchronous reset for generated hardware
| | `Mode` | `verilog`, `vhdl` | Generation of Verilog or VHDL files.
| | `Verbose` | 0-3 | Not yet implemented
| | `Name-Prefix` | String | Prefix for all generated designs (e.g., `Keccak`, `AES-128`, `KyberCCA`, ...)
| | `Separate-Folders` | `true`, `false` | Create a separate folder for each generated design
| | `Fixed-Seed` | `true`, `false` | Change whether a fixed seed or a random seed is used for simulation
| | `Local-Optimization-Depth` | 0-X | After which depth should the exploration be restarted. 0 and numbers larger than the maximum template depth result in a full design space exploration. 1 means that optimizations happen for every template individually, 2 that two levels in the hierarchy are explored together. 1 is the fastest, but gives suboptimal results for tradeoff performance metrics.
| | `Tasks/Explore` | `true`, `false` | Enable design space exploration
| | `Tasks/Generate` | `true`, `false` | Enable generation of HDL code for the explored designs
| | `Tasks/Simulate` | `true`, `false` | Enable simulation of designs (requires that a simulation function is passed to HADES)
| | `Top-X` | -1, 1-X | How many of the best designs should be generated/simulated. If -1 is selected, all designs are generated.
| | `Explore-Security` | `true`, `false` | Include the security order into the DSE to find the highest possible security order given a set of thresholds. Requires `Enable-Thresholds` to be `true` and at least one threshold to be set.
| | `Local-Search` | | Configuration for local search optimization
| | `Local-Search/Enable` | `true`, `false` | Enable local search
| | `Local-Search/Initial-Sample-Size` | 1-X | Size of the inital sample set, has to be greater than 0
| | `Local-Search/Runs` | 1-X | Number of consecutive runs, the best configuration over all runs is chosen in the end (greater than 0)
| | `Enable-Thresholds` | `true`, `false` | Enable the optimization with desired performance thresholds.
| | `Thresholds` | 1-X many entries | Array containing the thresholds
| | `Thresholds/Target` | `Latency`, `Randomness`, `ATP`, `ATRP`, `Area` | Performance metric
| | `Thresholds/Value` | Integer | Desired threshold for specified performance metric, design's performance should be below the value.
| | `Optimization/Area/Enable` | `true`, `false` | Enable optimization for area
| | `Optimization/Area/Secondary` | `Latency`, `Randomness`, `ATP`, `ATRP` | Secondary optimization target in case of a tie
| | `Optimization/Latency/Enable` | `true`, `false` | Enable optimization for Latency
| | `Optimization/Latency/Secondary` | `Area`, `Randomness`, `ATP`, `ATRP` | Secondary optimization target in case of a tie
| | `Optimization/Randomness/Enable` | `true`, `false` | Enable optimization for Randomness
| | `Optimization/Randomness/Secondary` | `Area`, `Latency`, `ATP`, `ATRP` | Secondary optimization target in case of a tie
| | `Optimization/Reload/Enable` | `true`, `false` | Enable optimization for Reload
| | `Optimization/Reload/Secondary` | `Area`, `Latency`, `ATP`, `ATRP` | Secondary optimization target in case of a tie
| | `OPtimization/ATP/Enable` | `true`, `false` | Enable optimization for Area-Latency-Product
| | `Optimization/ATP/Secondary` | `Area`, `Latency`, `Randomness`, `ATRP` | Secondary optimization target in case of a tie
| | `Optimization/ATRP/Enable` | `true`, `false` | Enable optimization for Area-Latency-Randomness-Product
| | `Optimization/ATRP/Secondary` | `Area`, `Latency`, `Randomness`, `ATP` | Secondary optimization target in case of a tie
| | `Visualization/Enable` | `true`, `false` | Not yet implemented
| | `Visualization/Path` | String | Not yet implemented
| | `Visualization/Full` | `true`, `false` | Not yet implemented
| | `Gadget-Config/Enable` | `true`, `false` | If true, the tool uses the specified configuration and does not explore the implemented Gadgets
| | `Gadget-Config/LatencyAND` | 0-X | Latency (in cycles) for an AND-gate
| | `Gadget-Config/LatencyXOR` | 0-X | Latency (in cycles) for an XOR-gate
| | `Gadget-Config/LatencyNOT` | 0-X | Latency (in cycles) for a NOT-gate
`Side-Channel` | `Enable` | `true`, `false` | Enable DSE of side-channel protected designs
| | `Order` | 1-X | Masking degree
`Fault-Injection` | `Enable` | `true`, `false` | Not yet implemented
| | `Order` | 1-X | Not yet implemented
`Combined` | `Enable` | `true`, `false` | Not yet implemented
| |`Sca-order` | 0-X | Not yet implemented
| | `Fault-order` | 0-X | Not yet implemented

### 4.2 Exploring a different Cryptographic Algorithm
To change the cryptographic algorithm that is being explored, modify the `main`-function in `HADES.scala`. The typical function call in the `main`-function looks as follows:

```
HADES("HADES")(new Keccak(64))().apply()
```
The first parameter is the used configuration file (`"HADES"`), followed by the template to be explored (`new Keccak(64)`) and the simulation function (in this case, none is specified).

### 4.3 Expected Output
If our tool is executed with the given configuration and the `Keccak`-template, the output should be the same as shown below:

```
[Progress] at 0,000 : Starting to explore all possible designs for Template Keccak
[Progress] at 0,019 : Currently exploring designs for Gadget HPC2
[Progress] at 0,026 : Starting to fetch all configurations
[Progress] at 0,034 : Found 7 possible configurations
[Progress] at 0,300 : Currently exploring designs for Gadget HPC3
[Progress] at 0,305 : Starting to fetch all configurations
[Progress] at 0,305 : Found 7 possible configurations
[Progress] at 0,429 : 14 possible configurations out of 14 total configurations were found
[Progress] at 0,434 : Top 2 configurations sorted by Area
[Progress] at 0,434 : |    Latency |     Reload |       Area | Randomness |        ATP |       ATRP | Configuration |
[Progress] at 0,434 : -----------------------------------------------------------------------------------------------
[Progress] at 0,437 : |       3144 |          0 |      15064 |         50 |     473E+5 |     236E+7 | (HashMap(toplevel -> Map(parallelism -> 1)),Gadgets.HPC3Gadget$@6cb107fd,1) |
[Progress] at 0,438 : |       4680 |          0 |      15825 |         25 |     740E+5 |     185E+7 | (HashMap(toplevel -> Map(parallelism -> 1)),Gadgets.HPC2Gadget$@60bd273d,1) |
[Progress] at 0,438 : 
[Progress] at 0,438 : Starting generation and simulation of the designs
[Progress] at 1,866 : Starting Masking
[Progress] at 2,225 : Generating Hardware
[Progress] at 4,550 : Starting Masking
[Progress] at 4,818 : Generating Hardware
[Progress] at 5,615 : Top 2 configurations sorted by Area_Time_Product
[Progress] at 5,615 : |    Latency |     Reload |       Area | Randomness |        ATP |       ATRP | Configuration |
[Progress] at 5,616 : -----------------------------------------------------------------------------------------------
[Progress] at 5,616 : |        264 |          0 |      42769 |        800 |     112E+5 |     903E+7 | (HashMap(toplevel -> Map(parallelism -> 16)),Gadgets.HPC3Gadget$@6cb107fd,1) |
[Progress] at 5,617 : |        168 |          0 |      72357 |       1600 |     121E+5 |     194E+8 | (HashMap(toplevel -> Map(parallelism -> 32)),Gadgets.HPC3Gadget$@6cb107fd,1) |
[Progress] at 5,617 : 
[Progress] at 5,617 : Starting generation and simulation of the designs
[Progress] at 6,566 : Starting Masking
[Progress] at 6,731 : Generating Hardware
[Progress] at 8,868 : Starting Masking
[Progress] at 9,493 : Generating Hardware
[Progress] at 10,341 : Top 2 configurations sorted by Area_Time_Randomness_Product
[Progress] at 10,341 : |    Latency |     Reload |       Area | Randomness |        ATP |       ATRP | Configuration |
[Progress] at 10,341 : -----------------------------------------------------------------------------------------------
[Progress] at 10,342 : |       4680 |          0 |      15825 |         25 |     740E+5 |     185E+7 | (HashMap(toplevel -> Map(parallelism -> 1)),Gadgets.HPC2Gadget$@60bd273d,1) |
[Progress] at 10,342 : |       2376 |          0 |      18414 |         50 |     437E+5 |     218E+7 | (HashMap(toplevel -> Map(parallelism -> 2)),Gadgets.HPC2Gadget$@60bd273d,1) |
[Progress] at 10,342 : 
[Progress] at 10,342 : Starting generation and simulation of the designs
[Progress] at 11,016 : Starting Masking
[Progress] at 11,149 : Generating Hardware
[Progress] at 12,394 : Starting Masking
[Progress] at 12,512 : Generating Hardware
[Progress] at 13,136 : Top 2 configurations sorted by Latency
[Progress] at 13,136 : |    Latency |     Reload |       Area | Randomness |        ATP |       ATRP | Configuration |
[Progress] at 13,136 : -----------------------------------------------------------------------------------------------
[Progress] at 13,136 : |        120 |          0 |     131546 |       3200 |     157E+5 |     505E+8 | (HashMap(toplevel -> Map(parallelism -> 64)),Gadgets.HPC3Gadget$@6cb107fd,1) |
[Progress] at 13,136 : |        144 |          0 |     179557 |       1600 |     258E+5 |     413E+8 | (HashMap(toplevel -> Map(parallelism -> 64)),Gadgets.HPC2Gadget$@60bd273d,1) |
[Progress] at 13,136 : 
[Progress] at 13,136 : Starting generation and simulation of the designs
[Progress] at 14,755 : Starting Masking
[Progress] at 15,741 : Generating Hardware
[Progress] at 18,931 : Starting Masking
[Progress] at 19,219 : Generating Hardware
[Progress] at 20,490 : Top 2 configurations sorted by Randomness
[Progress] at 20,490 : |    Latency |     Reload |       Area | Randomness |        ATP |       ATRP | Configuration |
[Progress] at 20,490 : -----------------------------------------------------------------------------------------------
[Progress] at 20,490 : |       4680 |          0 |      15825 |         25 |     740E+5 |     185E+7 | (HashMap(toplevel -> Map(parallelism -> 1)),Gadgets.HPC2Gadget$@60bd273d,1) |
[Progress] at 20,490 : |       2376 |          0 |      18414 |         50 |     437E+5 |     218E+7 | (HashMap(toplevel -> Map(parallelism -> 2)),Gadgets.HPC2Gadget$@60bd273d,1) |
[Progress] at 20,490 : 
[Progress] at 20,490 : Starting generation and simulation of the designs
[Progress] at 21,170 : Starting Masking
[Progress] at 21,280 : Generating Hardware
[Progress] at 22,531 : Starting Masking
[Progress] at 22,656 : Generating Hardware

Process finished with exit code 0
```

The output first shows the progress during DSE. Once DSE is completed, the optimization category is shown, followed by the best designs in this category with the respective design configuration and the reached performance in the form (Latency, estimated Area, Randomness, Area-Latency-Randomness-Product, Area-Latency-Product).

### 4.4 Generated Designs
The generated designs can be found in the folders `src/main/Hardware/VHDL` or `src/main/Hardware/Verilog`. Generated designs will be named `Keccak_Sec_1_Area_0.v` or similarly, where `Keccak` is the template name (the prefix would come before the template name if a prefix is set), `Sec_1` indicates first order side-channel protection, and `Area_0` indicates that this is the best design in terms of estimated Area.

The selected configuration as well as the resulting performance metrics are also included as a header comment within the generated output HDL files.

### 4.5 Running Custom Simulations
> **Note:**  In order to run simulations, there are some constraints on a specific set of configuration options within `HADES.json`.
> - `General/Mode` needs to be set to `verilog`
> - `General/Tasks/Simulate` needs to be set to `true`
> - `General/Gadget-Config/Enable` needs to be set to `true`
> - `Side-Channel/Enable` needs to be set to `false`
> 
> The fact that simulation with side-channel protections enabled is currently not possible is a known limitation which will be addressed in a future release.

In addition to our included suite of tests which can be run by executing `test` within the SBT shell, we also support running
arbitrary custom simulations for all templates. In order to do this, simply pass a simulation function (which defines input signals, how many clock cycles to simulate, ...) as the second argument to the `HADES` class.

For our Keccak template, such a simulation function could look as follows:
```scala
def mySimFunction(dut: Component): Unit = {
    val top = dut.asInstanceOf[Keccak]
    top.cd.forkStimulus(10)

    // Set inputs
    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            top.io.stateIn(i)(j) #= 42
        }
    }
    top.fsm.enable #= false

    // Start execution
    top.cd.waitSampling(1)
    top.fsm.enable #= true
    top.cd.waitSampling(1)
    top.fsm.enable #= false

    // Wait for execution to finish
    top.cd.waitSampling(top.latency)
}
```

To execute the simulation, you would replace the default HADES call within the main entrypoint in `HADES.scala` with:

```scala
HADES("HADES")(new Keccak(64))(mySimFunction).apply()
```


After performing any simulations, the waveforms will be located in sub-directories of `simWorkspace/` within the project's root directory.

## Acknowledgements
- HADES Logo by [Anna Guinet](https://www.annagui.net/), used under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).

## Licensing
Copyright (c) 2025, Fabian Buschkowski and Niklas Höher, Chair for Security Engineering, Ruhr-Universitaet Bochum

All rights reserved

Please see `LICENSE` for further license instructions.