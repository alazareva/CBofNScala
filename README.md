## Examples from The Computational Beauty of Nature (Scala/Processing)

These examples from Gary William Flake's 'Computational Beauty of Nature' book 
were reimplemented using Scala and Processing. The following applications are 
included in this repository.

#### L-Systems
Model growth using a series of simple production rules that though iteration
can crate complex systems resembling structures often found in nature. Example: a 
tree-like structure emerging after several iterations.

![L-Systems](https://github.com/alazareva/CBofNScala/blob/master/examples/lsystems.png)

#### Affine Transformation Fractals
Similar to L-Systems, Affine transformation iteratively apply a set of rules
encoded as linear transformation matrices. Example: Maple Leaf

![IFS](https://github.com/alazareva/CBofNScala/blob/master/examples/ifs.png)

#### Mandelbrot and Julia Sets
Interactively explores the relationship between Mandelbrot and Julia 
sets. Example a region of the Mandelbrot set and a Julia set corresponding to the point
selected on the Mandelbrot image (circled in red).

![Mandelbrot](https://github.com/alazareva/CBofNScala/blob/master/examples/mandelbrot.png)

#### Nonlinear Dynamics
Animated bifurcation diagram of the logistic map.

![Bifurcation](https://github.com/alazareva/CBofNScala/blob/master/examples/bifurcation.png)

#### Strange Attractors
Animated Lorenz attractor shows the trajectory of a point governed by a 
series of 3 differential equations.

#### Producer-Consumer Dynamics
Models the population dynamics of predator-pray systems.

#### Cellular Automata
Explores the evolution of system where the state of each cell is dependant 
on the states of its neighbors. Example: Hodgepodge Machine.

![Hodgepodge](https://github.com/alazareva/CBofNScala/blob/master/examples/hpm.png)

#### Autonomous Agents
Shows how collections of organism can self-organize by each individual 
following a set simple heuristics. Example: Boids going from random to flocking
behavior.

![Boids](https://github.com/alazareva/CBofNScala/blob/master/examples/boids.png)

#### Competition and Cooperation
Illustration of the principles of game theory.

#### Natural and Analog Computation
Using a basic model of a neuron to solve simple tasks.

#### Genetics and Evolution
Applies the concepts of evolution, reproduction, and mutation to evolve
agents that are highly fit to solve specific problems. Example: The evolution 
if a random string into a fun fact about Corgis.

![GA](https://github.com/alazareva/CBofNScala/blob/master/examples/ga.png)

### Installation Tips 
In order to run the code you'll need to add Processing's core.jar library to your classpath.
