#!/bin/bash -xe
curl -Ls "https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-1.5.3-linux-x86_64.tar.gz" | tar xz -C /opt
PATH=/opt/julia-1.5.3/bin:$PATH julia -e 'using Pkg; \
       Pkg.add("DifferentialEquations"); using DifferentialEquations; \
       pkg"add https://github.com/SciML/SciMLTutorials.jl"; using SciMLTutorials; \
       Pkg.add("IJulia"); using IJulia; \
       Pkg.add("PyPlot"); using PyPlot; \
       Pkg.add("Latexify"); using Latexify; \
       Pkg.add("DiffEqBiological"); using DiffEqBiological; \
       Pkg.add("ODEInterfaceDiffEq"); using ODEInterfaceDiffEq;'
cd /opt
git clone https://github.com/SciML/SciMLTutorials.jl
PATH=/opt/julia-1.5.3/bin:$PATH julia -e 'using Pkg, SciMLTutorials; \
       cd(joinpath(dirname(pathof(SciMLTutorials)), "..")); \
       Pkg.pkg"activate ."; \
       Pkg.pkg"instantiate"; \
       SciMLTutorials.weave_folder("introduction"); \
       SciMLTutorials.weave_folder("models"); \
       SciMLTutorials.weave_folder("ode_extras"); \
       SciMLTutorials.weave_folder("type_handling"); \
       '
