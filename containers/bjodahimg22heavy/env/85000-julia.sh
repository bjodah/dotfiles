#!/bin/bash -xe
JULIA_MAJOR=1
JULIA_MINOR=8
JULIA_PATCH=4
JL_V2=$JULIA_MAJOR.$JULIA_MINOR
JL_V3=$JULIA_MAJOR.$JULIA_MINOR.$JULIA_PATCH
ARCH=$(uname -m)
if [[ "$ARCH" == x86_64 ]]; then
    SHORT_ARCH=x64
else
    SHORT_ARCH=$ARCH
fi
URL="julialang-s3.julialang.org/bin/linux/${SHORT_ARCH}/${JL_V2}/julia-${JL_V3}-linux-${ARCH}.tar.gz"
curl -Ls "https://$URL" | tar xz -C /opt
PATH=/opt/julia-$JL_V3/bin:$PATH julia -e 'using Pkg; \
       Pkg.add("DifferentialEquations"); using DifferentialEquations; \
       pkg"add https://github.com/SciML/SciMLTutorials.jl"; using SciMLTutorials; \
       Pkg.add("IJulia"); using IJulia; \
       Pkg.add("PyPlot"); using PyPlot; \
       Pkg.add("Latexify"); using Latexify;
       Pkg.add("ODEInterfaceDiffEq"); using ODEInterfaceDiffEq;'

#        Pkg.add("DiffEqBiological"); using DiffEqBiological; \
# cd /opt
# git clone https://github.com/SciML/SciMLTutorials.jl
# PATH=/opt/julia-$JL_V3/bin:$PATH julia -e 'using Pkg, SciMLTutorials; \
#        cd(joinpath(dirname(pathof(SciMLTutorials)), "..")); \
#        Pkg.pkg"activate ."; \
#        Pkg.pkg"instantiate"; \
#        SciMLTutorials.weave_folder("tutorials/introduction"); \
#        SciMLTutorials.weave_folder("tutorials/models"); \
#        SciMLTutorials.weave_folder("tutorials/ode_extras"); \
#        SciMLTutorials.weave_folder("tutorials/type_handling"); \
#        '
