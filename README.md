# MMC
MMC is a Metropolis Monte Carlo program for the simulation of molecular assemblies in the canonical, grand-canonical and isothermal-isobaric ensembles employing several convergence acceleration techniques (e.g., force-biased displacement, extension-biased and local torsion changes, preferential selection of solvent to be moved, cavity-biased insertion, virial-biased volume change).

The simulated system generally consists of one part called solute and a collection of identical rigid molecules called solvent. The solute can consist of any number of molecules that can move freely and have any or all of their torsional degrees of freedom sampled. Bond lengths and bond angles are kept fixed.

Solvation free-energy (change) can be calculated using thermodynamic integration, the perturbation formula, the overlap ratio method, the (cavity) Widom insertion method or by potential of mean force calculation using the adaptive umbrella sampling method. Both continuous deformation and creation/annihilation paths can be used.

The solute environment can be analyzed based on the Proximity Criterion and the resulting analysis can be visualized by color coding the solute atoms based on selected analysis result. The Proximity Analysis can be also performed on Charmm or Amber trajectories.

The calculations can use Charmm, Amber, Gromos, Gromacs, OPLS, EPEN and Clementi's potentials. Many atomtypes are built in, additional ones can be defined by the user.

See the full documentation (mmc.html) for more details and for references to the methods used. 
