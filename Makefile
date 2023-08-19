sample_model:
	Rscript -e 'renv::run("scripts/run_model.r")'

ksc_data:
	Rscript -e 'renv::run("scripts/create_ksc_data.r")'

yahoo_data:
	Rscript -e 'renv::run("scripts/create_yahoo_data.r")'

sync_sims:
	rsync -auv -e ssh benjamiw@monarch.erc.monash.edu:~/zk28/benjamiw/stochastic_volatility_workflow/simulation_output .

run_sim:
	/bin/bash -c "module load  R/4.0.5"
	R --vanilla < scripts/_simulation_metadata.r --args adapt_delta_sim.r
	sbatch jobs/adapt_delta.sh

run_sbc:
	/bin/bash -c "module load  R/4.0.5"
	R --vanilla < scripts/_simulation_metadata.r --args sbc_sim.r
	sbatch jobs/sbc.sh

run_ksc:
	/bin/bash -c "module load  R/4.0.5"
	sbatch jobs/ksc.sh
