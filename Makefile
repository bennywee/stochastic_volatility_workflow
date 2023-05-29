sample_model:
	Rscript -e 'renv::run("scripts/run_model.r")'

ksc_data:
	Rscript -e 'renv::run("scripts/create_ksc_data.r")'

yahoo_data:
	Rscript -e 'renv::run("scripts/create_yahoo_data.r")'

sync_sims:
	rsync -auv -e ssh benjamiw@monarch.erc.monash.edu:~/zk28/benjamiw/stochastic_volatility_workflow/simulation_output .