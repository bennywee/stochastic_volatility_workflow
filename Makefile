train_model:
	Rscript -e 'renv::run("scripts/run_model.r")'

ksc_data:
	Rscript -e 'renv::run("scripts/create_ksc_data.r")'

yahoo_data:
	Rscript -e 'renv::run("scripts/create_yahoo_data.r")'