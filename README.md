# Sandbox usage statistics plots

Script for generating Sandbox usage statistics plots. Generates time series plots of total as well as sandbox-level usage statistics.

Google Cloud Storage bucket storing data for generating plots: **gs://fg-reports_reports** which can be found by link https://console.cloud.google.com/storage/browser/fg-reports_reports. Note that data should be copied locally.

## Run using Docker container from the Compute Engine VM

**STEP 1**. Authenticate to gcloud application default:
```
gcloud auth application-default login
```

**STEP 2**. Authenticate docker using OAuth access token:
```
gcloud auth application-default print-access-token | sudo docker login -u oauth2accesstoken --password-stdin https://eu.gcr.io
```

**STEP 3**. Pull the existing image:

```
sudo docker pull eu.gcr.io/finngen-factory-staging/sb_reports:0.1
```

Alternatively, build a local docker image from the root of the git source repository using e.g. tag name `sb_reports:latest`:
```
sudo docker build --tag=sb_reports:latest -f docker/Dockerfile .
```

**STEP 4**. Copy sandbox monthly statistics files to some local directory `/PATH/TO/INPUT/DATA/FILES` from the bucket that contains summary reports `gs://fg-reports_reports/`. 
```
gsutil cp s://fg-reports_reports/* /PATH/TO/INPUT/DATA/FILES/
```

**STEP 5**. Run the docker image specifying the following mounting points:
1. `/PATH/TO/INPUT/DATA/FILES:/data`: Mount your local path `/PATH/TO/INPUT/DATA/FILES` that stores sandbox statistics files downloaded on the previous step to the `/data` path under the docker. 
2. `~/.config:/root/.config`: Mount your local configs path `~/.config` that stores gcloud application default configuration access token to the `/root/.config` path under the docker. You can leave this parameter as-is.

**NOTE** that once the docker is mounted, you need to provide inputs to the script relative to the mounted directory, e.g. the output of the script in the example above can be specified as `--out /data/plots.pdf` and input as `--path /data`.

Pass <SANDBOX_DATASTORE_PROJECT_ID> Google Cloud project ID which stores Datastore with a list of sandboxes coonfigurations, the names of which should be matched to the sandbox ids on the plot. **NOTE** that Datastore reading rights are required for the script to perform names matching. Otherwise, sandbox ids will be used for the plotting. 

Put flag `--remove_unmatched TRUE` to remove Sandboxes with unmatched Sanbox Names from the report (default: FALSE), see the detailed user manual below.

```
sudo docker run -v ~/.config:/root/.config -v /PATH/TO/INPUT/DATA/FILES:/data \
        -it eu.gcr.io/finngen-factory-staging/sb_reports:0.1 \
		--path /data \
		--out /data/plots.pdf \
		--sb_project <SANDBOX_DATASTORE_PROJECT_ID> \
		--remove_unmatched TRUE \
		--start_date 2022-06-01 \
		--end_date 2023-08-01 \
		--add_nodata FALSE
```


## Run the script manually

Requires the following R libraries:
- optparse
- ggplot2
- RColorBrewer
- cowplot
- gridExtra

Requires the following python3 packages:
- google-auth>=2.16.0
- google-cloud-datastore>=2.13.2

Install python ppakcages: `pip3 install -r scripts/requirements.txt`


## Usage 

Type `Rscript generate_report.R --help` for printing help message. 

```

Usage: generate_report.R [options]

Options:
	-d CHARACTER, --path=CHARACTER
			Path to the location with files containing statistics gathered.

	-p CHARACTER, --sb_project=CHARACTER
			Google Cloud Project ID containing Datastore with 'SandboxConfing' 
			entity storing Sandbox names. If omitted, no Sandbox names matching is 
			performed [default NULL].

	-o CHARACTER, --out=CHARACTER
			Full path to the output file. Default: "./plots_<TIMESTAMP>.pdf"

	-S INTEGER, --size=INTEGER
			Text size [default= 18]

	-W INTEGER, --width=INTEGER
			PDF document width [default= 25]

	-H INTEGER, --height=INTEGER
			PDF document height [default= 15]

	-r LOGICAL, --remove_unmatched=LOGICAL
			Remove Sandboxes with unmatched Sanbox Name from the report 
			[default FALSE].

	-v LOGICAL, --plot_legacy_vm_profiles_separately=LOGICAL
			Plot summary of the legacy VM profiles on a separate plot 
			(i.e. 'Basic Machine') [default FALSE].

	-i LOGICAL, --add_nodata=LOGICAL
			Add info on the side of the plot with sb names omitted from 
			the plot if no stats data exists [default FALSE].

	-N INTEGER, --max_sb_plots_per_page=INTEGER
			Max number of SB figures per page in the overview plots. 
			Speciify if need to split into multiple pages. If not specified, 
			all sb suubplots will be shown on a single plot.

	-s CHARACTER, --start_date=CHARACTER
			Start date, format: %Y-%m-%d, e.g. 2022-11-01

	-e CHARACTER, --end_date=CHARACTER
			End date, format: %Y-%m-%d, e.g. 2023-05-01

	-h, --help
			Show this help message and exit

```

Example:


```
Rscript generate_report.R --path data/ \
	--out /path/to/your/output/plots.pdf \
	--sb_project  fg-production-master \
	--size 22 \
	--width 27 \
	--height 15 \
	--remove_unmatched TRUE \
	--plot_legacy_vm_profiles_separately TRUE \
	--add_nodata FALSE

List of files under data/ folder used in the example:
statistics_2022_10_20221101_043002.txt
statistics_2022_11_20221201_043002.txt
statistics_2022_12_20230101_043002.txt
statistics_2022_1_20220225_104716.txt
statistics_2022_2_20220301_043002.txt
statistics_2022_2_20220301_081830.txt
statistics_2022_3_20220401_033002.txt
statistics_2022_4_20220501_033002.txt
statistics_2022_5_20220601_033002.txt
statistics_2022_6_20220701_033003.txt
statistics_2022_7_20220801_033002.txt
statistics_2022_8_20220901_033003.txt
statistics_2022_9_20221001_033002.txt
statistics_2023_1_20230201_043002.txt
statistics_2023_2_20230301_043003.txt
statistics_2023_3_20230401_033002.txt
statistics_2023_4_20230501_033002.txt
statistics_2023_5_20230601_033002.txt
statistics_2023_6_20230701_033001.txt
statistics_2023_7_20230801_033002.txt
```

Data in the format:
```
[fg-production-master][] Unique users: XXX
[fg-production-master][] Backend errors: XXX
[fg-production-master][] Invalid internal JWT tokens: XXX
...
[fg-production-master][fg-production-sandbox-1] Pipelines started: XXX
[fg-production-master][fg-production-sandbox-2] Pipelines started: XXX
```





