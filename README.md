# Sandbox usage statistics plots

Script for generating Sandbox usage statistics plots. Generates time series plots of total as well as sandbox-level usage statistics.

Requires the following R libraries:
- optparse
- ggplot2
- RColorBrewer
- cowplot
- gridExtra


## Usage 

Type `Rscript generate_report.R --help` for printing help message. 

```

Usage: generate_report.R [options]

Options:
	--path=CHARACTER
		Path to the location with files containing statistics gathered.

	--out=CHARACTER
		Full path to the output file. Default: "./plots<TIMESTAMP>.pdf"

	--size=INTEGER
		Text size [default= 14]

	--width=INTEGER
		PDF document width [default= 23]

	--height=INTEGER
		PDF document height [default= 15]

	-h, --help
		Show this help message and exit

```

Example:


```
Rscript generate_report.R --path data/ --out /path/to/your/output/plots.pdf


List of files under data/ folder used in the example:
statistics_2022_10_20221101_043002.txt
statistics_2022_11_20221201_043002.txt
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





