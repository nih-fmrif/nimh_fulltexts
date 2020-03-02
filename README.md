# nimh_fulltexts

This directory contains the basic building blocks we are using in a set of projects whose ultimate goal is to identify instances of data sharing and data reuse in PubMed Central texts.

The raw data cannot be shared directly, but the data download pipeline is reproduced here. First, you need a few index files:

- All projects, publications, and link tables listed at Federal ExPORTER (https://federalreporter.nih.gov/FileDownload). These should be stored in their respective directories in this repo.  
- A PMC to PMID linking file (ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/PMC-ids.csv.gz *warning: the file is ~500mb*). This should be stored in `/data/external/`.

After downloading those, run the code in `src/data/make_nimh_paper_list.R`  followed by `notebooks/01.0-TAR-pull_fulltexts.ipynb`. At the end of this process, you should end up with the full text of around 58k papers funded by the NIMH.

You can also see the submitted OHBM abstract (`/reports/obhm_abstract.pdf`) and the code used to make the figures therein (`visualization/ohbm_figs.R`).