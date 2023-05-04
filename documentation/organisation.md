# Organisation of what we want to do during the Hackathon

## Teaser
Please help us modify and implement this file, with your ideas, expectation, questions, ...

## What plot types do we want to show?
- [Max] Volcano
- [KELLY] Violin
- [ ] GGplot
- [ ] Karyotype
- [ ] Pedigree
- [ ] Timeline
- [ ] Gene-specific expression across tSNE clusters
- [KELLY ] Single-cell mtDNA/mitochondrial transcriptome coverage
- [ ] RNAseq
- [Yaseswini Neelamraju] Survival
- [Suzy ] Box
- [Max] Heatmap
- [Lawryn] Scatterplots (general) and try adding plotly
- [ ] Plotly
- [Yaseswini Neelamraju; Christy LaFlamme] DNA methylation region plots/browsers
- [Yaseswini Neelamraju ] Manhattan plot
- [Max] sample-to-sample distance plot
- [Max] Histogram
- [Max] PCA plot


## What modules do we have so far? List under each one what cusotmization options we want
- [] Heatmap
- [] Scatterplopt
- [] Violin Plot
- [] Volcano Plot
- [] Histogram
## What general options do we want for each plot type? (Customization options, overlay options, export options)

## What file/object types do we want to be able to use?

There are two test data can be used, located in [example_data](/example_data). One is [MS_2.rda](/example_data/MS_2.rda) contains three `data.frame`:

- `df`, a `data.frame` with feature on row and sample on column. It doesn't matter what is feature, it could be gene if the data is RNA-Seq count/normalized count, or it could be peptide or metabolites if the data is Mass Spec peak intensity data. 

- `sample_meta`, a `data.frame` containing sample metadata, such as sample names, sample group/label/class, sex, time point, etc. 

- `feature_meta`, a `data.frame` containing feature metadata, such as gene name/symbol/emsembleID for gene (if gene is feature), accession number for protein/peptide (if peptide is feature)

The second data [L29_vitro_Control_vs_knockdown_diff](/example_data/L29_vitro_Control_vs_knockdown_diff.txt) is a statistic result table, containing p-value and log2FC, among other variables. Could be the output from DESeq2 or other stat package. 

## What general stucture should we go for ?
- Add tooltips to ggplot for plotly informations

## What role does everyone want to have?
- Project manager
- Clean up person
- Tester
- Documenter
