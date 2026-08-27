# Can the effects of genetic drift be large, even in a large population? 
Genetic drift is known to be inversely proportional to population size. Are there specific evolutionary conditions, however, under which this relationship stops holding true? Here we explore a few such factors, which contribute to a large effect of genetic drift, even in large populations.

## Table of Contents
1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Data analysis](#data_analysis)

## Introduction
Evolution is a genetic process, with the following as the four mechanisms of evolution: natural selection; genetic drift; mutation & migration. Out of these, drift particularly fascinates me, as it is always present in the background, irrespective of the other mechanisms at play. Also, very few sites in a genome actually code for proteins, and are directly under selection. However, _all_ sites undergo drift, making it all the more relevant. 
Drift is inversely proportional to population size. Here I'm interested at looking for such factors which can cause the effects of drift to be large, even in large popoulations. I'm using a forward genetic simulations based approach using the [SLiM software](https://messerlab.org/slim/) (Selection on Linked Mutation) to run the simulations. This software has specifically been designed to run population level genetic simulations. 

## Installation
### SLiM
The [SLiM Manual](https://github.com/MesserLab/SLiM/releases/download/v4.3/SLiM_Manual.pdf) provides a detailed installation guide for installations on all major platforms. 
SLiM version 4.3 has been used for all the code here. 

### R
R version 4.4.2 was used for this project, with RStudio as the IDE. 
- *Windows installation:* The latest version of R, as well as previous versions are available for download on windows [here](https://cran.r-project.org/bin/windows/base/)
- *Linux installations:* Various installations based on different distros can be found [here](https://cran.r-project.org/bin/)
- *macOS:* R installation files can be downloaded directly from [here](https://cran.r-project.org/bin/macosx/) for macOS and older versions of mac.

## Usage
### SLiM
SLiM is the software used for running the simulations and generating whole genome sequences. This software has its own language, Eidos, with many similarities and inbuilt functions like R, along with those specific to the simulation environment. The learning curve for a person unfamiliar with coding will be steep, as SLiM doesn't have intuitive syntax, and a lot of other functions which are specific to this framework. 
**Basic structure:**
- ```initialize()``` The very first code chunk, which defines all the conditions for the simulation. This includes, but is not limited to genome size, mutation rate, recombination rate, genome and mutation types.
- ```1 early()``` The first code chunk after the initialize chunk where all the starting conditions of the simulation are defined. This includes populations, population structures (migration rates), etc.
- ```late()``` Last chunk of the code, which marks the end of the simulation, and determines the number of generations the simulation runs for. Any outputs desired at the end of the simulation go in here.

Apart from this, a lot of other modifications can be made to the code, depending on the proficiency of the user. Log files (which I've used a lot) can be created to monitor population/individual parameters across generations. 

# Navigating through the data files
*All the data generated from the simulations can be found here.*
Data has been broadly classified based on the simulations being ran.
- **wf:** Wright Fisher (WF) model simulation results
- **sepsex:** Separate sexes simulation results
- **allFreq:** Simulations measuring mutation counts, which are nothing but the segregating alleles.
 These allele frequencies (mutation counts/2*population size) are measured for the WF model
- **test:** Files containing all the test simulation outputs, can be largely ignored

Within each folder (with the exception of allFreq), there are three types of files: **log_N.txt** , **pi_N.txt** and **.vcf**. The allFreq file has data
in a specific format explained further below.
The first (log file) logs different parameters at the population level across generations.
The second file (pi file) is the same as the log file, with an added column for measuring *pi*.
The third (vcf file) outputs population level sequence data *for the last generation*.

# log, pi and vcf file output

## log_N.txt
where N stands for population size. For example, log_100.txt
*All the parameters are logged every 10 generations*

### Columns and what they mean
1. cycle - Count of the generation in which the parameters are being logged.
2. Heterozygosity - A measure of genetic diversity. This is tabulated using the inbuilt calcHeterozygosity() function in SLiM, which measures the
 expected heterozygosity across the entire genome.
3. Watt_theta - Watterson's theta. Another estimator of genetic diversity based on the number of segregating sites
4. Seg_sites - Segregating sites. Number of sites for which allele frequencies are fluctuating over generations.
5. ReproInds - Number of reproducing individuals, or the number of individuals which contribute to the gene pool for the next generation.

## pi_N.txt
Columns 1-5 are the same as above.
6. Pi - Nucleotide diversity. Average number of nucleotide differences for all possible pairs in a locus (genomic region), across all loci.
## .vcf file
A variant call format (VCF) file gives us information about the variants in all the genomes of a population in a compact manner. The first few lines
are meta-information, characterized by lines starting with two hashes '##'. These tell us what each of the headers of the rest of the file mean, along
with some additional information such as date of outputting the VCF file, file format, etc.
Each row in the file (after the meta-information and the headers) gives us information about a single variant or mutation. It tells us the following:
(this is also the explanation for what the headers stand for)
CHROM - Chromosome on which the mutation occured
POS - Position of the mutation
ID - Mutation ID
REF - The reference nucleotide, or which nucleotide was present before the mutation occured at that locus
ALT - The mutant nucleotide (Eg. if the mutation was from T to A, T is the reference (REF) and A the alternate (ALT) nucleotide)
QUAL - A metric for the quality of the information available for this mutation. Can be ignored, as these are simulated results
FILTER - Specifies whether the data passes certain filter conditions or not. Can be ignored again.
INFO - Gives us some information about the mutation:
        MID: Mutation ID in SLiM
        S: Selection coefficient
        DOM: Dominance coefficient
        PO: Population of origin
        TO: Tick of origin
        MT: Mutation type
        AC: Allele count
        DP: Total depth
        AA: Ancestral allele (Relevant in the rare case of multiple mutations occuring at the same locus)
After that, it gives us information about the mutation in each individual in the format of 0s and 1s, where zero stands for lack of the mutation, and 1
for the presence of it.
For example, 0|0 means both genomes of the individual don't contain the mutation.
Similarly, 1|0 implies the first genome has the mutation, but the second doesn't.
This data also helps us find out the number of heterozygous and homozygous carriers of the mutation.

**NOTE:** The log and .vcf files are from the same simulation (eg. in the 'wf' file, log_100.txt & wf_100.vcf are generated together, in the same
simulation), while the pi_100.txt is from a separate simulation (under the same evolutionary setup).
# allFreq files
**Files contain allele frequency data for all segregating alleles in a population** This uses the SLiM inbuilt outputMutations() call which outputs
population level information for all segregating mutations. Once the mutation is fixed or wiped out, it stops tracking it.

*Allele count data is logged every generation*
## Column-wise explanation for allele frequency data
1 - OUT (plain text)
2 - tick
3 - cycle
4 - Tracked
5 - Subpopulation id
6 - **Mutation ID**
7 - Mutation type
8 - Position/locus of the mutation
9 - Selection coefficient
10 - Dominance coefficient
11 - Subpopulation of origin
12 - Origin tick of the mutation
13 - **Mutation count (Prevalance)**
14 - (In case of nucleotide based simulations) Nucleotide

Most of these parameters aren't useful for us, as we are dealing with simple simulations. There is only a single population & mutation type. The selection
 coefficients are 0, and dominance coefficients 0.5 throughout the simulations. As only a few of these columns are useful, they are filtered out and read
 into another file, with labels starting with **"filt"**, along with the compressed __".gz"__ file(s)
