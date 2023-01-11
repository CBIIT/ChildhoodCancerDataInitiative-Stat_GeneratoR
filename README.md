# ChildhoodCancerDataInitiative-Stat_GeneratoR
This takes a validated indexed CCDI template, and/or the newest version of dbGaP submission for the study.


To run the script on a validated CCDI template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CDS-Stat_GeneratoR.R --help
```

```
Usage: CCDI-Stat_GeneratoR.R [options]

CCDI-Stat_GeneratoR v2.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		A validated and indexed CCDI template file (.xlsx)

	-c CHARACTER, --subject_consent=CHARACTER
		A dbGaP subject_consent data file (SC_DS)

	-a CHARACTER, --sample_attribute=CHARACTER
		A dbGaP sample_attribute data file (SA_DS)

	-h, --help
		Show this help message and exit
```

An example set of files have been provided. They can all be used at once or any other combination of the test files:

```
Rscript --vanilla CCDI-Stat_GeneratoR.R -f test_files/a_all_pass_CCDI_Submission_Template_v1.0.2.xlsx -c test_files/set_b/SC_DS_b.txt -a test_files/set_b/SA_DS_b.txt
```

```
The data file is being validated at this time.
This is a validation output for a_all_pass_CCDI_Submission_Template_v1.0.2.



Process Complete.

The output file can be found here: ChildhoodCancerDataInitiative-Stat_GeneratoR/test_files/
```
