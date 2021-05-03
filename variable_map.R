library(data.table)

map_sex <- setNames(c(1,2,3,4,5,6,9), c("1: Male", "2: Female", "3: Other(hermaphrodite)", "4: Transsexual, NOS",
                                    "5: Transsexual, natal male", "6: Transsexual, natal female", "9: Not stated"))

map_chemotherapy <- setNames(c(0, 1, 2, 3, 82, 85, 86, 87, 88, 99), 
                             c("0: None, chemotherapy was not part of the planned first course of therapy; diagnosed at autopsy",
                               "1: Chemotherapy administered as first course therapy, but the type and number of agents is not documented in the patient record",
                               "2: Single-agent chemotherapy administered as first course therapy",
                               "3: Multiagent chemotherapy administered as first course therapy",
                               "82: Chemotherapy was not recommended/administered because it was contraindicated due to patient risk factors",
                               "85: Chemotherapy was not administered because the patient died prior to planned or recommended therapy",
                               "86: Chemotherapy was not administered.  It was recommended by the patient\'s physician, but was not administered as part of the first course of therapy.  No reason stated in patient record",
                               "87: Chemotherapy was not administered.  It was recommended by the patient\'s physician,but this treatment was refused by the patient, a patient\'s family member, or the patient\'s guardian",
                               "88: Chemotherapy was recommended, but it is unknown if it was administered",
                               "99:It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in the patient record"))


map_immunotherapy <- setNames(c(0, 1, 82, 85, 86, 87, 88, 99), 
                              c("0: None; immunotherapy was not part of the planned first course of therapy; diagnosed at autopsy",
                                "1: Immunotherapy administered as first course therapy",
                                "82: Immunotherapy was not recommended/administered because it was contraindicated due to patient risk factors",
                                "85: Immunotherapy was not administered because the patient died prior to planned or recommended therapy",
                                "86: Immunotherapy was not administered. It was recommended by the patient\'s physician, but was not administered as part of the first course of therapy. No reason stated in patient record",
                                "87: Immunotherapy was not administered. It was recommended by the patient\'s physician, but this treatment was refused by the patient, a patient\'s family member, or the patient\'s guardian",
                                "88: Immunotherapy was recommended, but it is unknown if it was administered",
                                "99: It is unknown whether an immunotherapeutic agent(s) was recommended or administered because it is not stated in the patient record. Death certificate only"))

map_other_trt <- setNames(c(0, 1, 2, 3, 6, 7, 8, 9), 
                                c("0: None", "1: Other", "2: Other - Experimental", "3: Other - Double Blind", "6: Other - Unproven",
                                  "7: Refusal", "8: Recommended; unknown if administered", "9: Unknown"))

map_seer_stages <- setNames(c(0, 1, 2, 3, 4, 7, 8, 9), 
                        c("In situ", "Localized only", "Regional by direct extension", "Regional to lymph nodes only",
                          "Regional by direct extension and to lymph nodes (combination of codes 2 and 3)",
                          "Distant metastases/systemic disease", "Benign/borderline (brain, CNS other, and intracranial gland only)",
                          "Unstaged, unknown, or unspecified"))