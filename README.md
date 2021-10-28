# CHASE
CHASE Tool for HELCOM hazardous Substance status assessment

The CHASE assessment is carried out by calling the function `CHASEassessment` from [CHASE.R](src/CHASE.R)

A required argument for the function is a dataframe containing the assessment indicator data.

| Column | Description | 
| :---         |     :---      |
| *AU* | assessment unit |
| *Matrix* [Required] | Water, Sediment, Biota |
| *Substance* [Required] | name of indicator |
| *Type* [Required] | HM / Org |
| *Threshold* | concentration limit  |
| *Status*    | observed concentration |
| *CR* [Required] | ratio of status to threshold |
| *ConfTemp* | temporal confidence H[igh]/M[oderate]/L[ow] |
| *ConfSpatial* | temporal confidence  H[igh]/M[oderate]/L[ow] |
| *ConfAcc* | accuracy confidence  H[igh]/M[oderate]/L[ow] |
| *ConfMethod* | methodological confidence  H[igh]/M[oderate]/L[ow] |
| *ConfThresh* | threshold confidence H[igh]/M[oderate]/L[ow] |

By default, the function returns 

Read about creation of the test data set [here](test_dataset.md)
