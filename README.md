# CHASE
CHASE Tool for HELCOM hazardous Substance status assessment

The CHASE assessment is carried out by calling the function `CHASEassessment` from [CHASE.R](src/CHASE.R)

A required argument for the function is a dataframe containing the assessment indicator data.

| Column | Required | Description | 
| :---         |     :---      |     :---      |
| *AU* | NO<sup>1</sup>  | assessment unit |
| *Matrix* | YES | Water, Sediment, Biota |
| *Substance* | YES | name of indicator |
| *Type* | YES | HM / Org |
| *Threshold* | NO | concentration limit  |
| *Status*    | NO | observed concentration |
| *CR* | YES<sup>2</sup> | ratio of status to threshold |
| *ConfTemp* | NO<sup>3</sup> | temporal confidence H[igh]/M[oderate]/L[ow] |
| *ConfSpatial* | NO<sup>3</sup> | temporal confidence  H[igh]/M[oderate]/L[ow] |
| *ConfAcc* | NO<sup>3</sup> | accuracy confidence  H[igh]/M[oderate]/L[ow] |
| *ConfMethod* | NO<sup>3</sup> | methodological confidence  H[igh]/M[oderate]/L[ow] |
| *ConfThresh* | NO<sup>3</sup>| threshold confidence H[igh]/M[oderate]/L[ow] |

By default, the function returns 

Read about creation of the test data set [here](test_dataset.md)
