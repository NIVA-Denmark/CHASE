# CHASE
CHASE Tool for HELCOM hazardous Substance status assessment

The CHASE assessment is carried out by calling the function `CHASEassessment` from [CHASE.R](src/CHASE.R)

A required argument for the function is a dataframe containing the assessment indicator data.

| Column | Required | Description | 
| :---         |     :---      |
| *AU* |  | assessment unit |
| *Matrix* | YES | Water, Sediment, Biota |
| *Substance* | YES | name of indicator |
| *Type* | YES | HM / Org |
| *Threshold* |  | concentration limit  |
| *Status*    |  | observed concentration |
| *CR* | YES<sup>1<sup> | ratio of status to threshold |
| *ConfTemp* |  | temporal confidence H[igh]/M[oderate]/L[ow] |
| *ConfSpatial* |  | temporal confidence  H[igh]/M[oderate]/L[ow] |
| *ConfAcc* |  | accuracy confidence  H[igh]/M[oderate]/L[ow] |
| *ConfMethod* |  | methodological confidence  H[igh]/M[oderate]/L[ow] |
| *ConfThresh* |  | threshold confidence H[igh]/M[oderate]/L[ow] |

By default, the function returns 

Read about creation of the test data set [here](test_dataset.md)
