# RadiomicsMM

This repository contains code, models and radiomic features used for the paper "Analysis of clinical, dosimetric and radiomic features for predicting local failure after stereotactic radiotherapy of brain metastases in malignant melanoma". The preprint version of the paper is available here: https://arxiv.org/abs/2405.20825

The nnUnet model for the segmentation of malignant melanoma metastases on T1, which was used in the paper to identify segmentation method dependent features is published separately and available on zenodo: https://zenodo.org/doi/10.5281/zenodo.11402124

Code was tested with R v4.1.2

If you use code, models or data please cite https://zenodo.org/doi/10.5281/zenodo.11402124 and/or https://arxiv.org/abs/2405.20825

---

In this project we tried to estimate the risk of local recurrence after radiotherapy of malignant melanoma brain metastases using MR images and corresponding image features. The radiomic features were computed using pyradiomics v3.0.1. The config-file is called "radiomicsSetting.yaml" and is part of this repository.

Four different types of models were created: 
- clinical models
- dosimetric models
- radiomic features models
- combined models (using clinical, dosimetric and radiomic features)

The models are available in their respective folders. Each folder contains models built with a nested cross validation scheme (which were used to identify relevant features) and a final model built with the entire dataset.

The data folder contains the radiomic features used in this work. Note that all other features (clinical and dosimetric) were set to zero for privacy reasons. 

---
To apply the feature elimination routines described in the paper and to build new models you can run the **RunModelTrainingWithCV.R** script. The script takes three arguments. The first one specifies the output folder, the second one defines the parameters to use (clinic, dose, radiomics, combined) and the last one is used to enable or disable the nested cross validation routine. To build a new model based on dose features without nested cross validation you run the following:

``./RunModelTrainingWithCV.R ./DoseModels/resultsFinalModel/ dose false``

As soon as feature elimination and training has finished you can run the two analysation scripts: 

```
./EvaluateModelsOfCV.R ./DoseModels/resultsFinalModel/

./EvaluateHrOfCvModels.R ./DoseModels/resultsFinalModel/
```

**EvaluateHrOfCvModels.R** creates a featureInfo.txt file which contains the names of all the features that are used in the generated models with the corresponding hazard ratios and the number of folds the respective feature was used in a model.

**EvaluateModelsOfCV.R** creates a info.txt file with information about the models created during training, accuracies in the inner- and outer-loops of the nested cross validation, features with corresponding hazard rations etc. 

---
If you have any questions about the code, models or data please drop me a line. 


