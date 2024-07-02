new_column_names = {
    'b1': 'sadness_2007_D',
    'deprb0111': 'sadness_2011_D',
    'b1_12': 'sadness_2012_D',
    'b2': 'pessimism_2007_D',
    'deprb0211': 'pessimism_2011_D',
    'b2_12': 'pessimism_2012_D',
    'b3': 'pastFailure_2007_D',
    'deprb0311': 'pastFailure_2011_D',
    'b3_12': 'pastFailure_2012_D',
    'b4': 'lossOfPleasure_2007_D',
    'deprb0411': 'lossOfPleasure_2011_D',
    'b4_12': 'lossOfPleasure_2012_D',
    'b5': 'guiltyFeelings_2007_D',
    'deprb0511': 'guiltyFeelings_2011_D',
    'b5_12': 'guiltyFeelings_2012_D',
    'b6': 'punishmentFeelings_2007_D',
    'deprb0611': 'punishmentFeelings_2011_D',
    'b6_12': 'punishmentFeelings_2012_D',
    'b7': 'selfDislike_2007_D',
    'deprb0711': 'selfDislike_2011_D',
    'b7_12': 'selfDislike_2012_D',
    'b8': 'selfCriticalness_2007_D',
    'deprb0811': 'selfCriticalness_2011_D',
    'b8_12': 'selfCriticalness_2012_D',
    'b9': 'suicidalThoughtOrWishes_2007_D',
    'deprb0911': 'suicidalThoughtOrWishes_2011_D',
    'b9_12': 'suicidalThoughtOrWishes_2012_D',
    'b10': 'crying_2007_D',
    'deprb1011': 'crying_2011_D',
    'b10_12': 'crying_2012_D',
    'b11': 'agitation_2007_D',
    'deprb1111': 'agitation_2011_D',
    'b11_12': 'agitation_2012_D',
    'b12': 'lossOfInterest_2007_D',
    'deprb1211': 'lossOfInterest_2011_D',
    'b12_12': 'lossOfInterest_2012_D',
    'b13': 'indecisiveness_2007_D',
    'deprb1311': 'indecisiveness_2011_D',
    'b13_12': 'indecisiveness_2012_D',
    'b14': 'worthlessness_2007_D',
    'deprb1411': 'worthlessness_2011_D',
    'b14_12': 'worthlessness_2012_D',
    'b15': 'lossOfEnergy_2007_D',
    'deprb1511': 'lossOfEnergy_2011_D',
    'b15_12': 'lossOfEnergy_2012_D',
    'b16': 'changesInSleepPattern_2007_D',
    'deprb1611': 'changesInSleepPattern_2011_D',
    'b16_12': 'changesInSleepPattern_2012_D',
    'b17': 'irritability_2007_D',
    'deprb1711': 'irritability_2011_D',
    'b17_12': 'irritability_2012_D',
    'b18': 'changesInAppetite_2007_D',
    'deprb1811': 'changesInAppetite_2011_D',
    'b18_12': 'changesInAppetite_2012_D',
    'b19': 'concentrationDifficulty_2007_D',
    'deprb1911': 'concentrationDifficulty_2011_D',
    'b19_12': 'concentrationDifficulty_2012_D',
    'b20': 'tirednessOrFatigue_2007_D',
    'deprb2011': 'tirednessOrFatigue_2011_D',
    'b20_12': 'tirednessOrFatigue_2012_D',
    'b21': 'lossOfInterestInSex_2007_D',
    'deprb2111': 'lossOfInterestInSex_2011_D',
    'b21_12': 'lossOfInterestInSex_2012_D',

    "ika07": "age_2007_CO",
    "ika11": "age_2011_CO",
    "ika12": "age_2012_CO",
    "smoke07": "smoking_2007_CO",
    "smoke11": "smoking_2011_CO",
    "smoke12": "smoking_2012_CO",
    "SP": "sex_2007_CO",
    "SP_x": "sex_2011_CO",
    "SP_y": "sex_2012_CO",
    "bmi07": "bmi_2007_CO",
    "BMI11": "bmi_2011_CO",
    "BMI12": "bmi_2012_CO",

    "Ace07": "acetate_2007_CVD",
    "Ace11": "acetate_2011_CVD",
    "Ace12": "acetate_2012_CVD",
    "apoa107": "apoprotein_2007_CVD",
    "APOA111": "apoprotein_2011_CVD",
    "apoa112": "apoprotein_2012_CVD",
    "crp07": "c-reactiveProtein_2007_CVD",
    "CRP11": "c-reactiveProtein_2011_CVD",
    "crp12": "c-reactiveProtein_2012_CVD",
    "dkv07": "diastolicKV_2007_CVD",
    "DKV11": "diastolicKV_2011_CVD",
    "dkv12": "diastolicKV_2012_CVD",
    "gluk07": "gluk_2007_CVD",
    "GLUK11": "gluk_2011_CVD",
    "gluk12": "gluk_2012_CVD",
    "HDLKOL11": "cholesterolHDL_2007_CVD",
    "hdlkol07": "cholesterolHDL_2011_CVD",
    "hdlkol12": "cholesterolHDL_2012_CVD",
    "insu07": "insu_2007_CVD",
    "INSU11": "insu_2011_CVD",
    "insu12": "insu_2012_CVD",
    "ldlkol07": "cholesterolLDL_2007_CVD",
    "ldlkol11": "cholesterolLDL_2011_CVD",
    "ldlkol12": "cholesterolLDL_2012_CVD",
    "syst07": "systolicBloodPressure_2007_CVD",
    "SYST11": "systolicBloodPressure_2011_CVD",
    "syst12": "systolicBloodPressure_2012_CVD",
    "totkol07": "cholesterolTotal_2007_CVD",
    "TOTKOL11": "cholesterolTotal_2011_CVD",
    "totkol12": "cholesterolTotal_2012_CVD",
    "trigly07": "triglycerides_2007_CVD",
    "TRIGLY11": "triglycerides_2011_CVD",
    "trigly12": "triglycerides_2012_CVD",
}

sorted_column_names = {
    'deprb0111': 'sadness_2011_D',
    'b1': 'sadness_2007_D',
    'b1_12': 'sadness_2012_D',
    'b2': 'pessimism_2007_D',
    'deprb0211': 'pessimism_2011_D',
    'b2_12': 'pessimism_2012_D',
    'b3': 'pastFailure_2007_D',
    'deprb0311': 'pastFailure_2011_D',
    'b3_12': 'pastFailure_2012_D',
    'b4': 'lossOfPleasure_2007_D',
    'deprb0411': 'lossOfPleasure_2011_D',
    'b4_12': 'lossOfPleasure_2012_D',
    'b5': 'guiltyFeelings_2007_D',
    'deprb0511': 'guiltyFeelings_2011_D',
    'b5_12': 'guiltyFeelings_2012_D',
    'b6': 'punishmentFeelings_2007_D',
    'deprb0611': 'punishmentFeelings_2011_D',
    'b6_12': 'punishmentFeelings_2012_D',
    'b7': 'selfDislike_2007_D',
    'deprb0711': 'selfDislike_2011_D',
    'b7_12': 'selfDislike_2012_D',
    'b8': 'selfCriticalness_2007_D',
    'deprb0811': 'selfCriticalness_2011_D',
    'b8_12': 'selfCriticalness_2012_D',
    'b9': 'suicidalThoughtOrWishes_2007_D',
    'deprb0911': 'suicidalThoughtOrWishes_2011_D',
    'b9_12': 'suicidalThoughtOrWishes_2012_D',
    'b10': 'crying_2007_D',
    'deprb1011': 'crying_2011_D',
    'b10_12': 'crying_2012_D',
    'b11': 'agitation_2007_D',
    'deprb1111': 'agitation_2011_D',
    'b11_12': 'agitation_2012_D',
    'b12': 'lossOfInterest_2007_D',
    'deprb1211': 'lossOfInterest_2011_D',
    'b12_12': 'lossOfInterest_2012_D',
    'b13': 'indecisiveness_2007_D',
    'deprb1311': 'indecisiveness_2011_D',
    'b13_12': 'indecisiveness_2012_D',
    'b14': 'worthlessness_2007_D',
    'deprb1411': 'worthlessness_2011_D',
    'b14_12': 'worthlessness_2012_D',
    'b15': 'lossOfEnergy_2007_D',
    'deprb1511': 'lossOfEnergy_2011_D',
    'b15_12': 'lossOfEnergy_2012_D',
    'b16': 'changesInSleepPattern_2007_D',
    'deprb1611': 'changesInSleepPattern_2011_D',
    'b16_12': 'changesInSleepPattern_2012_D',
    'b17': 'irritability_2007_D',
    'deprb1711': 'irritability_2011_D',
    'b17_12': 'irritability_2012_D',
    'b18': 'changesInAppetite_2007_D',
    'deprb1811': 'changesInAppetite_2011_D',
    'b18_12': 'changesInAppetite_2012_D',
    'b19': 'concentrationDifficulty_2007_D',
    'deprb1911': 'concentrationDifficulty_2011_D',
    'b19_12': 'concentrationDifficulty_2012_D',
    'b20': 'tirednessOrFatigue_2007_D',
    'deprb2011': 'tirednessOrFatigue_2011_D',
    'b20_12': 'tirednessOrFatigue_2012_D',
    'b21': 'lossOfInterestInSex_2007_D',
    'deprb2111': 'lossOfInterestInSex_2011_D',
    'b21_12': 'lossOfInterestInSex_2012_D',

    "Ace07": "acetate_2007_CVD",
    "Ace11": "acetate_2011_CVD",
    "Ace12": "acetate_2012_CVD",
    "apoa107": "apoprotein_2007_CVD",
    "APOA111": "apoprotein_2011_CVD",
    "apoa112": "apoprotein_2012_CVD",
    "crp07": "c-reactiveProtein_2007_CVD",
    "CRP11": "c-reactiveProtein_2011_CVD",
    "crp12": "c-reactiveProtein_2012_CVD",
    "dkv07": "diastolicKV_2007_CVD",
    "DKV11": "diastolicKV_2011_CVD",
    "dkv12": "diastolicKV_2012_CVD",
    "gluk07": "gluk_2007_CVD",
    "GLUK11": "gluk_2011_CVD",
    "gluk12": "gluk_2012_CVD",
    "HDLKOL11": "cholesterolHDL_2007_CVD",
    "hdlkol07": "cholesterolHDL_2011_CVD",
    "hdlkol12": "cholesterolHDL_2012_CVD",
    "insu07": "insu_2007_CVD",
    "INSU11": "insu_2011_CVD",
    "insu12": "insu_2012_CVD",
    "ldlkol07": "cholesterolLDL_2007_CVD",
    "ldlkol11": "cholesterolLDL_2011_CVD",
    "ldlkol12": "cholesterolLDL_2012_CVD",
    "syst07": "systolicBloodPressure_2007_CVD",
    "SYST11": "systolicBloodPressure_2011_CVD",
    "syst12": "systolicBloodPressure_2012_CVD",
    "totkol07": "cholesterolTotal_2007_CVD",
    "TOTKOL11": "cholesterolTotal_2011_CVD",
    "totkol12": "cholesterolTotal_2012_CVD",
    "trigly07": "triglycerides_2007_CVD",
    "TRIGLY11": "triglycerides_2011_CVD",
    "trigly12": "triglycerides_2012_CVD",
}
