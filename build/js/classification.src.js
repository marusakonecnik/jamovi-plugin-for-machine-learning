
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"dep","title":"Dependent Variable","type":"Variable"},{"name":"indep","title":"Independent Variables","type":"Variables"},{"name":"testSize","title":"Testing","type":"Number"},{"name":"noOfFolds","title":"No. of folds","type":"Number"},{"name":"testing","title":"Testing","type":"List","options":[{"name":"trainSet","title":"Train set"},{"name":"split","title":"Train/test split"},{"name":"crossValidation","title":"Cross-validation"}]},{"name":"reporting","type":"NMXList","title":"Reporting","options":[{"name":"classificationMatrix","title":"classification Matrix"},{"name":"confusionMatrix","title":"confusion Matrix"},{"name":"AUC","title":"AUC"}]},{"name":"classifier","title":"Classifier","type":"List","options":[{"name":"simpleDecisionTree","title":"Simple Decision Tree"},{"name":"randomForest","title":"random forest"}]}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Decision tree",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Dependent Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Independent Variables",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "indep",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			style: "inline",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Testing",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "large",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "testing_train",
									optionName: "testing",
									optionPart: "trainSet"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "testing_split",
									optionName: "testing",
									optionPart: "split",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "testSize",
											format: FormatDef.number
										}
									]
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "testing_crossValidation",
									optionName: "testing",
									optionPart: "crossValidation",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "noOfFolds",
											format: FormatDef.number
										}
									]
								}
							]
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Reporting",
					controls: [
						{
							name: "reporting_classificationMatrix",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "reporting",
							optionPart: "classificationMatrix"
						},
						{
							name: "reporting_confusionMatrix",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "reporting",
							optionPart: "confusionMatrix"
						},
						{
							name: "reporting_AUC",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "reporting",
							optionPart: "AUC"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Classifier",
					controls: [
						{
							name: "classifier_singleDecisionTree",
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							optionName: "classifier",
							optionPart: "simpleDecisionTree"
						},
						{
							name: "classifier_randomForest",
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							optionName: "classifier",
							optionPart: "randomForest"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
