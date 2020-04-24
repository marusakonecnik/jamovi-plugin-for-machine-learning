
const events = {
    update: function (ui) {
        console.log('update was called');

        ui.classifierSettings.$input.css({"width" : "400px"});

        let classifiers = ui.classifiersToUse.getControls();

        classifiers.forEach(classifier => classifier.$label.removeClass('silky-list-item-value'));



    },

    onRemove_classifiersList: function(ui) {

    },

    onChange_classifiersList: function (ui) {
        console.log("onChane_classifiersList was called")
        let classifiersList = this.base.clone(ui.classifiersToUse.value());
        let lastElementIndex = classifiersList.length - 1;
        let settings = "";

        if(ui.classifierSettings.getValue() != null) {
            settings = ui.classifierSettings.getValue();
        }

        classifiersList[lastElementIndex] = `${classifiersList[lastElementIndex]} (${settings})`;

        ui.classifiersToUse.getControls()[lastElementIndex].$label.removeClass('silky-list-item-value');

        ui.classifiersToUse.setValue(classifiersList);

        ui.classifierSettings.setValue("");

    },

    onUpdate_classifiersSupplier: function (ui) {
        console.log("on update classifiersSupplier was called")
        let availableClassifiers = ["Decision tree", "Random forest", "KNN", 'Logistic regression', 'Naive bayes'];

        ui.classifiers.setValue(this.base.valuesToItems(availableClassifiers, FormatDef.variable));

        this.cloneArray

    },

};

var update_classifiersSupplier = function(ui, context) {
    let availableClassifiers = ["Decision tree", "Random forest", "KNN", 'Logistic regression', 'Naive bayes'];

    ui.classifiers.setValue(context.base.valuesToItems(availableClassifiers, FormatDef.variable));

};

var filterClassifiersToUse = function(ui, context) {
    var termsList = context.base.clone(ui.classifiersToUse.value());
    var diff = context.base.findChanges("classifierstoUse", termsList, true, FormatDef.term);

    var changed = false;
    if (diff.removed.length > 0) {
        var itemsRemoved = false;
        for (var i = 0; i < diff.removed.length; i++) {
            var item = diff.removed[i];
            for (var j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], item)) {
                    termsList.splice(j, 1);
                    j -= 1;
                    itemsRemoved = true;
                }
            }
        }

        if (itemsRemoved)
            changed = true;
    }

    if (context.base.sortArraysByLength(termsList))
        changed = true;

    if (changed)
        ui.rmTerms.setValue(termsList);

   update_classifiersSupplier(ui, context);
};

module.exports = events;