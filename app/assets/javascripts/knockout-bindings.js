/*
  Jquery simple Autocomplete Binding Handler, only inits autocomplete, does not update it
*/
ko.bindingHandlers.initAutocomplete = {
  init: function (element, params) {
      $(element).autocomplete(params());
  },
};

/*
  Jquery simple meio mask binding handler, only inits meio mask, does not update it
*/
ko.bindingHandlers.initMask = {
  init: function (element, params) {
    $(element).setMask(params());
  },
};

/*
  JQuery File Upload binding handler
*/
ko.bindingHandlers.fileupload = {
  init: function(element, valueAccessor, allBindingsAccessor, viewModel) {
    var url = ko.utils.unwrapObservable(valueAccessor());
    var allBindings = allBindingsAccessor();
    $(element).fileupload({url: url, multipart: false, formData: {method: 'post'}});
    if (allBindings.fileuploadAdd) {
      $(element).bind('fileuploadadd', allBindings.fileuploadAdd);
    }
    if (allBindings.fileuploadSubmit) {
      $(element).bind('fileuploadsubmit', allBindings.fileuploadSubmit);
    }
    if (allBindings.fileuploadprogressall) {
      $(element).bind('fileuploadprogressall', allBindings.fileuploadprogressall);
    }
  },
  update: function(element, valueAccessor, allBindingsAccessor, viewModel) {
    var url = ko.utils.unwrapObservable(valueAccessor());
    $(element).fileupload('option', 'url', url);
  }
}


/*
  Jquery Autocomplete Binding Handler (not-tested)
  http://stackoverflow.com/questions/7537002/autocomplete-combobox-with-knockout-js-template-jquery
  http://jsfiddle.net/rniemeyer/YNCTY/

  jqAuto -- main binding (should contain additional options to pass to autocomplete)
  jqAutoSource -- the array of choices
  jqAutoValue -- where to write the selected value
  jqAutoSourceLabel -- the property that should be displayed in the possible choices
  jqAutoSourceInputValue -- the property that should be displayed in the input box
  jqAutoSourceValue -- the property to use for the value
*/
ko.bindingHandlers.jqAuto = {
    init: function(element, valueAccessor, allBindingsAccessor, viewModel) {
        var options = valueAccessor() || {},
            allBindings = allBindingsAccessor(),
            unwrap = ko.utils.unwrapObservable,
            modelValue = allBindings.jqAutoValue,
            source = allBindings.jqAutoSource,
            query = allBindings.jqAutoQuery,
            valueProp = allBindings.jqAutoSourceValue,
            inputValueProp = allBindings.jqAutoSourceInputValue || valueProp,
            labelProp = allBindings.jqAutoSourceLabel || inputValueProp;

        //function that is shared by both select and change event handlers
        function writeValueToModel(valueToWrite) {
            if (ko.isWriteableObservable(modelValue)) {
               modelValue(valueToWrite );
            } else {  //write to non-observable
               if (allBindings['_ko_property_writers'] && allBindings['_ko_property_writers']['jqAutoValue'])
                        allBindings['_ko_property_writers']['jqAutoValue'](valueToWrite );
            }
        }

        //on a selection write the proper value to the model
        options.select = function(event, ui) {
            writeValueToModel(ui.item ? ui.item.actualValue : null);
        };

        //on a change, make sure that it is a valid value or clear out the model value
        options.change = function(event, ui) {
            var currentValue = $(element).val();
            var matchingItem =  ko.utils.arrayFirst(unwrap(source), function(item) {
               return unwrap(item[inputValueProp]) === currentValue;
            });

            if (!matchingItem) {
               writeValueToModel(null);
            }
        }

        //hold the autocomplete current response
        var currentResponse = null;

        //handle the choices being updated in a DO, to decouple value updates from source (options) updates
        var mappedSource = ko.dependentObservable({
            read: function() {
                    mapped = ko.utils.arrayMap(unwrap(source), function(item) {
                        var result = {};
                        result.label = labelProp ? unwrap(item[labelProp]) : unwrap(item).toString();  //show in pop-up choices
                        result.value = inputValueProp ? unwrap(item[inputValueProp]) : unwrap(item).toString();  //show in input box
                        result.actualValue = valueProp ? unwrap(item[valueProp]) : item;  //store in model
                        return result;
                });
                return mapped;
            },
            write: function(newValue) {
                source(newValue);  //update the source observableArray, so our mapped value (above) is correct
                if (currentResponse) {
                    currentResponse(mappedSource());
                }
            }
        });

        if (query) {
            options.source = function(request, response) {
                currentResponse = response;
                query.call(this, request.term, mappedSource);
            }
        } else {
            //whenever the items that make up the source are updated, make sure that autocomplete knows it
            mappedSource.subscribe(function(newValue) {
               $(element).autocomplete("option", "source", newValue);
            });

            options.source = mappedSource();
        }


        //initialize autocomplete
        $(element).autocomplete(options);
    },
    update: function(element, valueAccessor, allBindingsAccessor, viewModel) {
       //update value based on a model change
       var allBindings = allBindingsAccessor(),
           unwrap = ko.utils.unwrapObservable,
           modelValue = unwrap(allBindings.jqAutoValue) || '',
           valueProp = allBindings.jqAutoSourceValue,
           inputValueProp = allBindings.jqAutoSourceInputValue || valueProp;

       //if we are writing a different property to the input than we are writing to the model, then locate the object
       if (valueProp && inputValueProp !== valueProp) {
           var source = unwrap(allBindings.jqAutoSource) || [];
           var modelValue = ko.utils.arrayFirst(source, function(item) {
                 return unwrap(item[valueProp]) === modelValue;
           }) || {};
       }

       //update the element with the value that should be shown in the input
       $(element).val(modelValue && inputValueProp !== valueProp ? unwrap(modelValue[inputValueProp]) : modelValue.toString());
    }
};

/*
  Select optgroups binding handler
  https://github.com/SteveSanderson/knockout/pull/94
*/
ko.bindingHandlers["groupedOptions"] = {
    init: function (element, valueAccessor, allBindingsAccessor)
    {
        if (element.tagName != "SELECT")
            throw new Error("options binding applies only to SELECT elements");

        var previousSelectedValues = ko.utils.arrayMap(ko.utils.arrayFilter(element.childNodes, function (node)
        {
            return node.tagName && node.tagName == "OPTION" && node.selected;
        }), function (node)
        {
            return ko.selectExtensions.readValue(node) || node.innerText || node.textContent;
        });
        var previousScrollTop = element.scrollTop;

        var value = ko.utils.unwrapObservable(valueAccessor());
        var selectedValue = element.value;

        if (value)
        {
            var allBindings = allBindingsAccessor();
            if (typeof value.length != "number")
                value = [value];
            if (allBindings['optionsCaption'])
            {
                var option = document.createElement("OPTION");
                option.innerHTML = allBindings['optionsCaption'];
                ko.selectExtensions.writeValue(option, undefined);
                element.appendChild(option);
            }

            var optionsGroupNamesValue = allBindings['optionsGroupNames'];

            // Group values into optgroups
            var groupedOptions = [];
            var optionsGroupValue = allBindings['optionsGroup']; // undefined if not given
            for (var i = 0, j = value.length; i < j; i++)
            {
                var optionsGroup = null;
                if (typeof optionsGroupValue == "function")
                    optionsGroup = optionsGroupValue(value[i]);
                else if (typeof optionsGroupValue == "string")
                    optionsGroup = value[i][optionsGroupValue];
                else
                    optionsGroup = "";
                if (typeof groupedOptions[optionsGroup] == "undefined")
                    groupedOptions[optionsGroup] = [];
                groupedOptions[optionsGroup].push(value[i]);
            }

            // Create HTML elements
            for (var groupName in groupedOptions)
            {
                var optgroup = null;
                // Add an OPTGROUP for all groups except for ""
                if (groupName != "")
                {
                    optgroup = document.createElement("OPTGROUP");
                    optgroup.label = groupName;
                    element.appendChild(optgroup);
                }

                // Create HTML elements for options within this group
                for (var i = 0, j = groupedOptions[groupName].length; i < j; i++)
                {
                    var valueGroup = groupedOptions[groupName];
                    var option = document.createElement("OPTION");
                    var optionValue = typeof allBindings['optionsValue'] == "string" ? valueGroup[i][allBindings['optionsValue']] : valueGroup[groupName][i];

                    // Pick some text to appear in the drop-down list for this data value
                    var optionsTextValue = allBindings['optionsText'];
                    if (typeof optionsTextValue == "function")
                        optionText = optionsTextValue(valueGroup[i]); // Given a function; run it against the data value
                    else if (typeof optionsTextValue == "string")
                        optionText = valueGroup[i][optionsTextValue]; // Given a string; treat it as a property name on the data value
                    else
                        optionText = optionValue; // Given no optionsText arg; use the data value itself

                    optionValue = ko.utils.unwrapObservable(optionValue);
                    optionText = ko.utils.unwrapObservable(optionText);
                    ko.selectExtensions.writeValue(option, optionValue);

                    option.innerHTML = optionText.toString();

                    if (optgroup != null)
                        optgroup.appendChild(option);
                    else
                        element.appendChild(option);
                }
            }

            // IE6 doesn't like us to assign selection to OPTION nodes before they're added to the document.
            // That's why we first added them without selection. Now it's time to set the selection.
            var newOptions = element.getElementsByTagName("OPTION");
            var countSelectionsRetained = 0;
            for (var i = 0, j = newOptions.length; i < j; i++)
            {
                if (ko.utils.arrayIndexOf(previousSelectedValues, ko.selectExtensions.readValue(newOptions[i])) >= 0)
                {
                    ko.utils.setOptionNodeSelectionState(newOptions[i], true);
                    countSelectionsRetained++;
                }
            }

            if (previousScrollTop)
                element.scrollTop = previousScrollTop;
        }
    }
};


ko.bindingHandlers['selectedOptions'] = {
    getSelectedValuesFromSelectNode: function (selectNode)
    {
        var result = [];
        var nodes = selectNode.childNodes;
        for (var i = 0, j = nodes.length; i < j; i++)
        {
            var node = nodes[i];
            if ((node.tagName == "OPTGROUP") && node.childNodes != null)
            {
                var subResult = this.getSelectedValuesFromSelectNode(node);
                for (var k = 0; k < subResult.length; k++)
                {
                    result.push(subResult[k]);
                }
            }
            else
            {
                if ((node.tagName == "OPTION") && node.selected)
                    result.push(ko.selectExtensions.readValue(node));
            }
        }
        return result;
    },
    setSelectedValuesFromSelectNode: function (selectNode, newValue)
    {
        var nodes = selectNode.childNodes;
        for (var i = 0, j = nodes.length; i < j; i++)
        {
            var node = nodes[i];
            if (node.tagName == "OPTGROUP" && node.childNodes != null)
            {
                ko.bindingHandlers['selectedOptions'].setSelectedValuesFromSelectNode(node, newValue);
            }
            else
            {
                if (node.tagName == "OPTION")
                    ko.utils.setOptionNodeSelectionState(node, ko.utils.arrayIndexOf(newValue, ko.selectExtensions.readValue(node)) >= 0);
            }
        }
    },
    'init': function (element, valueAccessor, allBindingsAccessor)
    {
        ko.utils.registerEventHandler(element, "change", function ()
        {
            var value = valueAccessor();
            if (ko.isWriteableObservable(value))
                value(ko.bindingHandlers['selectedOptions'].getSelectedValuesFromSelectNode(this));
            else
            {
                var allBindings = allBindingsAccessor();
                if (allBindings['_ko_property_writers'] && allBindings['_ko_property_writers']['value'])
                    allBindings['_ko_property_writers']['value'](ko.bindingHandlers['selectedOptions'].getSelectedValuesFromSelectNode(this));
            }
        });
    },
    'update': function (element, valueAccessor)
    {
        if (element.tagName != "SELECT")
            throw new Error("values binding applies only to SELECT elements");

        var newValue = ko.utils.unwrapObservable(valueAccessor());
        if (newValue && typeof newValue.length == "number")
        {
            ko.bindingHandlers['selectedOptions'].setSelectedValuesFromSelectNode(element, newValue);
        }
    }
};
