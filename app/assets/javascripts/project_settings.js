$(function() {
  $('#add_language').autocomplete({
    source: window.languages,
    select: function(event, ui) {
      if (!is_present(ui.item.value)) {
        var list_item = $('#language_proto').clone();
        $('span', list_item).html(ui.item.label + " (" + ui.item.value + ")");
        $('input', list_item).val(ui.item.value);
        $(this).parent().before(list_item);
        add_option_to_select(ui.item);
      }
      $(this).val('');
      return false;
    }
  });
  $('.remove_language').live('click', function(e) {
    e.preventDefault();
    remove_option_from_select($(this).siblings('input').val());
    $(this).parent().remove();
  });
  $('#add_language_button').click(function(e) {
    e.preventDefault();
    $('#add_language').autocomplete('search', $('#add_language').val());
  });
  $('#project_tts_engine').change(function() {
    set_ispeech_options_visibility();
  });

  function set_ispeech_options_visibility() {
    if ($('#project_tts_engine').val() == 'ispeech') {
      $('#project_tts_ispeech_api_key').parent().show('fast');
    } else {
      $('#project_tts_ispeech_api_key').parent().hide('fast');
    }
  }

  function add_option_to_select(item) {
    $('select').append($('<option></option>').val(item.value).html(item.label));
  }

  function remove_option_from_select(value) {
    $('option[value=' + value + ']').remove();
  }

  function is_present(value) {
    return $('option[value=' + value + ']').size() > 0;
  }

  set_ispeech_options_visibility();
});