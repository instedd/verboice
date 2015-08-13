$(function() {
  $('.add_project_variable .ux-optext').live('click', function(){
    $(this).next(".clist-add").click();
  });

  $('.edit_variable.language').autocomplete({
    source: window.languages
  });

  $('ul.addresses').each(function(index, element) {
    var root = $(element);

    function nonDestroyed() {
      return $('li', root).filter(function() {
        return $(this).find('input[name$="[_destroy]"]').val() == 'false';
      });
    }

    root.on('click', '.clist-remove', function() {
      if (nonDestroyed().length > 1) {
        var li = $(this).parents('li').first();
        li.find('input[name$="[_destroy]"]').val(true);
        li.hide();
      }
    });

    root.on('keydown', '.new_address', function(evt) {
      if (evt.keyCode == 13) {
        evt.preventDefault();
        root.find('.clist-add').click();
      }
    });

    root.on('click', '.clist-add', function() {
      var addr_field = root.find('.new_address');
      var addr = addr_field.val().trim();

      if (addr) {
        var template = $(this).data('template');
        var new_id = new Date().getTime();
        template = template.replace(/__INDEX__/g, new_id);

        var last_li = root.find('li').last();
        last_li.before(template);
        last_li.prev('li').find('input[name$="[address]"]').val(addr);
        addr_field.val('');
      }
    });
  });

  $('.call-selectors input[type="radio"]').on('change',function(){
      window.location = $(this).data('path');
  });

  $('.file-upload .choose-button').click(function (elem) {
    $(this).closest('form').find('.choose').click();
  });

  $('input.choose').change(function (elem) {
    $(this).closest('form').find('#choose-file').val(this.files[0].name);
  });

  $('.file-upload .import').click(function (elem) {
    $(this).closest('form').submit();
  });
});

function initContactsFilter(count) {
  $(function() {
    var filters = JSON.parse($('.filters').val());
    var model = window.contactsFilter = new ContactsFilter(filters, count);
    ko.applyBindings(model, document.getElementById('contactsFilter'));
    if (model.filters().length > 0) {
      $('.search form').submit();
    }
  });
}

