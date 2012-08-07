$(function() {
  $('.add_project_variable .ux-optext').live('click', function(){
    $(this).next(".clist-add").click();
  });

  $('.edit_variable.language').autocomplete({
    source: window.languages
  });
});