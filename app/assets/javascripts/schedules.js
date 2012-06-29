$(function() {
  $('ul.days li').live('click', function(){
    var li = $(this);
    var ul = li.parent();
    var input = ul.children('input');
    li.toggleClass('no');
    var days = [];
    ul.children('li').each(function(index, value){
      if (!$(value).hasClass('no')) {
        days.push(index);
      }
    });
    input.val(days.join());
  });
});
