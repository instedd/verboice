$(function() {
  $mode = $('#application_mode');
  $callback_url_container = $('#callback_url_container');
  $flow_container = $('#flow_container');
  $flow_actions = $('#flow_actions');
  $add_flow_action = $('#add_flow_action');
  $application_flow = $('#application_flow');

  $mode.change(function() {
    if ($mode.val() == 'callback_url') {
      $callback_url_container.show();
      $flow_container.hide();
    } else {
      $callback_url_container.hide();
      $flow_container.show();
    }
  });

  $flow_actions.append('<option>Select an action to add...</option>');
  for(var name in commands) {
    $flow_actions.append('<option>' + name + '</option>');
  }

  function add_command(name, args) {
    var args_html = '<input type="hidden" name="application[flow][][name]" value="' + name + '"/>';
    for(var i in args) {
      var arg = args[i];
      args_html += ' &nbsp; ';
      args_html += arg.name;
      args_html += ': ';
      args_html += '<input type="text"';
      args_html += 'name="application[flow][][' + arg.name + ']"';
      if (arg['default']) {
        args_html += 'value="' + arg['default'] + '"';
      }
      if (arg.ui_length) {
        args_html += 'size="' + arg.ui_length + '"';
      }
      args_html += '/>';
    }
    var $li = $('<li><b>' + name + '</b>' + args_html + ' &nbsp; <a href="javascript:void(0)">remove</a>' + '</li>');
    $li.find('a').click(function() { $li.remove(); });

    $application_flow.append($li);
  }

  function clone(obj){
    if(obj == null || typeof(obj) != 'object') return obj;
    var temp = new obj.constructor();
    for(var key in obj) temp[key] = clone(obj[key]);
    return temp;
  }

  $flow_actions.change(function() {
    var name = $flow_actions.val();
    var options = commands[name];
    add_command(name, options);
    $flow_actions.find('option').get(0).selected = 'selected';
  });

  // Init flow
  if (flow) {
    for(var i in flow) {
      var cmd = flow[i];
      if (typeof(cmd) == 'string') {
        add_command(cmd, []);
      } else {
        for(var name in cmd) break;
        var values = cmd[name];
        var options = clone(commands[name]);
        if (options.length == 1) {
          options[0]['default'] = values;
        } else {
          for(var j in values) {
            for(var k in options) {
              if (options[k].name == j) break
            }
            options[k]['default'] = values[j];
          }
        }
        add_command(name, options);
      }
    }
  }
})
