class AlertsModel
  constructor: ->
    @alerts = ko.observableArray()
    window.setInterval(@reload, 60 * 1000)
    @reload()

  reload: =>
    $.get('/alerts.json', null, @alerts)

  open: (x) ->
    window.location.href = "/alerts/#{x.id}"

$ ->
  $alerts = $('#alerts')
  if $alerts.length > 0
    $alertsBox = $('#alerts .box')
    $alerts.click (event) ->
      $alertsBox.toggleClass('visible')
      event.stopPropagation()

    $('html').click ->
      if ($alertsBox.hasClass('visible'))
        $alertsBox.removeClass('visible')

    ko.applyBindings(new AlertsModel(), document.getElementById('alerts'))
