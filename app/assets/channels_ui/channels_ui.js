function navigate(url) {
  window.location = url;
}

function sendCancel() {
  parent.postMessage({type: 'cancel'}, "*");
}

function sendCreated(channel) {
  parent.postMessage({type: 'created', channel: channel}, "*");
}

window.onload = function() {
  parent.postMessage({type: 'resize', height: document.documentElement.scrollHeight}, "*");
}
