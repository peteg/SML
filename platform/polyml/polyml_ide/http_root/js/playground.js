"use strict";

// ----------------------------------------
// Playground: talk to Poly/ML via messages with the main window.
// FIXME Basically send and receive JSON objects AFAICT. Probably copies things though.
// FIXME this does very little: just forwarding.

function PolyML(handle_user_data) {
  this.request = function(json) {
    parent.postMessage(json, '*');
  };

  window.addEventListener(
    'message',
    function(evt) {
      console.log("playground got data: " + evt.data);

      handle_user_data(evt.data);
    });
};
