// ----------------------------------------
// Poly/ML IDE.

"use strict";

// FIXME
// namespace pollution.
// make the JSON uses more robust: check for properties before dispatching on them.
// FIXME try to abstract from CodeMirror and the rest of the UI as much as possible.
//   - introduce an object to mediate between the communication and the UI.

function Polyml_ide(log, ui) {
  var polyml_ide_url = 'ws://' + window.location.host + '/polyml_ide'; // FIXME parameterize
  var socket = new WebSocket(polyml_ide_url);

  // FIXME Seems plausible to just use dates as requestIds... but what are they in JS?
  var request_id; // most recent request ID.

  function send_request(json) {
    if(typeof json.requestId === "undefined") {
      request_id = Date.now().toString();
      json.requestId = request_id;
    }
    var msg = JSON.stringify(json);
    socket.send(msg);
    log("Sent '" + msg + "'");
  };

  function hello() {
    send_request({
      tag: "Hello",
    });
  };

  function compile_request() {
    send_request({
      tag: "Compile",
      fileName: "JS IDE",
      startPosition: 0,
      preludeCode: "open Polyml_protocol.IO",
      sourceCode: ui.get_code()
    });
    ui.compile_run();
  };

  function compile_response_compile_errors(compile_errors) {
    ui.compiler_clear_annotations();
    var hard_error = false;

    if(compile_errors.length > 0) {
      compile_errors.map(function(compile_error) {
        var loc = compile_error.location;
        var class_name;
        if(compile_error.hardError) {
          class_name =  "compile_error";
          hard_error = true;
        } else {
          class_name = "compile_warning";
        };
        ui.compiler_annotate(loc.startPosition, loc.endPosition, class_name, compile_error.message);
      });
    };

    // FIXME somehow indicate a successful compilation.
    ui.compile_quiescent(hard_error ? "compile_failed" : "compile_success");
  };

  function compile_response_runtime_exception(exn, compile_errors) {
    alert("FIXME code exploded at runtime, after compilation: " + exn);

    if(!(compile_errors == "")) {
      compile_response_compile_errors(compile_errors);
    } else {
      ui.compile_quiescent("compile_quiescent");
    };
  };

  function compile_response(result) {
    switch (result.tag) {
    case "CompileFail":
    case "Succeeded":
      compile_response_compile_errors(result.compile_errors);
      break;

    case "RuntimeException":
      compile_response_runtime_exception(result.string, result.compile_errors);
      break;
    };
  };

  function compile_kill_request() {
    send_request({
      tag: "Kill",
      requestId: request_id
    });
  };

  // FIXME external API, perhaps move, make a method?
  function send_user_request(json) {
    send_request({
      tag: "UserRequest",
      payload: json
    });
  };

  function hello_response() {
    ui.attach_event_handlers({
      compile_request: compile_request,
      compile_kill_request: compile_kill_request,
      send_user_request: send_user_request,
    });
  };

  // Socket event handlers.

  function socket_onopen() {
    log('Connected to polyml_ide.');
    hello();
  };

  function socket_onclose() {
    log('Disconnected from polyml_ide.');
  };

  function socket_onerror(error) {
    log('polyml_ide WebSocket Error ' + error.message);
  };

  function socket_onmessage(msg) {
    log('Got data from ' + polyml_ide_url);
    var json = JSON.parse(msg.data);

    log(" >> Data: '" + JSON.stringify(json) + "'");
    switch (json.tag) {
    case "Hello":
      hello_response ();
      break;

    case "CompilerResponse":
      // FIXME refine, scope, connect with freshness of editor, ...
      compile_response(json.result);
      break;

    case "UserData":
      ui.handle_user_data(json.payload);
      break;
    };
  };

  // Attach event handlers to the socket.
  // FIXME reattach if we get disconnected?

  socket.onopen = socket_onopen;
  socket.onclose = socket_onclose;
  socket.onerror = socket_onerror;
  socket.onmessage = socket_onmessage;
};

// ----------------------------------------

// FIXME nuke all attempts at OO here?
function UI(log, editor, set_status) {
  var playground_iframe = document.getElementById("playground");

  var compile_button = document.getElementById("compile");
  var compile_kill_button = document.getElementById("compile_kill");
  var clear_playground_button = document.getElementById("clear_playground");

  var send_user_request = null;

  this.get_code = function() { return editor.getValue(); };

  // FIXME should this be tied to killing the compilation?
  // FIXME call this before firing off a compilation request?
  function clear_playground() {
    // Just nuke the contents of the iframe.
    playground_iframe.src = "about:blank";
    playground_iframe.srcdoc = "";
    set_status("Playground cleared.");
  };

  // Compiler state.

  this.compile_run = function() {
    // FIXME somehow indicate that we're waiting for the compiler.
    compile_button.className = "compile_waiting";
    compile_kill_button.disabled = false;
  };

  this.compile_quiescent = function(compile_button_css_class) {
    compile_button.className = compile_button_css_class;
    compile_kill_button.disabled = true;
  };

  var marks = [];
  var next_mark = 0;
  this.compiler_clear_annotations = function() {
    marks.map(function(mark) {
      mark.clear();
    });
    marks = [];
    next_mark = 0;

    // FIXME only if it is currently showing an error or other possibly stale stuff:
    set_status("");
  };

  this.compiler_annotate = function(start, end, class_name, message) {
    var start_pos = editor.posFromIndex(start);
    var end_pos = editor.posFromIndex(end);
    var mark = editor.markText(start_pos, end_pos, {className: class_name});
    marks.push(mark);

    // FIXME add pos info
    // FIXME retain formatting
    mark.on("beforeCursorEnter", function() {
      set_status(message);
    });

    // FIXME if the cursor is currently in the mark...
  };

  function compile_next_mark() {
    if(next_mark < marks.length) {
      var mark = marks[next_mark];
      next_mark++;
      // FIXME move into the mark, show the error/warning.
      editor.setCursor(mark.find().from);
    }
  }

  // User-level stuff.

  window.addEventListener(
    'message',
    function(evt) {
      // The sandboxed iframe lacks the 'allow-same-origin' header and
      // therefore has a "null" origin.
      if(evt.origin === "null" && evt.source === playground_iframe.contentWindow) {
        send_user_request(evt.data);
      };
    });

  this.handle_user_data = function(json) {
    switch (json.tag) {
    case "STDOUT":
      // FIXME put this somewhere the user is likely to see it.
      log("STDOUT: " + json.payload);
      break;

    case "HTML":
      // Load a full HTML document (including <head>) into the iframe.
      playground_iframe.srcdoc = json.payload;
      break;

    case "URI":
      // Load a full HTML document (including <head>) into the iframe, from a URI.
      playground_iframe.src = json.payload;
      break;

    default:
      // Pass on user JSON data to the iframe. As the iframe is
      // sandboxed without the 'allow-same-origin' header, we don't
      // have an origin to target.
      playground_iframe.contentWindow.postMessage(json, '*');
      break;
    };
  };

  this.attach_event_handlers = function(fns) {
    compile_button.onclick = fns.compile_request;
    compile_button.disabled = false;
    compile_kill_button.onclick = fns.compile_kill_request;
    clear_playground_button.onclick = clear_playground;

    send_user_request = fns.send_user_request;

    editor.setOption("extraKeys", {
      "Ctrl-C": function(cm) { compile_button.click(); },
      "Ctrl-X `": function(cm) { compile_next_mark(); }
    });

    set_status("Attached to Poly/ML.");
  };

};

// ----------------------------------------

function ping_the_echo_server(log) {
  // var url = 'ws://html5rocks.websocket.org/echo'
  var url = 'ws://' + window.location.host + '/echo';
  log('Connecting to: ' + url);
  var connection = new WebSocket(url);
  log('Extensions: ' + connection.extensions);

  connection.onopen = function () {
    connection.send('Ping');
    log('echo: sent Ping.');
  };

  connection.onerror = function (error) {
    log('echo WebSocket Error ' + error.message);
  };

  connection.onmessage = function (e) {
    log('echo server: ' + e.data);
    connection.close(); // FIXME generates an error under Chrome: server shouldn't respond?
  };
};

// ----------------------------------------
// Top level.
// FIXME try to isolate the CodeMirror dependencies.

function dostuff() {
  /* Configure CodeMirror */
  var editor;

  /* FIXME cheesy persistence of the editor contents across reloads.
     Probably want to sync it more often than just on page unload.
     Bind it to the save action in CodeMirror.
     FIXME investigate CodeMirror doc.isClean
     FIXME add some notification that the document is clean or dirty.
     FIXME also need to push the data back to the server, but only sometimes (not on page unload).
   */
  function save() {
    localStorage.setItem("code", editor.getValue());
    log (">> saved");
  }

  CodeMirror.commands.save = save;

  window.onbeforeunload = function() {
    save();
  };

  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    lineWrapping: false,
    gutters: ["CodeMirror-linenumbers"],
    highlightSelectionMatches: {showToken: /\w/, annotateScrollbar: true},
    showTrailingSpace: true,
    showCursorWhenSelecting: true,
    matchBrackets: true,
    keyMap: "emacs",
    mode: 'text/x-sml', // FIXME not always
    indentUnit: 2,
    tabSize: 2
  });

  // FIXME passing code in the init object doesn't appear to work.
  var code = localStorage.getItem("code");
  if(code) {
    editor.setValue(code)
  };

  // Add a permanent panel to the bottom.
  // FIXME overkill if there's only one.
  // FIXME closing it is permanent presently. Perhaps clear instead?
  var numPanels = 0;
  var panels = {};

  function makePanel(where) {
    var node = document.createElement("div");
    var id = ++numPanels;
    var widget, close, label;

    node.id = "panel-" + id;
    node.className = "panel " + where;
    close = node.appendChild(document.createElement("a"));
    close.setAttribute("title", "Remove me!");
    close.setAttribute("class", "remove-panel");
    close.textContent = "✖";
    CodeMirror.on(close, "click", function() {
      panels[node.id].clear();
    });
    label = node.appendChild(document.createElement("span"));
    label.textContent = "";
    return {node: node, label: label};
  }

  function addPanel(where) {
    var node_label = makePanel(where);
    var node = node_label.node
    var label = node_label.label;

    var panel = editor.addPanel(node, {position: where});
    panels[node.id] = panel;

    // FIXME hacky: return a fn that sets the textContent on the label (span). What's kosher?
    // FIXME probably want to install arbitrary formatted stuff, not just a string.
    return function(str) {
      label.textContent = str;
      panel.changed();
    };
  };

  var set_status = /*function(x) {console.log(x)}; // */ addPanel("bottom");

  // Fire up the Poly/ML IDE proper.
  var ui = new UI(log, editor, set_status);
  var polyml_ide = new Polyml_ide(log, ui);
};
