function content($slide) {
  return $slide.children().first().nextAll();
}

function addReplToSlide($, deck, $slide) {
  var endpoint = $[deck]('getOptions').repl.endpoint;
  content($slide).wrapAll('<div class="repl-slide-column repl-text-column"></div>');
  var replHtmlId = "console-" + $slide[0].id;
  $('<div/>', { id: replHtmlId, class: 'repl-slide-column repl-console-column' })
    .appendTo($slide);
  $('<script></script>')
    .append("$(function () { newConsole('" + endpoint + "', $('#" + replHtmlId + "')); });")
    .appendTo($slide);
  content($slide).wrapAll('<div class="repl-slide-columns"></div>');
}

function protocol() {
  switch (location.protocol) {
    case 'https:': return 'wss:';
    default:       return 'ws:';
  }
}

function url(endpoint) {
  return protocol() + endpoint;
}

function getContext(element) {
  return element.attr('data-repl-context') || element.parents('[data-repl-context]').attr('data-repl-context');
}

function hasContext(element) {
  var ctx = getContext(element);
  return ctx !== undefined && ctx !== "";
}

function newConsole(endpoint, element) {
  var replContext = getContext(element);
  var jqconsole = element.jqconsole("", "> ");

  var startPrompt;
  var writeText = function(text) {
    jqconsole.Write(text, 'jqconsole-output');
    startPrompt();
  };
  var writeError = function(text) {
    jqconsole.Write(text, 'jqconsole-error');
    startPrompt();
  }

  jqconsole.Disable();

  addFullscreenHint(element);

  if (endpoint) {
    var connect = function () {
      var ws = new WebSocket(url(endpoint));
      ws.onmessage = function(event) {
        jqconsole.Enable();
        writeText(event.data);
      };
      ws.onerror = function(event) {
        writeError("Connection error\n");
      };
      ws.onopen = function(event) {
        ws.send("/load " + replContext);
      };
      return ws;
    }
    var ws = connect();

    startPrompt = function () {
      jqconsole.Prompt(true, function (input) {
        if (input === '/reconnect') {
          ws = connect();
        } else if (input !== '') {
          if (ws.readyState === WebSocket.OPEN) {
            ws.send(input);
          } else {
            writeError("Not connected.");
          }
        }
      });
    };

    var setup = function() {
      jqconsole.RegisterShortcut('L', reset);
      startPrompt();
    };

    var reset = function() {
      var history = jqconsole.GetHistory();
      jqconsole.Reset();
      jqconsole.SetHistory(history);
      setup();
    };

    setup();
  } else {
    startPrompt = function() {};

    writeText("REPL offline.\n" +
              "No livecoding for you :-(");

    jqconsole.Prompt(true, function() {});
  }

};

function addFullscreenHint(element) {
  $('<div/>', { class: 'repl-fullscreen-hint', text: 'Fullscreen — Hit F to quit' }).appendTo(element);
}

function toggleOrder(i, order) {
  switch (order) {
    case '0': return '1';
    default:  return '0';
  }
}

function isKey(e, keyValue) {
  return e.which === keyValue || $.inArray(e.which, keyValue) > -1;
}

(function($, deck, window, undefined) {
  var $d = $(document);

  /*
    Extends defaults/options.

    options.keys.replPositionToggle
    Key to toggle REPL position between left and right (right by default).
    Default key is 'T'.

    options.keys.replFullscreenToggle
    Key to toggle REPL to fullscreen, hiding the other column and slide title.
    Default key is 'F'.

    options.repl.endpoint
    URL of the websocket endpoint to use for REPL without the protocol part.
  */
  $.extend(true, $[deck].defaults, {
    classes: {
      repl: 'deck-repl'
    },
    keys: {
      replPositionToggle:   84, // t
      replFullscreenToggle: 70  // f
    },
    repl: {
      endpoint: ''
    }
  });

  $d.bind('deck.beforeInit', function() {
    if ($[deck]('getOptions').repl.endpoint) {
      warnAgainstCtrlW($);
    }

    $.each($[deck]('getSlides'), function(i, $slide) {
      if ($slide.hasClass('repl') && hasContext($slide)) {
        addReplToSlide($, deck, $slide);
      }
    });
  });

  /*
    jQuery.deck('toggleReplPosition')

    Toggles REPL position (right column first).
  */
  $[deck]('extend', 'toggleReplPosition', function() {
    $('.repl-console-column').css('order', toggleOrder);
  });

  $[deck]('extend', 'toggleReplFullscreen', function() {
    $('.deck-current .repl-slide-columns').siblings().toggle();
    $('.deck-current .repl-text-column').toggle();
    $('.deck-current .repl-console-column').toggleClass('repl-console-column-fullscreen');
  });

  $d.bind('deck.init', function() {
    var opts = $[deck]('getOptions');
    // Bind key events
    $d.unbind('keydown.deckrepl').bind('keydown.deckrepl', function(e) {
      if (isKey(e, opts.keys.replPositionToggle)) {
        $[deck]('toggleReplPosition');
        e.preventDefault();
      }
      if (isKey(e, opts.keys.replFullscreenToggle)) {
        $[deck]('toggleReplFullscreen');
        e.preventDefault();
      }
    });
  });
})(jQuery, 'deck', this);

function warnAgainstCtrlW($) {
  $(window).on('beforeunload', function(e) {
    return 'Bad habit of deleting words with Ctrl-W? ESC to stay here.';
  });

}
