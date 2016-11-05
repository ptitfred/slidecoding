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

function newConsole(endpoint, element) {
  var jqconsole = element.jqconsole("", "> ");
  var writeText = function(text) {
    jqconsole.Write(text, 'jqconsole-output');
    startPrompt();
  };
  var writeError = function(text) {
    jqconsole.Write(text, 'jqconsole-error');
    startPrompt();
  }

  var connect = function () {
    var ws = new WebSocket(url(endpoint));
    ws.onmessage = function(event) {
      writeText(event.data);
    };
    ws.onerror = function(event) {
      writeError("Connection error\n");
    };
    ws.onopen = function(event) {
      writeText("Ready\n");
    };
    return ws;
  }
  var ws = connect();

  var startPrompt = function () {
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

  addFullscreenHint(element);

  startPrompt();
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
      endpoint: '//echo.websocket.org/'
    }
  });

  $d.bind('deck.beforeInit', function() {
    $.each($[deck]('getSlides'), function(i, $slide) {
      if ($slide.hasClass('repl')) {
        addReplToSlide($, deck, $slide);
      }
    });
  });

  /*
    jQuery.deck('toggleReplPosition')

    Toggles REPL position (right column first).
  */
  $[deck]('extend', 'toggleReplPosition', function() {
    $('.console-column').css('order', toggleOrder);
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
