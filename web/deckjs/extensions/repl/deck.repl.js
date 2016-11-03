function content($slide) {
  return $slide.children().first().nextAll();
}

function addReplToSlide($, $slide) {
  content($slide).wrapAll('<div class="slide-column"></div>');
  var replHtmlId = "console-" + $slide[0].id;
  $('<div/>', { id: replHtmlId, class: 'slide-column console' })
    .appendTo($slide);
  $('<script></script>')
    .append("$(function () { newConsole($('#" + replHtmlId + "')); });")
    .appendTo($slide);
  content($slide).wrapAll('<div class="slide-columns"></div>');
}

function protocol() {
  switch (location.protocol) {
    case 'https:': return 'wss:';
    default:       return 'ws:';
  }
}

function newConsole(element) {
  var jqconsole = element.jqconsole("", "> ");
  var writeText = function(text) {
    jqconsole.Write(text, 'jqconsole-output');
    startPrompt();
  };
  var writeError = function(text) {
    jqconsole.Write(text, 'jqconsole-error');
    startPrompt();
  }

  var url = protocol() + '//echo.websocket.org/';
  var connect = function () {
    var ws = new WebSocket(url);
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

  startPrompt();
};

function toggleOrder(i, order) {
  switch (order) {
    case '0': return '1';
    default:  return '0';
  }
}

(function($, deck, window, undefined) {
  var $d = $(document);

  /*
    Extends defaults/options.

    options.keys.repl
    Key to toggle REPL position between left and right (right by default).
  */
  $.extend(true, $[deck].defaults, {
    classes: {
      repl: 'deck-repl'
    },
    keys: {
      repl: 84 // t
    }
  });

  $d.bind('deck.beforeInit', function() {
    $.each($[deck]('getSlides'), function(i, $slide) {
      if ($slide.hasClass('repl')) {
        addReplToSlide($, $slide);
      }
    });
  });

  /*
    jQuery.deck('toggleReplPosition')

    Toggles REPL position (right column first).
  */
  $[deck]('extend', 'toggleReplPosition', function() {
    $('.console').css('order', toggleOrder);
  });

  $d.bind('deck.init', function() {
    var opts = $[deck]('getOptions');
    // Bind key events
    $d.unbind('keydown.deckrepl').bind('keydown.deckrepl', function(e) {
      if (e.which === opts.keys.repl || $.inArray(e.which, opts.keys.repl) > -1) {
        $[deck]('toggleReplPosition');
        e.preventDefault();
      }
    });
  });
})(jQuery, 'deck', this);
