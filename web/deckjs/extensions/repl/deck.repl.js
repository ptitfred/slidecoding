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

function newConsole(element) {
  var jqconsole = element.jqconsole("", "> ");
  var writeText = function(text) {
    jqconsole.Write(text, 'jqconsole-output');
    startPrompt();
  };

  var startPrompt = function () {
    jqconsole.RegisterMatching('(', ')', 'brackets');

    // Start the prompt with history enabled.
    jqconsole.Prompt(true, function (input) {
      if (input !== '') {
        writeText(input);
      }
    });
  };

  writeText('(Echo only for demo.)');
  startPrompt();
};

(function($, deck, window, undefined) {
  $.extend(true, $[deck].defaults, {
    classes: {
      repl: 'deck-repl'
    }
  });

  $(document).bind('deck.beforeInit', function() {
    $.each($[deck]('getSlides'), function(i, $slide) {
      if ($slide.hasClass('repl')) {
        addReplToSlide($, $slide);
      }
    });
  });
})(jQuery, 'deck', this);
