var qs = (function(a) {
  if (a == "") return {};
  var b = {};
  for (var i = 0; i < a.length; ++i) {
    var p=a[i].split('=', 2);
    if (p.length == 1) {
      b[p[0]] = "";
    } else {
      b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
    }
  }
  return b;
})(window.location.search.substr(1).split('&'));

function inflateUrl(lichessUrl) {
  var gameId = lichessUrl.match(/lichess:\/\/(.*)/)[1];
  return "https://lichess.org/embed/" + gameId + "?theme=purple&bg=dark";
}

if (qs.print !== "true") {
  (function($, deck, window, undefined) {
    var inflateLink = function() {
      var $link = $(this);
      if ($link.attr('href').startsWith('lichess://')) {
        var url = inflateUrl($link.attr('href'));
        var $game = $("<iframe/>", { width: 926, height: 600, src: url });
        if ($link.text() !== "") {
          var $wrapper = $('<div/>');
          var $text = $('<p>', { html: $link.html() });
          $wrapper.append($text).append($game);
          $link.replaceWith($wrapper);
        } else {
          $link.replaceWith($game);
        }
      }
    };

    var embedLichess = function() {
      $.each($('a[href]'), inflateLink);
    };

    $(document).bind('deck.beforeInit', embedLichess);
  })(jQuery, 'deck', this);
}
