//------------------------------------------------------------------------------

var haddock = (function() {

//------------------------------------------------------------------------------
// Style menu
//------------------------------------------------------------------------------

function getStyleLinks() {
  return $("link[rel=\"stylesheet\"], link[rel=\"alternate stylesheet\"]");
}

function getStyles() {
  return getStyleLinks()
    .map(function(i, link) { return $(link).attr("title"); })
    .get();
}

function initStyleMenu() {
  var styles = getStyles();

  var buttons = styles.map(function(title) {
    return $("<li>").append(
      $("<a>", {href: "#"})
        .click(function() {
            setActiveStyleSheet(title);
            Cookies.set("haddock-style", title);
            $("#style-menu").hide();
          })
        .text(title));
  });

  if (buttons.length > 1) {
    var holder = $("<div>", {id: "style-menu-holder"});
    var menu = $("<ul>", {id: "style-menu"}).append(buttons).hide();
    var menuItem =
      $("<a>", {class: "dropdown", href: "#"})
        .click(function() { menu.slideToggle(200); })
        .text("Style");
    holder.append(menuItem, menu);
    $("#page-menu").append($("<li>").append(holder));
  }
}

function setActiveStyleSheet(title) {
  var targetLink = null;
  getStyleLinks().each(function(i, link) {
    link.disabled = true;
    if (link.title == title) {
      targetLink = link;
    }
  });
  targetLink.disabled = false;
}

//------------------------------------------------------------------------------
// Style
//------------------------------------------------------------------------------

function initStyle() {
  var title = Cookies.get("haddock-style");
  if (title) {
    setActiveStyleSheet(title);
  }
}

//------------------------------------------------------------------------------
// Collapsers
//------------------------------------------------------------------------------

var collapsers = {};

function toggleCollapsible(id) {
  var collapser = document.getElementById("collapser." + id);
  var target = document.getElementById("collapser-target." + id);

  var collapsed = $(collapser).hasClass("collapsed");

  $(collapser).toggleClass("collapsed");
  $(target).toggleClass("collapsed");

  if (collapsed) {
    collapsers[id] = null;
  } else {
    delete collapsers[id];
  }

  Cookies.set("haddock-collapsed", Object.keys(collapsers).join("+"));
}

function initCollapsers() {
  var cookie = Cookies.get("haddock-collapsed");
  if (cookie) {
    cookie.split("+").forEach(toggleCollapsible);
  }
}

//------------------------------------------------------------------------------
// Page initialization
//------------------------------------------------------------------------------

function initPage() {
  $("body").addClass("has-javascript");
  initStyleMenu();
  initStyle();
  initCollapsers();
}

//------------------------------------------------------------------------------
// API exports
//------------------------------------------------------------------------------

return {
  // Private
  _initPage: initPage,
  _toggleCollapsible: toggleCollapsible
};

//------------------------------------------------------------------------------

})();

//------------------------------------------------------------------------------
