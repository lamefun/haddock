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
        .text(title)
        .click(function() {
          setActiveStyleSheet(title);
          Cookies.set("haddock-style", title);
          $("#style-menu").hide();
        }));
  });

  if (buttons.length > 1) {
    var holder = $("<div>", {class: "dropdown-holder"});
    var menu = $("<ul>", {class: "dropdown-menu"}).append(buttons).hide();
    var menuItem =
      $("<a>", {href: "#"})
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

function toggleCollapsible(id, initial) {
  var collapser = document.getElementById("collapser." + id);
  if (collapser) {
    var target = document.getElementById("collapser-target." + id);

    var collapsed = $(collapser).hasClass("collapsed");

    $(collapser).toggleClass("collapsed");

    if (initial) {
      $(target).toggle();
    } else {
      $(target).fadeToggle(100);
    }
  }

  if (Object.prototype.hasOwnProperty.call(collapsers, id)) {
    delete collapsers[id];
  } else {
    collapsers[id] = null;
  }

  Cookies.set("haddock-collapsed", Object.keys(collapsers).join("+"));
}

function initCollapsers() {
  var cookie = Cookies.get("haddock-collapsed");
  if (cookie) {
    cookie.split("+").forEach(function(id) { toggleCollapsible(id, true); });
  }
}

//------------------------------------------------------------------------------
// Sidebar
//------------------------------------------------------------------------------


function initSidebarScrolling() {
  $(".sidebar-tab-content").addClass("nano-content");
  $(".sidebar-tab").addClass("nano").nanoScroller({
    preventPageScrolling: true
  });


    /*
  var $docContents = $("#sidebar-doc-contents");
  var $pageContents = $("#sidebar-page-contents");

  $docContents.nanoScroller();
  $pageContents.nanoScroller();


  $(window).resize(function() {
    docContentsJsp.reinitialise();
    pageContentsJsp.reinitialise();
  });

  var cookie = Cookies.getJSON("haddock-scroll");
  if (cookie) {
    if (typeof cookie.docContentsX == "number") {
      docContentsJsp.scrollToPercentX(cookie.docContentsX);
    }
    if (typeof cookie.docContentsY == "number") {
      docContentsJsp.scrollToPercentY(cookie.docContentsY);
    }
  }

  $(window).unload(function() {
    alert('Foo');
    Cookies.set("haddock-scroll", {
      docContentsX: docContentsJsp.getPercentScrolledX(),
      docContentsY: docContentsJsp.getPercentScrolledY()
    });
  });
  */
}

function initSidebarNav() {
  $("<div>", {id: "sidebar-nav"}).insertAfter("#sidebar-header");
}


function initSidebarSearch() {
  var SEARCH_PLACEHOLDER = "Search (press '/' to focus)";

  var $searchEntry =
    $("<input>", {id: "sidebar-search-entry",
                  type: "text",
                  class: "placeholder",
                  value: SEARCH_PLACEHOLDER})
      .focus(function() {
        if ($(this).hasClass("placeholder")) {
          $(this).removeClass("placeholder").val("");
        }
      })
      .blur(function() {
        if ($(this).val().match(/^\s*$/)) {
          $(this).addClass("placeholder").val(SEARCH_PLACEHOLDER);
        }
      });

  var $searchButton =
    $("<button>", {id: "sidebar-search-button"}).append(
      $("<span>", {class: "label"}).text("Search"));

  var $searchBar =
    $("<div>", {id: "sidebar-search-bar"}).append(
      $("<div>", {id: "sidebar-search-panel"}).append(
        $("<div>", {id: "sidebar-search-entry-wrapper"}).append(
          $searchEntry
        ),
      $searchButton)
    );

  Mousetrap.bind("/", function() {
    $searchEntry.focus();
  });

  $("#sidebar-nav").append($searchBar);
}

function initSidebarTabs() {
  var $tabBar = $("<div>", {id: "sidebar-tab-bar"});

  var possibleTabs = [
    {name: "Pages",    id: "sidebar-pages-tab"},
    {name: "Contents", id: "sidebar-contents-tab"}
  ];

  var activateLastTab;

  possibleTabs.forEach(function(tab) {
    var $tab = $(document.getElementById(tab.id));
    if ($tab.length == 0) {
      return;
    }

    var $button =
      $("<button>", {id: tab.id + "-button", class: "sidebar-tab-button"})
        .append($("<span>", {class: "button-label"}).text(tab.name));

    var activate = function() {
      if (!$button.hasClass("selected")) {
        $(".sidebar-tab-button").removeClass("selected");
        $(".sidebar-tab").hide();
        $button.addClass("selected");
        $tab.show();
      }
    };

    $button.click(activate);
    $tabBar.append($button);

    activateLastTab = activate;
  });

  activateLastTab();

  $("#sidebar-nav").append($tabBar);
}

function initSidebar() {
  initSidebarScrolling();
  initSidebarNav();
  initSidebarSearch();
  initSidebarTabs();
}

//------------------------------------------------------------------------------
// Smooth scrolling
//------------------------------------------------------------------------------

function initSmoothAnchorScrolling() {
  $("a").click(function() {
    var hash = this.hash;
    var target = null;
    var targetDocument = document;
    var targetWindow = window;

    if (this.target == "_parent") {
      targetDocument = parent.document;
      targetWindow = parent.window;
    }

    if (hash.length > 0 && hash[0] == '#') {
      target = targetDocument.getElementById(hash.substring(1));
    }

    if (target) {
      $(targetDocument).find("html, body").animate({
        scrollTop: $(target).offset().top
      }, 250, function () {
        targetWindow.location.hash = hash;
      });
      return false;
    } else {
      return true;
    }
  });
}

//------------------------------------------------------------------------------
// Page initialization
//------------------------------------------------------------------------------

function initPage() {
  $("html").addClass("has-javascript");
  initStyleMenu();
  initStyle();
  initCollapsers();
  initSmoothAnchorScrolling();
  initSidebar();
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
